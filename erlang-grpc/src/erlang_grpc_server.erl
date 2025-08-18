-module(erlang_grpc_server).
-behaviour(gen_server).

-include("../_build/default/lib/erlang_grpc/generated/key_value_pb.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

-define(TABLE_NAME, <<"key_value_table">>).
-define(KMS_KEY_ID, "alias/key_value_table_kms").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(50051, [
        {active, false},
        {reuseaddr, true},
        {packet, 0}
    ]),
    io:format("Server gRPC (Protobuf/gpb) running on port 50051~n"),
    spawn(fun() -> accept_loop(ListenSocket) end),
    {ok, #state{socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Client connected: ~p~n", [Socket]),
            gen_tcp:controlling_process(Socket, self()),
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);

        {error, closed} ->
            ok
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            process_request(Data, Socket),
            handle_client(Socket);

        {error, closed} ->
            io:format("Client disconnected.~n"),
            gen_tcp:close(Socket),
            ok
    end.

process_request(BinaryList, Socket) ->
    Binary = list_to_binary(BinaryList),
    
    io:format("binary received with ~p bytes: ~p~n", [size(Binary), Binary]),

    try
        case key_value_pb:decode_msg(Binary, 'req_envelope') of
            Env when is_record(Env, req_envelope) ->
                io:format("Envelope decoded: ~p~n", [Env]),
                case Env of
                    #'req_envelope'{type = set_request_t, set_req = Req} ->
                        io:format("Request SET received.~n"),
                        Data = Req#'set_request'.req,
                        Key = Data#'data'.key,
                        Value = Data#'data'.value,

                        io:format("SET key=~s, value=~s~n", [Key, Value]),
                        put_dynamo_item(Key, Value, Socket);
                    #'req_envelope'{type = get_request_t, get_req = Req} ->
                        io:format("Request GET received.~n"),
                        Key = Req#'get_request'.key,

                        io:format("GET key=~s~n", [Key]),

                        get_dynamo_item(Key, Socket);
                    _ ->
                        io:format("Unknown message type: ~p~n", [Env]),
                        ok
                end;

            {error, Reason} ->
                io:format("Protobuf Decoding Error: ~p~n", [Reason]),
                ok;

            Other ->
                io:format("decode_msg unexpect return: ~p~n", [Other]),
                ok
        end
    catch
        _Class:Error ->
            io:format("Unexpect error when processing the request: ~p~n", [Error]),
            ok
    end.

put_dynamo_item(Key, Value, Socket) ->
    try
        io:format("Encrypting data with envelope encryption~n"),

        KeyIdBinary = list_to_binary(?KMS_KEY_ID),
        Options = [{number_of_bytes, 32}],

        case erlcloud_kms:generate_data_key(KeyIdBinary, Options) of
            {ok, DataKeyResult} ->
                PlaintextKeyBase64 = proplists:get_value(<<"Plaintext">>, DataKeyResult),
                EncryptedKey = proplists:get_value(<<"CiphertextBlob">>, DataKeyResult),

                PlaintextKey = base64:decode(PlaintextKeyBase64),

                io:format("Data Key generated - Base64: ~p bytes, Decoded: ~p bytes~n",
                         [byte_size(PlaintextKeyBase64), byte_size(PlaintextKey)]),

                IV = crypto:strong_rand_bytes(12),
                {CipherText, AuthTag} = crypto:crypto_one_time_aead(
                    aes_256_gcm, PlaintextKey, IV, Value, <<>>, true
                ),

                Payload = <<IV/binary, AuthTag/binary, CipherText/binary>>,

                io:format("Data encrypted (~p bytes -> ~p bytes)~n",
                         [byte_size(Value), byte_size(Payload)]),

                Item = [
                    {<<"key">>, Key},
                    {<<"value">>, base64:encode(Payload)},
                    {<<"data_key">>, base64:encode(EncryptedKey)}
                ],

                case erlcloud_ddb2:put_item(?TABLE_NAME, Item) of
                    {ok, _} ->
                        io:format("Encrypted Item saved on DynamoDB~n"),
                        OkResp = #'set_response'{error = ok},
                        OkEnvResp = #'req_envelope'{type = set_response_t, set_resp = OkResp},
                        send_response(OkEnvResp, Socket);
                    {error, Reason} ->
                        io:format("Errr saving on DynamoDB: ~p~n", [Reason]),
                        ErrorResp = #'set_response'{error = internal},
                        ErrorEnvResp = #'req_envelope'{type = set_response_t, set_resp = ErrorResp},
                        send_response(ErrorEnvResp, Socket)
                end;

            {error, KmsError} ->
                io:format("Error generating Data Key: ~p~n", [KmsError]),
                KmsErrorResp = #'set_response'{error = internal},
                KmsErrorEnvResp = #'req_envelope'{type = set_response_t, set_resp = KmsErrorResp},
                send_response(KmsErrorEnvResp, Socket)
        end

    catch
        Class:Error:Stacktrace ->
            io:format("Error on encrypt - Class: ~p, Error: ~p~n", [Class, Error]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            CatchResp = #'set_response'{error = internal},
            CatchEnvResp = #'req_envelope'{type = set_response_t, set_resp = CatchResp},
            send_response(CatchEnvResp, Socket)
    end.

get_dynamo_item(Key, Socket) ->
    KeyMap = [
        {<<"key">>, {s, Key}}
    ],

    try
        case erlcloud_ddb2:get_item(?TABLE_NAME, KeyMap) of
            {ok, ItemList} when is_list(ItemList), ItemList =/= [] ->
                io:format("Item found: ~p~n", [ItemList]),

                {<<"value">>, EncodedPayload} = lists:keyfind(<<"value">>, 1, ItemList),
                {<<"data_key">>, EncodedDataKey} = lists:keyfind(<<"data_key">>, 1, ItemList),

                decrypt_and_respond(Key, EncodedPayload, EncodedDataKey, Socket);

            {ok, []} ->
                io:format("Item not found~n"),
                NotFoundResp = #'get_response'{error = not_found},
                NotFoundEnvResp = #'req_envelope'{type = get_response_t, get_resp = NotFoundResp},
                send_response(NotFoundEnvResp, Socket);

            {error, Reason} ->
                io:format("Error getting from DynamoDB: ~p~n", [Reason]),
                ErrorResp = #'get_response'{error = internal},
                ErrorEnvResp = #'req_envelope'{type = get_response_t, get_resp = ErrorResp},
                send_response(ErrorEnvResp, Socket)
        end
    catch
        _Class:Error ->
            io:format("Exception in get_dynamo_item: ~p~n", [Error]),
            ExceptionResp = #'get_response'{error = internal},
            ExceptionEnvResp = #'req_envelope'{type = get_response_t, get_resp = ExceptionResp},
            send_response(ExceptionEnvResp, Socket)
    end.

send_response(Message, Socket) ->
    Binary = key_value_pb:encode_msg(Message),
    gen_tcp:send(Socket, Binary).

decrypt_and_respond(Key, EncodedPayload, EncodedDataKey, Socket) ->
    try
        io:format("Decrypting data~n"),

        Payload = base64:decode(EncodedPayload),
        EncryptedDataKey = base64:decode(EncodedDataKey),

        <<IV:12/binary, AuthTag:16/binary, CipherText/binary>> = Payload,

        case erlcloud_kms:decrypt(EncryptedDataKey) of
            {ok, DecryptResult} ->
                PlaintextKeyBase64 = proplists:get_value(<<"Plaintext">>, DecryptResult),

                PlaintextKey = base64:decode(PlaintextKeyBase64),

                io:format("Data Key decrypted - Base64: ~p bytes, Decoded: ~p bytes~n",
                         [byte_size(PlaintextKeyBase64), byte_size(PlaintextKey)]),

                OriginalValue = crypto:crypto_one_time_aead(
                    aes_256_gcm, PlaintextKey, IV, CipherText, <<>>, AuthTag, false
                ),

                io:format("Data decrypted (~p bytes)~n", [byte_size(OriginalValue)]),

                Data = #'data'{key = Key, value = OriginalValue},
                SuccessResp = #'get_response'{error = ok, req = Data},
                SuccessEnvResp = #'req_envelope'{type = get_response_t, get_resp = SuccessResp},
                send_response(SuccessEnvResp, Socket);

            {error, KmsError} ->
                io:format("Error decrypting Data Key: ~p~n", [KmsError]),
                KmsErrorResp = #'get_response'{error = internal},
                KmsErrorEnvResp = #'req_envelope'{type = get_response_t, get_resp = KmsErrorResp},
                send_response(KmsErrorEnvResp, Socket)
        end

    catch
        Class:Error:Stacktrace ->
            io:format("Error on decrypting - Class: ~p, Error: ~p~n", [Class, Error]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            ExceptionResp = #'get_response'{error = internal},
            ExceptionEnvResp = #'req_envelope'{type = get_response_t, get_resp = ExceptionResp},
            send_response(ExceptionEnvResp, Socket)
    end.
