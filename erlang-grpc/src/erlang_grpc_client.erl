-module(erlang_grpc_client).

-export([start/0, set/2, get/1]).

-include("../_build/default/lib/erlang_grpc/generated/key_value_pb.hrl").

start() ->
    case gen_tcp:connect("localhost", 50051, [
        {active, false},
        {packet, 0}
    ]) of
        {ok, Socket} ->
            io:format("Connected to server on port 50051.~n"),
            Socket;
        {error, Reason} ->
            io:format("Error connecting to the server: ~p~n", [Reason]),
            {error, Reason}
    end.

set(Key, Value) ->
    Socket = start(),
    set(Socket, Key, Value),
    gen_tcp:close(Socket).

get(Key) ->
    Socket = start(),
    Result = get(Socket, Key),
    gen_tcp:close(Socket),
    Result.

set(Socket, Key, Value) ->
    DataMsg = #'data'{key = Key, value = Value},
    SetReqMsg = #'set_request'{req = DataMsg},
    EnvReq = #'req_envelope'{type = set_request_t, set_req = SetReqMsg},
    Binary = key_value_pb:encode_msg(EnvReq),
    
    ok = gen_tcp:send(Socket, Binary),
    
    case gen_tcp:recv(Socket, 0) of
        {ok, ResponseData} ->
            ResponseBinary = case ResponseData of
                Data when is_list(Data) -> 
                    io:format("Converting list to bytes: ~p~n", [Data]),
                    list_to_binary(Data);
                Data when is_binary(Data) -> 
                    io:format("Data already in bytes: ~p~n", [Data]),
                    Data
            end,
            
            io:format("Decoding binary of ~p bytes~n", [size(ResponseBinary)]),
            
            case key_value_pb:decode_msg(ResponseBinary, 'req_envelope') of
                EnvResp when is_record(EnvResp, req_envelope) ->
                    io:format("Envelope decoded: ~p~n", [EnvResp]),
                    
                    case EnvResp of
                        #'req_envelope'{type = set_response_t, set_resp = Resp} ->
                            case Resp#'set_response'.error of
                                ok ->
                                    io:format("SET OK: {~s, ~s}~n", [Key, Value]);
                                internal ->
                                    io:format("SET ERROR: Internal Error~n");
                                Other ->
                                    io:format("SET ERROR: ~p~n", [Other])
                            end;
                        _ ->
                            io:format("Unknown response type: ~p~n", [EnvResp])
                    end;
                {error, Reason} ->
                    io:format("Decoding response error: ~p~n", [Reason]);
                Other ->
                    io:format("decode_msg unknown response: ~p~n", [Other])
            end;
        {error, Reason} ->
            io:format("Error receiving response: ~p~n", [Reason])
    end.

get(Socket, Key) ->
    GetReqMsg = #'get_request'{key = Key},
    EnvReq = #'req_envelope'{type = get_request_t, get_req = GetReqMsg},
    Binary = key_value_pb:encode_msg(EnvReq),

    ok = gen_tcp:send(Socket, Binary),

    case gen_tcp:recv(Socket, 0) of
        {ok, ResponseData} ->
            ResponseBinary = case ResponseData of
                Data when is_list(Data) -> 
                    io:format("Converting list to bytes: ~p~n", [Data]),
                    list_to_binary(Data);
                Data when is_binary(Data) -> 
                    io:format("Data already in bytes: ~p~n", [Data]),
                    Data
            end,
            
            io:format("Decoding binary of ~p bytes~n", [size(ResponseBinary)]),
            
            case key_value_pb:decode_msg(ResponseBinary, 'req_envelope') of
                EnvResp when is_record(EnvResp, req_envelope) ->
                    io:format("Envelope decoded: ~p~n", [EnvResp]),
                    
                    case EnvResp of
                        #'req_envelope'{type = get_response_t, get_resp = Resp} ->
                            case Resp#'get_response'.error of
                                ok ->
                                    DataRecord = Resp#'get_response'.req,
                                    ReturnedKey = DataRecord#'data'.key,
                                    Value = DataRecord#'data'.value,
                                    io:format("GET OK: {~s, ~s}~n", [ReturnedKey, Value]),
                                    {ok, Value};
                                not_found ->
                                    io:format("GET NOT_FOUND: Key ~s not found~n", [Key]),
                                    {error, not_found};
                                internal ->
                                    io:format("GET ERROR: Internal error~n"),
                                    {error, internal};
                                Other ->
                                    io:format("GET ERROR: ~p~n", [Other]),
                                    {error, Other}
                            end;
                        _ ->
                            io:format("Unknown response type: ~p~n", [EnvResp]),
                            {error, unexpected_response}
                    end;
                {error, Reason} ->
                    io:format("Decoding response error: ~p~n", [Reason]),
                    {error, decode_error};
                Other ->
                    io:format("decode_msg unknown response: ~p~n", [Other]),
                    {error, unexpected_decode_result}
            end;
        {error, Reason} ->
            io:format("Error receiving response: ~p~n", [Reason]),
            {error, Reason}
    end.
