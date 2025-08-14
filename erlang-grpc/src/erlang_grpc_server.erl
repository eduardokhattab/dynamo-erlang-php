-module(erlang_grpc_server).
-behaviour(gen_server).

-include("../_build/default/lib/erlang_grpc/generated/key_value_pb.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, db}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(50051, [
        {active, false},
        {reuseaddr, true},
        {packet, 0}
    ]),
    io:format("Server gRPC (Protobuf/gpb) running on port 50051~n"),
    Db = ets:new(kv_db, [set, public]),
    spawn(fun() -> accept_loop(ListenSocket, Db) end),
    {ok, #state{socket = ListenSocket, db = Db}}.

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

accept_loop(ListenSocket, Db) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Client connected: ~p~n", [Socket]),
            gen_tcp:controlling_process(Socket, self()),
            spawn(fun() -> handle_client(Socket, Db) end),
            accept_loop(ListenSocket, Db);
        {error, closed} ->
            ok
    end.

handle_client(Socket, Db) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            process_request(Data, Socket, Db),
            handle_client(Socket, Db);
        {error, closed} ->
            io:format("Client disconnected.~n"),
            gen_tcp:close(Socket),
            ok
    end.

process_request(BinaryList, Socket, Db) ->
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
                        ets:insert(Db, {Key, Value}),
                        
                        Resp = #'set_response'{error = ok},
                        EnvResp = #'req_envelope'{type = set_response_t, set_resp = Resp},
                        send_response(EnvResp, Socket);

                    #'req_envelope'{type = get_request_t, get_req = Req} ->
                        io:format("Request GET received.~n"),
                        Key = Req#'get_request'.key,

                        io:format("GET key=~s~n", [Key]),
                        case ets:lookup(Db, Key) of
                            [{_Key, Value}] ->
                                Data = #'data'{key = Key, value = Value},
                                Resp = #'get_response'{error = ok, req = Data},
                                EnvResp = #'req_envelope'{type = get_response_t, get_resp = Resp},
                                send_response(EnvResp, Socket);
                            [] ->
                                Resp = #'get_response'{error = not_found},
                                EnvResp = #'req_envelope'{type = get_response_t, get_resp = Resp},
                                send_response(EnvResp, Socket)
                        end;
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


send_response(Message, Socket) ->
    Binary = key_value_pb:encode_msg(Message),
    gen_tcp:send(Socket, Binary).
