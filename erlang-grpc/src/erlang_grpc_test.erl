-module(erlang_grpc_test).

-export([
    create_empty_data/0,
    create_key_value/2]).
 
% This imports the record for you to use from the generated file
-include("key_value_pb.hrl").


create_empty_data() ->
    #data{}.

create_key_value(Name, Value) ->
    #data{key = Name, value = Value}.
