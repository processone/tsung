-module(rabbit_misc).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([method_record_type/1]).
-export([amqp_error/4]).
-export([frame_error/2]).
-export([protocol_error/3, protocol_error/4, protocol_error/1]).

method_record_type(Record) ->
    element(1, Record).

amqp_error(Name, ExplanationFormat, Params, Method) ->
    Explanation = format(ExplanationFormat, Params),
    #amqp_error{name = Name, explanation = Explanation, method = Method}.

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

frame_error(MethodName, BinaryFields) ->
    protocol_error(frame_error, "cannot decode ~w", [BinaryFields], MethodName).

protocol_error(Name, ExplanationFormat, Params) ->
    protocol_error(Name, ExplanationFormat, Params, none).

protocol_error(Name, ExplanationFormat, Params, Method) ->
    protocol_error(amqp_error(Name, ExplanationFormat, Params, Method)).

protocol_error(#amqp_error{} = Error) ->
    exit(Error).

