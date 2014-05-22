-module(inject).

-export([load/1, start/1, hello/1, stop/1]).

load(Node) ->
    {ok, foo, Code} = compile:file("foo.erl", [binary]),
    rpc:call(Node, code, load_binary, [foo, "foo.erl", Code], 10000).

start(Node) ->
    rpc:call(Node, foo, start_link, [], 10000).

hello(Node) ->
    rpc:cast(Node, foo, hello, []).

stop(Node) ->
    rpc:cast(Node, foo, stop, []).
