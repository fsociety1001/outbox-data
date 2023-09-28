-module(myApp).
-behaviour(sup).
-export([start/0, start/0, process1/0, process2/0]).
-export([init/1]).

start() ->
    {ok, _} = sup:start_link({local, ?MODULE}, ?MODULE, []),
    start().

start() ->
    sup:start_child(?MODULE, []),
    sup:start_child(?MODULE, []),
    sup:start_child(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.

process1() ->
    receive
        {Sender, Mensagem} ->
            io:format("process 1 recebeu uma mensagem: ~p~n", [Mensagem]),
            Sender ! "Olá, process 1"
    end.

process2() ->
    receive
