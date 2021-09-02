-module(pqueue_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

order_test() ->
  Q = pqueue:new(),
  pqueue:in({1, 1}, 1, Q),
  pqueue:in({0, 1}, 0, Q),
  pqueue:in({1, 2}, 1, Q),
  pqueue:in({1, 3}, 1, Q),
  pqueue:in({0, 2}, 0, Q),
  pqueue:in({0, 3}, 0, Q),
  ?assertMatch( [{0,1}, {0,2}, {1, 1}, {0, 3}, {1, 2}, {1, 3}]
              , to_list(Q)
              ).

make_graphs_test() ->
  Q1 = pqueue:new(),
  [pqueue:in(Prio, Prio, Q1) || N <- lists:seq(1, 10000), Prio <- [1, 2, 3]],
  run("straight", Q1, [1, 2, 3]),
  pqueue:close(Q1),

  Q2 = pqueue:new(),
  [pqueue:in(Prio, Prio, Q2) || N <- lists:seq(1, 10000), Prio <- [3, 2, 1]],
  run("reverse", Q2, [1, 2, 3]),
  pqueue:close(Q2).


run(Name, Q, Prios) ->
  {ok, FD} = file:open("/tmp/" ++ Name ++ ".csv", [write]),
  consume(FD, #{}, Q, Prios),
  file:close(FD).

consume(FD, Acc0, Q, Prios) ->
  case pqueue:out(Q) of
    {value, Val} ->
      Acc = Acc0#{Val => maps:get(Val, Acc0, 0) + 1},
      Line = [integer_to_list(maps:get(I, Acc, 0)) || I <- Prios],
      io:format(FD, "~s~n", [string:join(Line, ",")]),
      consume(FD, Acc, Q, Prios);
    empty ->
      ok
  end.

to_list(Q) ->
  case pqueue:out(Q) of
    {value, V} ->
      [V|to_list(Q)];
    empty ->
      []
  end.
