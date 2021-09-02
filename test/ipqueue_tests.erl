-module(ipqueue_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

order_test() ->
  Q = ipqueue:new(),
  ipqueue:in({1, 1}, 1, Q),
  ipqueue:in({0, 1}, 0, Q),
  ipqueue:in({1, 2}, 1, Q),
  ipqueue:in({1, 3}, 1, Q),
  ipqueue:in({0, 2}, 0, Q),
  ipqueue:in({0, 3}, 0, Q),
  ?assertMatch( [{0,1}, {0,2}, {1, 1}, {0, 3}, {1, 2}, {1, 3}]
              , to_list(Q)
              ).

benchmark_test() ->
  NItems = 100000,
  NPrios = 10,
  Items = lists:seq(1, NItems),
  Data = [{Prio, I} || I <- Items, Prio <- lists:seq(1, NPrios)],
  Q = ipqueue:new(),
  {TPush, _} =
    timer:tc(
      fun() ->
          lists:foreach(fun({Prio, I}) -> ipqueue:in(I, Prio, Q) end, Data)
      end),
  io:format(standard_error, "Push: ~p us~n", [TPush/(NItems * NPrios)]),
  {TPop, _} =
    timer:tc(
      fun() ->
          just_dump(Q)
      end),
  io:format(standard_error, "Pop: ~p us~n", [TPop/(NItems * NPrios)]).

make_graphs_test() ->
  Q1 = ipqueue:new(),
  [ipqueue:in(Prio, Prio, Q1) || N <- lists:seq(1, 10000), Prio <- [1, 2, 3]],
  run("straight", Q1, [1, 2, 3]),
  ipqueue:close(Q1),

  Q2 = ipqueue:new(),
  [ipqueue:in(Prio, Prio, Q2) || N <- lists:seq(1, 10000), Prio <- [3, 2, 1]],
  run("reverse", Q2, [1, 2, 3]),
  ipqueue:close(Q2).


run(Name, Q, Prios) ->
  {ok, FD} = file:open("/tmp/" ++ Name ++ ".csv", [write]),
  consume(FD, #{}, Q, Prios),
  file:close(FD).

consume(FD, Acc0, Q, Prios) ->
  case ipqueue:out(Q) of
    {value, Val} ->
      Acc = Acc0#{Val => maps:get(Val, Acc0, 0) + 1},
      Line = [integer_to_list(maps:get(I, Acc, 0)) || I <- Prios],
      io:format(FD, "~s~n", [string:join(Line, ",")]),
      consume(FD, Acc, Q, Prios);
    empty ->
      ok
  end.

to_list(Q) ->
  case ipqueue:out(Q) of
    {value, V} ->
      [V|to_list(Q)];
    empty ->
      []
  end.

just_dump(Q) ->
  case ipqueue:out(Q) of
    {value, V} ->
      to_list(Q);
    empty ->
      []
  end.
