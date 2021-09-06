-module(prop_ipqueue).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

prop_size() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            ipqueue_srv:start_link(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            ipqueue_srv:stop(),
            ?WHENFAIL(io:format(standard_error, "History: ~p\nState: ~p\nResult: ~p\n",
                                [History, State, Result]),
                      aggregate(command_names(Cmds), Result =:= ok))
          end).

-record(s,
        { queue :: list()
        }).

initial_state() ->
    #s{ queue = []
      }.

command(_State) ->
    oneof([ {call, ipqueue_srv, in, [0, term()]}
          , {call, ipqueue_srv, out, []}
          , {call, ipqueue_srv, size, []}
          ]).

precondition(#s{}, {call, _Mod, _Fun, _Args}) ->
    true.

postcondition(_State, {call, ipqueue_srv, in, [_, _]}, Res) ->
  Res == ok;
postcondition(#s{queue = Q}, {call, ipqueue_srv, out, []}, Res) ->
  case {Q, Res} of
    {[Hd|_], {value, Hd}} -> true;
    {[], empty}           -> true
  end;
postcondition(#s{queue = Q}, {call, ipqueue_srv, size, []}, Res) ->
  length(Q) =:= Res.

next_state(State = #s{queue = Q}, _Res, {call, ipqueue_srv, in, [0, Term]}) ->
  State#s{queue = Q ++ [Term]};
next_state(State = #s{queue = Q}, _Res, {call, ipqueue_srv, out, []}) ->
  case Q of
    []     -> State;
    [_|Tl] -> State#s{queue = Tl}
  end;
next_state(State, Res, {call, ipqueue_srv, size, []}) ->
  State.
