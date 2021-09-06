-module(prop_size).

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
        { len   :: non_neg_integer()
        }).

initial_state() ->
    #s{ len   = 0
      }.

prio() ->
  byte().

in_args() ->
  ?LET(Prio, non_neg_integer(),
       [Prio, {Prio, term()}]).

command(_State) ->
    oneof([ {call, ipqueue_srv, in, in_args()}
          , {call, ipqueue_srv, out, []}
          , {call, ipqueue_srv, size, []}
          ]).

precondition(#s{}, {call, _Mod, _Fun, _Args}) ->
    true.

postcondition(_State, {call, ipqueue_srv, in, [_, _]}, Res) ->
  Res == ok;
postcondition(#s{len = Len}, {call, ipqueue_srv, out, []}, Res) ->
  case {Res, Len} of
    {{value, Val}, Len} when Len > 0 ->
      true;
    {empty, 0} ->
      true
  end;
postcondition(#s{len = S}, {call, ipqueue_srv, size, []}, Res) ->
  S =:= Res.

next_state(State = #s{len = Len}, _Res, {call, ipqueue_srv, in, [_Prio, _Term]}) ->
  State#s{len = Len + 1};
next_state(State = #s{len = 0}, _Res, {call, ipqueue_srv, out, []}) -> %% Empty queue
  State;
next_state(State = #s{len = Len}, _Res, {call, ipqueue_srv, out, []}) ->
  State#s{len = Len - 1};
next_state(State, Res, {call, ipqueue_srv, size, []}) ->
  State.
