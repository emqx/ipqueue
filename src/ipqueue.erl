-module(ipqueue).

-export([ in/3
        , out/1
        , new/0
        , close/1
        ]).

-type prio() :: non_neg_integer().

-record(priority_queue,
        { tab :: ets:tid()
        }).

-define(size, {size, size}).
-define(seqno(PRIO), {seqno, PRIO}).

-opaque t() :: #priority_queue{}.

-export_type([ prio/0
             , t/0
             ]).

-spec new() -> t().
new() ->
  Tab = ets:new(ipqueue_tab, [ordered_set]),
  ets:insert(Tab, {?size, 0}),
  #priority_queue{tab = Tab}.

-spec close(t()) -> ok.
close(#priority_queue{tab = Tab}) ->
  true = ets:delete(Tab),
  ok.

-spec in(term(), prio(), t()) -> ok.
in(Val, Prio, #priority_queue{tab = Tab}) when Prio >= 0 ->
  Key = {get_next_seqno(Tab, Prio), Prio},
  true = ets:insert_new(Tab, {Key, Val}),
  ets:update_counter(Tab, ?size, {2, 1}, {?size, 0}),
  ok.

-spec out(t()) -> {value, term()} | empty.
out(#priority_queue{tab = Tab}) ->
  case ets:first(Tab) of
    Key = {SeqNo, _Prio} when is_integer(SeqNo) ->
      Val = ets:lookup_element(Tab, Key, 2),
      ets:update_counter(Tab, ?size, {2, -1}),
      ets:delete(Tab, Key),
      {value, Val};
    _ ->
      empty
  end.

%% This function generates keys that go in sequence for each
%% individual priority level, but interleave for different priority
%% levels. Keys with lower priority are more sparse, so they are
%% consumed less often in the total sequence
get_next_seqno(Tab, Prio) ->
  Delta = Prio + 1,
  Key = ?seqno(Prio),
  ets:update_counter(Tab, Key, {2, Delta}, {Key, 0}).
