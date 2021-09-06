-module(ipqueue_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, in/2, out/0, size/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?SERVER).

in(Prio, Term) ->
  gen_server:call(?SERVER, {in, Prio, Term}).

out() ->
  gen_server:call(?SERVER, out).

size() ->
  gen_server:call(?SERVER, size).

init([]) ->
  {ok, ipqueue:new()}.

handle_call({in, Prio, Term}, _From, Q) ->
  {reply, ipqueue:in(Term, Prio, Q), Q};
handle_call(out, _From, Q) ->
  {reply, ipqueue:out(Q), Q};
handle_call(size, _From, Q) ->
  {reply, ipqueue:len(Q), Q}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, Q) ->
  ipqueue:close(Q).
