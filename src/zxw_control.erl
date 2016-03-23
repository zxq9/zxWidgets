%%% @doc
%%% This module implements a very simple data management service process to
%%% persist configuration data necessary for some zx_widgets functions to
%%% work. It also implements a very simple key-value store interface which
%%% allows configuration values to be adjusted after startup. This can be used
%%% to adjust themed icon directory data or translations without requiring a
%%% system restart.
%%%
%%% To initialize zxw_control add it to a supervision tree as a permanent,
%%% process, calling start_link/1 with a list of the following form of as the
%%% configuration argument:
%%%
%%% `Conf :: [{Key :: term(), Value :: term()}]'
%%%
%%% Required arguments are currently limited to `[{icon_dir, DirectoryPath}]`,
%%% where DirectoryPath can be relative or absolute, but must be resolvable
%%% from the executing node's perspective. How this is determined is up to
%%% the calling code.
%%%
%%% (Experience has shown that creation of an absolute path at execution time,
%%% and dynamic update via `zxw_control:set_conf(icon_dir, NewPath)' is the most
%%% painless over the long-term, though perhaps a bit of extra work up front in
%%% the supervisor code.)
%%%
%%% Note that zxw_control registers itself as a <em>local</em> process only;
%%% the intended use is for wxErlang applications running locally on a single
%%% node. If using as part of a GUI frontend for a larger distributed system,
%%% zxw_control will only need to be initialized on any node that is serving a
%%% GUI interface that uses zxw widget functions.

-module(zxw_control).
-author("Craig Everett <zxq9@zxq9.com>").
-behavior(gen_server).


%% API
-export([start_link/1, set_conf/2, get_conf/1, get_conf/2]).


%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%% Types

%% Note: This may need to revert back to a dictionary if support =< R17 is
%%       required again. There is effectively no difference in performance.
-type state() :: map().


%%% Interface

-spec set_conf(Attribute, Value) -> ok
    when Attribute :: term(),
         Value     :: term().
set_conf(Attribute, Value) ->
    gen_server:cast(?MODULE, {set_conf, Attribute, Value}).


-spec get_conf(Attribute) -> {ok, Value} | {error, undefined}
    when Attribute :: term(),
         Value     :: term().
get_conf(Attribute) ->
    gen_server:call(?MODULE, {get_conf, Attribute}).


-spec get_conf(Attribute, Default) -> {ok, Value} | {ok, Default}
    when Attribute :: term(),
         Default   :: term(),
         Value     :: term().
get_conf(Attribute, Default) ->
    gen_server:call(?MODULE, {get_conf, Attribute, Default}).


%%% Startup

-spec start_link(Conf :: [{term(), term()}]) -> {ok, pid()}.
start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Conf, []).


-spec init(Conf :: [{term(), term()}]) -> {ok, state()}.
init(Conf) ->
    State = maps:from_list(Conf),
    {ok, State}.


%%% gen_server behavior

-spec handle_call(term(), {pid(), term()}, state) -> {reply, Reply, state()}
    when Reply :: {ok, term()} | {error, undefined}.
handle_call({get_conf, Attribute}, _, State) ->
    Result = case maps:get(Attribute, State) of
        undefined -> {error, undefined};
        Value     -> {ok, Value}
    end,
    {reply, Result, State};
handle_call({get_conf, Attribute, Default}, _, State) ->
    Value = maps:get(Attribute, State, Default),
    {reply, {ok, Value}, State}.


-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({set_conf, Attribute, Value}, State) ->
    {noreply, maps:put(Attribute, Value, State)}.


-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_, State) -> {noreply, State}.


-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.


-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_Version, State, _Extra) -> {ok, State}.
