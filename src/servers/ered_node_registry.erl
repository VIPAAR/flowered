-module(ered_node_registry).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, register_node/2, node_type_to_module/1]).

start() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid}.

register_node(Name, Mod) ->
    gen_server:call(?MODULE, {register_node, Name, Mod}).

node_type_to_module(Name) ->
    gen_server:call(?MODULE, {node_type_to_module, Name}).

%%
%%
init([]) ->
    {ok, #{}}.

handle_call({register_node, Name, Mod}, _From, State) ->
    UpdatedState = maps:put(State, Name, Mod),
    {reply, ok, UpdatedState};

handle_call({node_type_to_module, Name}, _From, State) ->
    case maps:find(Name, State) of
        {ok, Mod} ->
            {reply, {ok, Mod}, State};
        error ->
            {reply, reply, State}
    end.

%%
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

