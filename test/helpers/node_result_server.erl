-module(node_result_server).

-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

start_link(Ws_Name, Pid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Ws_Name, Pid], []).

init([Ws_Name, Pid]) ->
    pg:join(Ws_Name, self()),
    {ok, #{ws_name => Ws_Name, pid => Pid}}.

handle_cast(Msg, State) ->
    {noreply, send_message(Msg, State)}.

handle_info(Info, State) ->
    {noreply, send_message(Info, State)}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

send_message(Msg, State) ->
    Pid = maps:get(pid, State),
    Pid ! Msg,
    State.
