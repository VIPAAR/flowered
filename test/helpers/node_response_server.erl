%%
%% A genserver which will be joined to completed group and exception group,
%% then forward the message to test process.
%%
-module(node_response_server).

-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

start_link(NodeDef, Pid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [NodeDef, Pid], []).

init([NodeDef, Pid]) ->
    WsName = maps:get('_node_pid_', NodeDef),
    {ok, FlowId} = maps:find(<<"z">>, NodeDef),
    pg:join(ered_message_exchange:pg_complete_group_name(NodeDef, WsName), self()),
    pg:join(ered_message_exchange:pg_exception_group_name(FlowId, WsName), self()),
    {ok, #{node_def => NodeDef, pid => Pid}}.

handle_cast({completed_msg, _, Msg}, State) ->
    send_success_message(maps:get(pid, State), Msg),
    {noreply, State};

handle_cast({exception, _SrcNode, _SrcMsg, ErrMsg}, State) ->
    send_error_message(maps:get(pid, State), ErrMsg),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, State, State}.

send_success_message(Pid, Msg) ->  Pid ! {success, Msg}.

send_error_message(Pid, Msg) ->  Pid ! {error, Msg}.
