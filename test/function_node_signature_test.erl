-module(function_node_signature_test).
-include_lib("eunit/include/eunit.hrl").

-define(NODE_PID, node_pid_function).

with_valid_signature_test() ->
    FlowId = "flowId",
    NodeId = "nodeId",
    WsName = node_pid_function,
    Secret = "a-real-secret",

    os:putenv("FUNCTION_NODE_SECRET", Secret),
    logger:set_primary_config(level, debug),
    pg:start_link(),
    ered_config_store:start(),
    ered_msgtracer_manager:start_link(),
    ered_erlmodule_exchange:start_link(),

    Code = <<"ok">>,
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    NodeDef = build_node_def(FlowId, NodeId, WsName, Code, Signature),
    node_response_server:start_link(NodeDef, self()),

    Message = build_payload(<<"Hello">>, WsName),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {success, Msg} ->
            ?assertEqual(Message, Msg)
    after 5000 ->
        ?assert(false)
    end.


build_payload(Content, WsName) ->
    #{<<"payload">> => Content, '_ws' => WsName}.

build_node_def(FlowId, NodeId, WsName, Code, Signature) ->
    #{<<"type">> => <<"function">>,
        <<"func">> => Code,
        <<"signature">> => Signature,
        '_node_pid_' => WsName,
        '_mc_incoming' => 0,
        <<"wires">> => [],
        <<"z">> => FlowId,
        <<"id">> => NodeId}.
