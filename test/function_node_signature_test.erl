-module(function_node_signature_test).
-include_lib("eunit/include/eunit.hrl").

-define(WS_NAME, node_pid_function).

with_valid_signature_test() ->
    Code = <<"ok">>,
    Secret = "a-real-secret",
    os:putenv("FUNCTION_NODE_SECRET", Secret),
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    NodeDef = prepare(Code, Signature),

    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {success, Msg} ->
            ?assertEqual(Message, Msg)
    after 5000 ->
        ?assert(false)
    end.

invalid_signature_test() ->
    Secret = "a-real-secret",
    os:putenv("FUNCTION_NODE_SECRET", Secret),

    Code = <<"ok">>,
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    NodeDef = prepare(<<"error">>, Signature),

    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {error, Msg} ->
            ?assertEqual(<<"signature mismatched">>, Msg)
    after 5000 ->
        ?assert(false)
    end.

missing_secret_test() ->
    os:unsetenv("FUNCTION_NODE_SECRET"),
    Code = <<"ok">>,
    Secret = "a-real-secret",
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    NodeDef = prepare(Code, Signature),

    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {error, Msg} ->
            ?assertEqual(<<"no signature secret">>, Msg)
    after 5000 ->
        ?assert(false)
    end.

missing_signature_sertting_test() ->
    os:unsetenv("FUNCTION_NODE_SECRET"),
    Code = <<"ok">>,
    Secret = "a-real-secret",
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    NodeDef = prepare(Code, Signature),
    NewNodeDef = maps:remove(<<"signature">>, NodeDef),

    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NewNodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {error, Msg} ->
            ?assertEqual(<<"signature missed">>, Msg)
    after 5000 ->
        ?assert(false)
    end.

prepare(Code, Signature) ->
    logger:set_primary_config(level, debug),
    pg:start_link(),
    ered_config_store:start(),
    ered_msgtracer_manager:start_link(),
    ered_erlmodule_exchange:start_link(),

    FlowId = "flowId",
    NodeId = "nodeId",
    WsName = ?WS_NAME,

    NodeDef = build_node_def(FlowId, NodeId, WsName, Code, Signature),
    node_result_message_handler:start_link(NodeDef, self()),
    NodeDef.


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
