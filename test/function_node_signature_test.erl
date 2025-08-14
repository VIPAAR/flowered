-module(function_node_signature_test).
-include_lib("eunit/include/eunit.hrl").

-define(WS_NAME, node_pid_function).

valid_signature_test() ->
    NodeDef = setup(),
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
    NodeDef = setup(),
    % change the function code after set the signature
    NewNodeDef = maps:put(<<"func">>, <<"error">>, NodeDef),
    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NewNodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {error, Msg} ->
            ?assertEqual(<<"signature mismatch">>, Msg)
    after 5000 ->
        ?assert(false)
    end.

missing_secret_test() ->
    NodeDef = setup(),
    config_utils:remove_config(function_node_secret),
    Message = build_payload(<<"Hello">>, ?WS_NAME),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {error, Msg} ->
            ?assertEqual(<<"no signature secret">>, Msg)
    after 5000 ->
        ?assert(false)
    end.

missing_signature_setting_test() ->
    NodeDef = setup(),
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

setup() ->
    config_utils:load_config(),
    pg:start_link(),
    ered_config_store:start(),
    ered_msgtracer_manager:start_link(),
    ered_erlmodule_exchange:start_link(),
    Code = <<"ok">>,
    Secret = config_utils:get_config(function_node_secret),
    Signature = crypto:mac(hmac, sha256, Secret, Code),
    prepare(Code, Signature).

prepare(Code, Signature) ->
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
