-module(function_node_test).
-include_lib("eunit/include/eunit.hrl").

-define(NODE_PID, node_pid_function).

-define(NODE_RESULT_SERVER_NAME, <<"complete_nodes_node_pid_function_z">>).

function_node_with_valid_signature_test() ->
    logger:set_primary_config(level, debug),  % 设置日志级别

    %% pg is required for the catch nodes
    pg:start_link(),
    ered_config_store:start(),
    ered_msgtracer_manager:start_link(),
    ered_erlmodule_exchange:start_link(),

    Code = <<"ok">>,
    Signature = crypto:mac(hmac, sha256, os:getenv("FUNCTION_NODE_SECRET"), Code),
    NodeDef = get_function_node_def(Code, Signature),
    {ok, Pid} = ered_node_function:start(NodeDef, self()),
    node_result_server:start_link(?NODE_RESULT_SERVER_NAME, self()),
    Message = get_payload(<<"Hello">>),
    gen_server:cast(Pid, {incoming, Message}),

    receive
        {completed_msg, _, _} ->
            ?assert(true)
    after 5000 ->
        ?assert(false)
    end.

get_payload(Payload) ->
    #{<<"payload">> => Payload, '_ws' => ?NODE_PID}.


get_function_node_def(Code, Signature) ->
    #{<<"type">> => <<"function">>,
        <<"func">> => Code,
        <<"signature">> => Signature,
        '_node_pid_' => ?NODE_PID,
        '_mc_incoming' => 0,
        <<"wires">> => [],
        <<"z">> => "z"}.
