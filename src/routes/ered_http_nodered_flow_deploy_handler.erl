-module(ered_http_nodered_flow_deploy_handler).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_flow_restart/2,
    handle_flow_deploy/2,
    format_error/2
]).

-import(ered_compute_engine, [
    deploy/2,
    reload/1
]).
-import(ered_nodered_comm, [
    websocket_name_from_request/1
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {
        [
            {<<"application/json">>, handle_flow_deploy},
            {<<"application/json; charset=utf-8">>, handle_flow_deploy},
            {<<"application/x-json-restart">>, handle_flow_restart}
        ],
        Req,
        State
    }.

handle_flow_restart(Req, State) ->
    WsName = websocket_name_from_request(Req),
    Resp =
        case cowboy_req:header(<<"node-red-deployment-type">>, Req) of
            <<"reload">> ->
                cowboy_req:set_resp_body(reload(WsName), Req);
            RestartType ->
                io:format("Unknow restart type: ~p~n", [RestartType]),
                cowboy_req:set_resp_body(<<"ok">>, Req)
        end,
    {true, Resp, State}.

handle_flow_deploy(Req, State) ->
    {ok, Body, Req2} = ered_http_utils:read_body(Req, <<"">>),

    WsName = websocket_name_from_request(Req),

    %% Different types of deployment do - in real life - different things
    %% here they all do the same thing. 'nodes' is just restarting those
    %% nodes that have changed, 'flows' is redeploy only flow tabs that
    %% contain modified nodes and 'full' is simply a complete new deploy.
    %% All three methods receive the entire flow, so it becomes difficult
    %% to compare what has changed to what is currently running on the
    %% server. The client should send the diff so that the server
    %% does exactly what is required.
    RespText =
        case cowboy_req:header(<<"node-red-deployment-type">>, Req) of
            <<"full">> ->
                deploy(Body, WsName);
            <<"nodes">> ->
                deploy(Body, WsName);
            <<"flows">> ->
                deploy(Body, WsName);
            DeployType ->
                io:format("Unknow deploy type: ~p~n", [DeployType])
        end,

    Resp = cowboy_req:set_resp_body(RespText, Req2),

    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
