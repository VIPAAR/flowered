-module(ered_node_template).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%%
%% Attributes of importance:
%%
%%    "field": "payload",
%%    "fieldType": "msg",
%%    "format": "handlebars", <<---- ignored since this is for the frontend
%%    "syntax": "plain", <<---- (can be "mustache template")
%%    "output": "str",   <<---- (can be "parse the content as json")
%%    "template": "This is the payload: {{payload}} !",
%%

-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    ws_from/1,
    unsupported/3
]).

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).

-import(ered_msg_handling, [
    decode_json/1,
    map_keys_to_lists/1
]).

doit(Prop, <<"msg">>, Template, <<"plain">>, <<"str">>, Msg) ->
    Msg2 = maps:put(binary_to_atom(Prop), Template, Msg),
    {ok, Msg2};

doit(Prop, <<"msg">>, Template, <<"mustache">>, <<"str">>, Msg) ->
    MustachedRendered = bbmustache:render(Template, map_keys_to_lists(Msg)),
    Msg2 = maps:put(binary_to_atom(Prop), MustachedRendered, Msg),
    {ok, Msg2};

doit(Prop, <<"msg">>, Template, <<"plain">>, <<"json">>, Msg) ->
    Msg2 = maps:put(binary_to_atom(Prop), decode_json(Template), Msg),
    {ok, Msg2};

doit(Prop, <<"msg">>, Template, <<"mustache">>, <<"json">>, Msg) ->
    MustachedRendered = bbmustache:render(Template, map_keys_to_lists(Msg)),
    Msg2 = maps:put(binary_to_atom(Prop), decode_json(MustachedRendered), Msg),
    {ok, Msg2};

doit(_, _, _, _, _, _) ->
    unsupported.

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(field, NodeDef),
    {ok, PropType} = maps:find(fieldType, NodeDef),
    {ok, Syntax} = maps:find(syntax, NodeDef),
    {ok, Template} = maps:find(template, NodeDef),
    {ok, Output} = maps:find(output, NodeDef),

    case doit(Prop, PropType, Template, Syntax, Output, Msg) of
        {ok, Msg2} ->
            send_msg_to_connected_nodes(NodeDef, Msg2),
            {NodeDef, Msg2};
        unsupported ->
            ErrMsg = jstr("Unsupported configuration: ~p", [NodeDef]),
            unsupported(NodeDef, Msg, ErrMsg),

            %% Ok, we push the content into the msg object but warn as
            %% well. Not modifying the message and sending it on would
            %% be an error but not supporting formatting is kind of ...
            %% an error really but since I don't raise an error here, set
            %% the content in the msg.
            Msg2 = maps:put(binary_to_atom(Prop), Template, Msg),

            send_msg_to_connected_nodes(NodeDef, Msg2),
            {NodeDef, Msg}
    end.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
