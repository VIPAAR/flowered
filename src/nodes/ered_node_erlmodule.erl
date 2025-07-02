-module(ered_node_erlmodule).

-behaviour(ered_node).

-include("ered_nodes.hrl").

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

-export([install/2]).

%%
%% Erlang-Red Erlang module node for defining Erlang modules.
%%
%% Code inject modules.
%%
%% This does not nothing except for install code into the BEAM.
%%
%% The install process isn't managed by the node, instead the startup
%% procedure does the installation. That is because there has to be an
%% order maintained in how things happen. Another node wanting to use this
%% code must have that code already installed.
%%

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    post_exception_or_debug/3
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({stop, _WsName}, #{<<"id">> := NodeId} = NodeDef) ->
    ered_erlmodule_exchange:remove_module_for_nodeid(NodeId),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
install(
  #{
    <<"module_name">> := ModBinaryName,
    <<"code">> := ModuleCode,
    <<"id">> := NodeId
  } = NodeDef,
  WsName
) ->
    ModuleName = binary_to_atom(ModBinaryName),

    FileName = binary_to_list(
        list_to_binary(
            io_lib:format("/tmp/~s.erl", [ModuleName])
        )
    ),

    file:write_file(FileName, ModuleCode),

    case compile:file(FileName, [binary, return_errors, return_warnings]) of
        {ok, ModuleName, Binary, _} ->
            node_status(WsName, NodeDef, "installed", "green", "dot"),
            spawn(fun() -> clear_status_after_one_sec(WsName, NodeDef) end),
            {module, ModuleName} = code:load_binary(ModuleName, [], Binary),
            ered_erlmodule_exchange:add_module(NodeId, ModuleName);
        {error, ErrorList, WarnList} ->
            Msg = ?PUT_WS(#{
                error => compiler_list_to_json_list(ErrorList),
                warning => compiler_list_to_json_list(WarnList)
            }),
            node_status(WsName, NodeDef, "compile failed", "red", "dot"),
            post_exception_or_debug(NodeDef, Msg, <<"compile failed">>)
    end,

    file:delete(FileName).

%%
%% ErrorLists and WarnLists aren't JSON compatible therefore they need to be
%% massaged into form.
%% See https://www.erlang.org/doc/apps/compiler/compile.html for details.
compiler_list_to_json_list([]) ->
    [];
compiler_list_to_json_list([{_Filename, [ErrorInfo | Lst]}]) ->
    [errorinfo_tuple_to_list(ErrorInfo) | errorinfo_list(Lst)];
compiler_list_to_json_list([{_Filename, [ErrorInfo]} | Lst]) ->
    [errorinfo_tuple_to_list(ErrorInfo) | compiler_list_to_json_list(Lst)].

errorinfo_list([]) ->
    [];
errorinfo_list([ErrorInfo]) ->
    [errorinfo_tuple_to_list(ErrorInfo)];
errorinfo_list([ErrorInfo | Lst]) ->
    [errorinfo_tuple_to_list(ErrorInfo) | errorinfo_list(Lst)].

errorinfo_tuple_to_list({{Line, Char}, Module, Desc}) ->
    [
        list_to_binary(io_lib:format("Line: ~p, Char: ~p", [Line, Char])),
        Module,
        list_to_binary(io_lib:format("~p", [Desc]))
    ].

%%
%%
clear_status_after_one_sec(WsName, NodeId) ->
    timer:sleep(1000),
    node_status_clear(WsName, NodeId).
