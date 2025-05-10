-module(ered_flow_store_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

-export([get_flow_data/0]).
-export([update_all_flows/0]).
-export([update_flow/2]).
-export([get_filename/1]).
-export([all_flow_ids/0]).

%%
%% Manage the collection of test flows in the priv/testflows directory.
%% This manages the retrieval but not storage - which is an inconsistency.
%% Storage is handled directlty in the http request.
%%
-import(ered_flows, [
    parse_flow_file/1
]).
-import(ered_nodes, [
    jstr/1
]).

start() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    %% do an initial load of all flows, after that it should be maintained
    %% by itself.
    erlang:start_timer(200, Pid, initial_load_of_flow_files),
    {ok, Pid}.

get_flow_data() ->
    gen_server:call(?MODULE, {get_store}).

update_all_flows() ->
    gen_server:call(?MODULE, {update_all}).

update_flow(FlowId, Filename) ->
    gen_server:call(?MODULE, {update_one, FlowId, Filename}).

get_filename(FlowId) when is_list(FlowId) ->
    get_filename(list_to_binary(FlowId));
get_filename(FlowId) ->
    gen_server:call(?MODULE, {filename, FlowId}).

all_flow_ids() ->
    gen_server:call(?MODULE, {all_flow_ids}).

%%
%%
init([]) ->
    {ok, #{}}.

%%
%% Specific implementation for the flow store
%%
handle_call({all_flow_ids}, _From, FlowStore) ->
    AllFlowIds = maps:keys(FlowStore),
    {reply, AllFlowIds, FlowStore};
handle_call({update_all}, _From, _FlowStore) ->
    {reply, true, compile_file_store(compile_file_list(), #{})};
handle_call({update_one, FlowId, Filename}, _From, FlowStore) ->
    FlowDetails = compile_file_store([{FlowId, Filename}], #{}),
    {reply, true, maps:merge(FlowStore, FlowDetails)};
handle_call({get_store}, _From, FlowStore) ->
    %% beacuse the path attribute exposes internal pathways, strip it
    %% off before responding - this call is used for the frontend, i.e.
    %% it's going beyond the bounds of the four walls of the application.
    List = maps:to_list(FlowStore),
    RemoveDir = fun({ok, Path}) ->
        filename:basename(Path)
    end,
    ListStriped = [
        {Key, maps:put(path, RemoveDir(maps:find(path, M)), M)}
     || {Key, M} <- List
    ],
    {reply, maps:from_list(ListStriped), FlowStore};
handle_call({filename, FlowId}, _From, FlowStore) ->
    case maps:find(FlowId, FlowStore) of
        {ok, Val} ->
            case maps:find(path, Val) of
                {ok, Path} ->
                    {reply, Path, FlowStore};
                _ ->
                    {reply, error, FlowStore}
            end;
        _ ->
            {reply, error, FlowStore}
    end;
handle_call(_Msg, _From, FlowStore) ->
    {reply, FlowStore, FlowStore}.

%%
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

%%
%%
handle_info({store_flow, FlowId, JsonText}, FlowStore) ->
    Push = fun(Key, Value, Acc) ->
        [{binary_to_atom(Key), Value} | Acc]
    end,

    {FlowMap, _, _} = json:decode(JsonText, ok, #{object_push => Push}),
    {ok, NodeAry} = maps:find(flow, FlowMap),

    DestFileName = io_lib:format(
        "~s/testflows/~s/flows.json",
        [code:priv_dir(erlang_red), FlowId]
    ),

    filelib:ensure_dir(DestFileName),
    case file:write_file(DestFileName, NodeAry) of
        ok ->
            ignore_all_went_well;
        R ->
            io:format( "FILE SAVING FAILED: ~s --> ~p~n",
                       [DestFileName, R])
    end,

    FlowDetails = compile_file_store([{binary_to_list(FlowId),
                                       lists:flatten(DestFileName)}], #{}),

    {noreply, maps:merge(FlowStore, FlowDetails)};
handle_info({timeout, _From, initial_load_of_flow_files}, _FlowStore) ->
    {noreply, compile_file_store(compile_file_list(), #{})};
handle_info(stop, FlowStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, FlowStore}.

%%
%%
code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

%%
%%
tab_name_or_filename([], FileName) ->
    FileName;
tab_name_or_filename([NodeDef | MoreNodeDefs], FileName) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            {ok, Val} = maps:find(label, NodeDef),
            Val;
        _ ->
            tab_name_or_filename(MoreNodeDefs, FileName)
    end.

compile_file_list() ->
    {ok, MP} = re:compile("([A-Z0-9]{16})/flows.json", [caseless]),

    TestFlowDir = io_lib:format("~s/testflows/", [code:priv_dir(erlang_red)]),

    FileNames = filelib:fold_files(
        TestFlowDir,
        "flows.json",
        true,
        fun(Fname, Acc) ->
            case re:run(Fname, MP) of
                {match, [{_, _}, {S, L}]} ->
                    [{string:substr(Fname, S + 1, L), Fname} | Acc];
                _ ->
                    Acc
            end
        end,
        []
    ),
    FileNames.

%% erlfmt:ignore lining stuff up
compile_file_store([], FileStore) ->
    FileStore;
compile_file_store([FileDetails | MoreFileNames], FileStore) ->
    FlowId   = element(1, FileDetails),
    FileName = element(2, FileDetails),
    Ary      = parse_flow_file(FileName),
    TestName = tab_name_or_filename(Ary, FlowId),

    compile_file_store(
        MoreFileNames,
        maps:put(
            jstr(FlowId),
            #{
                path => jstr(FileName),
                id   => jstr(FlowId),
                name => TestName
            },
            FileStore
        )
    ).
