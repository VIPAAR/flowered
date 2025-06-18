-module(ered_flows).

-export([
    append_tab_name_to_filename/2,
    compute_timeout/1,
    ignore_as_eunit_test/1,
    is_test_case_pending/1,
    parse_flow_file/1,
    should_keep_flow_running/1
]).

%%
%% Module contains flow specific functionality. Anything that affects
%% an entire flow should be here.
%%

-import(ered_msg_handling, [
    decode_json/1
]).

%%
%% return the env array on the tab or empty array.
find_tab_env_ary([]) ->
    [];
find_tab_env_ary([NodeDef = #{<<"type">> := <<"tab">>} | _T]) ->
    case maps:find(<<"env">>, NodeDef) of
        {ok, V} ->
            V;
        _ ->
            []
    end;
find_tab_env_ary([_H | T]) ->
    find_tab_env_ary(T).

%%
%% Compute a timeout for the flow test, can be set in the flows.json file.
%% Since flows have delays or even test the delay node, it must be possible
%% to define a timeout per flow test. This is possible by using the ENV
%% variables, for a flow tab:
%%
%%   double-click on flow tab --> properties --> env --> ERED_TIMEOUT
%%
%% a timeout value is always in seconds.
%%
%% Default timeout is 1234ms which allows eunit to complete. The returned
%% value of this function is milliseconds, even if the original value in
%% the flow was seconds.
obtain_timeout([]) ->
    2222;
obtain_timeout([#{<<"value">> := V, <<"name">> := <<"ERED_TIMEOUT">>} | _T]) ->
    element(1, string:to_integer(V)) * 1000;
obtain_timeout([_H | T]) ->
    obtain_timeout(T).

compute_timeout(Ary) ->
    obtain_timeout(find_tab_env_ary(Ary)).

%%
%%
parse_flow_file(FileName) ->
    {ok, Json} = file:read_file(FileName),
    decode_json(Json).

%%
%%
append_tab_name_to_filename(Ary, FileName) ->
    append_tab_name_to_filename(
        Ary,
        FileName,
        maps:find(<<"z">>, lists:nth(2, Ary))
    ).

append_tab_name_to_filename([], FileName, {ok, TabId}) ->
    {TabId, FileName};
append_tab_name_to_filename([NodeDef | MoreNodeDefs], FileName, {ok, TabId}) ->
    case maps:find(<<"type">>, NodeDef) of
        {ok, <<"tab">>} ->
            {ok, Val} = maps:find(<<"label">>, NodeDef),
            {ok, TabId2} = maps:find(<<"id">>, NodeDef),

            {TabId2,
                binary_to_list(
                    list_to_binary(
                        io_lib:format(
                            "~s (~s)",
                            [Val, FileName]
                        )
                    )
                )};
        _ ->
            append_tab_name_to_filename(MoreNodeDefs, FileName, {ok, TabId})
    end.

%%
%%
get_pending_envvar([]) ->
    false;
get_pending_envvar([#{<<"value">> := V, <<"name">> := <<"ERED_PENDING">>} | _T]) ->
    (V == <<"true">>) or (V == <<"TRUE">>);
get_pending_envvar([_H | T]) ->
    get_pending_envvar(T).

is_test_case_pending(Ary) ->
    get_pending_envvar(find_tab_env_ary(Ary)).

%%
%%
keep_running([]) ->
    false;
keep_running([#{<<"value">> := V, <<"name">> := <<"ERED_KEEPRUNNING">>} | _T]) ->
    (V == <<"true">>) or (V == <<"TRUE">>);
keep_running([_H | T]) ->
    keep_running(T).

should_keep_flow_running(Ary) ->
    keep_running(find_tab_env_ary(Ary)).

%%
%% these are test flows that are ignored as eunit tests because they are
%% - at the time of writing - too annoying to test and secondly get tested
%% in the flow editor whenn all tests are run.
%%
%% Hence these test flows are not pending, they are "flow editor only tests"
not_eunit_test([]) ->
    false;
not_eunit_test([#{<<"value">> := V, <<"name">> := <<"ERED_NOT_EUNIT">>} | _T]) ->
    (V == <<"true">>) or (V == <<"TRUE">>);
not_eunit_test([_H | T]) ->
    not_eunit_test(T).

ignore_as_eunit_test(Ary) ->
    not_eunit_test(find_tab_env_ary(Ary)).
