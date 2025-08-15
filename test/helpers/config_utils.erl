%% test/test_utils.erl
-module(config_utils).
-export([load_config/0, get_config/1, put_config/2, remove_config/1]).

load_config() ->
    ConfigPath = test_config_path(),
    case file:consult(ConfigPath) of
        {ok, [Config]} ->
            lists:foreach(fun({App, Settings}) ->
                application:load(App),
                lists:foreach(fun({Key, Value}) ->
                    application:set_env(App, Key, Value)
                end, Settings)
            end, Config),
            ok;
        {error, Reason} ->
            Reason
    end.

test_config_path() ->
    PossiblePaths = [
        "config/sys.config"
    ],
    case lists:filter(fun filelib:is_file/1, PossiblePaths) of
        [Path|_] -> Path;
        [] -> error(no_config_found)
    end.

get_config(Key) ->
    application:get_env(flowered, Key, undefined).

put_config(Key, Value) ->
    application:set_env(flowered, Key, Value).

remove_config(Key) ->
    application:unset_env(flowered, Key).
