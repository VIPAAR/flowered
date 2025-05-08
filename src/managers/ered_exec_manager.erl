-module(ered_exec_manager).

-behaviour(gen_server).

-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    stop/0,
    start/5
]).

%%
%% The exec manager that acts as a wrapper around erlexec which sends messages
%% to the linked process. This module receives those messages and sends them
%% to the nodes connected nodes, i.e. along the wires.
%%

-import(ered_nodes, [
    jstr/2,
    send_msg_on/2
]).

-define(STDOUTWIRES, element(2, State)).
-define(STDERRWIRES, element(3, State)).
-define(DONEWIRES, element(4, State)).
-define(NODEPID, element(1, State)).
-define(MSG, element(6, State)).
-define(CMD, element(5, State)).
-define(MACHPID, element(9, State)).
-define(TIMEOUT, maps:get(timeout, element(7, State))).

% erlfmt:ignore alignment
start(NodePid, Wires, Cmd, Msg, Opts) ->
    exec:start(),
    [StdoutWires, StderrWires, DoneWires] = Wires,
    gen_server:start(
        ?MODULE,
        [
            {
                NodePid,      %% 1
                StdoutWires,  %% 2
                StderrWires,  %% 3
                DoneWires,    %% 4
                Cmd,          %% 5
                Msg,          %% 6
                Opts,         %% 7
                -1,           %% 8 - Erlang PID
                -1            %% 9 - Machine PID
            }
        ],
        []
    ).

init([State]) ->
    {ok, State}.

%%
%%
handle_call({kill_command, Signal}, _From, State) ->
    exec:kill(?MACHPID, sig_to_num(binary_to_atom(Signal))),
    {reply, ok, State};
handle_call(run_command, _From, State) ->
    {ok, Pid, MachPid} = exec:run(?CMD, [stdout, stderr, monitor]),
    ?TIMEOUT > 0 andalso erlang:start_timer(?TIMEOUT * 1000, self(), 'TIMEOUT'),
    State2 = setelement(9, setelement(8, State, Pid), MachPid),
    {reply, MachPid, State2};
handle_call(Msg, _From, State) ->
    io:format("Exec Manager Unknown Call: ~p~n", [Msg]),
    {reply, ok, State}.

%%
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    io:format("Exec Manager Unknown Cast: ~p~n", [Msg]),
    {noreply, State}.

%%
%%
handle_info({timeout, _, 'TIMEOUT'}, State) ->
    exec:kill(?MACHPID, sig_to_num('SIGTERM')),
    %% give the process 1 second to die else kill it.
    erlang:start_timer(1000, self(), 'TIMEOUT-KILL'),
    {noreply, State};
handle_info({timeout, _, 'TIMEOUT-KILL'}, State) ->
    exec:kill(?MACHPID, sig_to_num('SIGKILL')),
    {noreply, State};
handle_info({'DOWN', MachPid, process, _ErlangPid, Status}, State) ->
    Status2 =
        case Status of
            normal ->
                #{pid => MachPid, code => 0};
            {exit_status, Num} ->
                #{pid => MachPid, code => Num};
            _ ->
                io:format("ExecMgs: Unknown Status: ~p~n", [Status]),
                #{pid => MachPid, code => jstr("Unknown: ~p", [Status])}
        end,
    gen_server:cast(
        ?NODEPID, {exec_process_died, maps:put(payload, Status2, ?MSG)}
    ),
    send_msg_on(?DONEWIRES, maps:put(payload, Status2, ?MSG)),
    {stop, normal, State};
handle_info({stdout, _Pid, Payload}, State) ->
    send_msg_on(?STDOUTWIRES, maps:put(payload, Payload, ?MSG)),
    {noreply, State};
handle_info({stderr, _Pid, Payload}, State) ->
    send_msg_on(?STDERRWIRES, maps:put(payload, Payload, ?MSG)),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Exec Manager Unknown Info: ~p~n", [Msg]),
    {noreply, State}.

%%
%%
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:cast(?MODULE, stop).

%%
%%
terminate(normal, _State) ->
    ok;
terminate(Event, _State) ->
    io:format("Exec manager terminated with {{{ ~p }}}~n", [Event]),
    ok.

%%
%%
% erlfmt:ignore alignment
sig_to_num('SIGHUP')      -> 1;
sig_to_num('SIGINT')      -> 2;
sig_to_num('SIGQUIT')     -> 3;
sig_to_num('SIGILL')      -> 4;
sig_to_num('SIGTRAP')     -> 5;
sig_to_num('SIGABRT')     -> 6;
sig_to_num('SIGBUS')      -> 7;
sig_to_num('SIGFPE')      -> 8;
sig_to_num('SIGKILL')     -> 9;
sig_to_num('SIGUSR1')     -> 10;
sig_to_num('SIGSEGV')     -> 11;
sig_to_num('SIGUSR2')     -> 12;
sig_to_num('SIGPIPE')     -> 13;
sig_to_num('SIGALRM')     -> 14;
sig_to_num('SIGTERM')     -> 15;
sig_to_num('SIGSTKFLT')   -> 16;
sig_to_num('SIGCHLD')     -> 17;
sig_to_num('SIGCONT')     -> 18;
sig_to_num('SIGSTOP')     -> 19;
sig_to_num('SIGTSTP')     -> 20;
sig_to_num('SIGTTIN')     -> 21;
sig_to_num('SIGTTOU')     -> 22;
sig_to_num('SIGURG')      -> 23;
sig_to_num('SIGXCPU')     -> 24;
sig_to_num('SIGXFSZ')     -> 25;
sig_to_num('SIGVTALRM')   -> 26;
sig_to_num('SIGPROF')     -> 27;
sig_to_num('SIGWINCH')    -> 28;
sig_to_num('SIGIO')       -> 29;
sig_to_num('SIGPWR')      -> 30;
sig_to_num('SIGSYS')      -> 31;
sig_to_num('SIGRTMIN')    -> 34;
sig_to_num('SIGRTMIN+1')  -> 35;
sig_to_num('SIGRTMIN+2')  -> 36;
sig_to_num('SIGRTMIN+3')  -> 37;
sig_to_num('SIGRTMIN+4')  -> 38;
sig_to_num('SIGRTMIN+5')  -> 39;
sig_to_num('SIGRTMIN+6')  -> 40;
sig_to_num('SIGRTMIN+7')  -> 41;
sig_to_num('SIGRTMIN+8')  -> 42;
sig_to_num('SIGRTMIN+9')  -> 43;
sig_to_num('SIGRTMIN+10') -> 44;
sig_to_num('SIGRTMIN+11') -> 45;
sig_to_num('SIGRTMIN+12') -> 46;
sig_to_num('SIGRTMIN+13') -> 47;
sig_to_num('SIGRTMIN+14') -> 48;
sig_to_num('SIGRTMIN+15') -> 49;
sig_to_num('SIGRTMAX-14') -> 50;
sig_to_num('SIGRTMAX-13') -> 51;
sig_to_num('SIGRTMAX-12') -> 52;
sig_to_num('SIGRTMAX-11') -> 53;
sig_to_num('SIGRTMAX-10') -> 54;
sig_to_num('SIGRTMAX-9')  -> 55;
sig_to_num('SIGRTMAX-8')  -> 56;
sig_to_num('SIGRTMAX-7')  -> 57;
sig_to_num('SIGRTMAX-6')  -> 58;
sig_to_num('SIGRTMAX-5')  -> 59;
sig_to_num('SIGRTMAX-4')  -> 60;
sig_to_num('SIGRTMAX-3')  -> 61;
sig_to_num('SIGRTMAX-2')  -> 62;
sig_to_num('SIGRTMAX-1')  -> 63;
sig_to_num('SIGRTMAX')    -> 64;
%% Default is SIGTERM
sig_to_num(_) -> sig_to_num('SIGTERM').
