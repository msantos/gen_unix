%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(gen_unix).
-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/1]).
-export([start_link/1]).
-export([
    listen/2,
    connect/2,
    accept/2, accept/3,
    close/2,
    sendmsg/3, sendmsg/4,
    recvmsg/3, recvmsg/4,

    pollid/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        poll,
        fds = dict:new()
    }).

start() ->
    start_link([]).
start(Options) when is_list(Options) ->
    start_link(Options).

start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

stop(Ref) ->
    gen_server:call(Ref, stop).

listen(Ref, Path) ->
    gen_server:call(Ref, {listen, Path}).

accept(Ref, Socket) ->
    accept(Ref, Socket, []).
accept(Ref, Socket, Options) ->
    Poll = pollid(Ref),
    Timeout = proplists:get_value(timeout, Options, infinity),
    case poll(Poll, Socket, [{mode,read}, {timeout, Timeout}]) of
        {ok,read} ->
            gen_server:call(Ref, {accept, Socket});
        Error ->
            Error
    end.

connect(Ref, Path) ->
    gen_server:call(Ref, {connect, Path}).

close(Ref, Socket) ->
    gen_server:call(Ref, {close, Socket}).

sendmsg(Ref, Socket, Msg) ->
    sendmsg(Ref, Socket, Msg, []).
sendmsg(_Ref, Socket, Msg, Options) ->
    Flags = proplists:get_value(flags, Options, 0),
    unixsock:sendmsg(Socket, Msg, Flags).

recvmsg(Ref, Socket, Msg) ->
    recvmsg(Ref, Socket, Msg, []).
recvmsg(Ref, Socket, Msg, Options) ->
    Flags = proplists:get_value(flags, Options, 0),
    Timeout = proplists:get_value(timeout, Options, infinity),
    Poll = pollid(Ref),
    case poll(Poll, Socket, [{mode,read}, {timeout,Timeout}]) of
        {ok,read} ->
            unixsock:recvmsg(Socket, Msg, Flags);
        Error ->
            Error
    end.

pollid(Ref) ->
    gen_server:call(Ref, pollid).


%%--------------------------------------------------------------------
%%% gen_server callbacks
%%--------------------------------------------------------------------
init([_Options]) ->
    process_flag(trap_exit, true),
    Poll = prim_inert:start(),
    {ok, #state{
            poll = Poll
        }}.

handle_call({listen, Path}, _From, #state{fds = FDs} = State) ->
    Reply = unixsock:listen(Path),
    FDs1 = case Reply of
        {ok, FD} ->
            dict:store(FD, Path, FDs);
        _Error ->
            FDs
    end,
    {reply, Reply, State#state{fds = FDs1}};

handle_call({accept, Socket}, _From, #state{fds = FDs} = State) ->
    Reply = unixsock:accept(Socket),
    FDs1 = case Reply of
        {ok, FD} ->
            dict:store(FD, undefined, FDs);
        _Error ->
            FDs
    end,
    {reply, Reply, State#state{fds = FDs1}};

handle_call({connect, Path}, _From, #state{fds = FDs} = State) ->
    Reply = unixsock:connect(Path),
    FDs1 = case Reply of
        {ok, FD} ->
            dict:store(FD, undefined, FDs);
        _Error ->
            FDs
    end,
    {reply, Reply, State#state{fds = FDs1}};

handle_call({close, FD}, _From, #state{fds = FDs} = State) ->
    Reply = unixsock:close(FD),
    FDs1 = case Reply of
        ok ->
            case dict:find(FD, FDs) of
                {ok, undefined} ->
                    dict:erase(FD, FDs);
                {ok, Path} ->
                    file:delete(Path),
                    dict:erase(FD, FDs);
                _ ->
                    FDs
            end;
        _Error ->
            FDs
    end,
    {reply, Reply, State#state{fds = FDs1}};

handle_call(pollid, _From, #state{poll = Poll} = State) ->
    {reply, Poll, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, #state{fds = FDs, poll = Poll}) ->
    dict:map(fun
            (FD, undefined) ->
                unixsock:close(FD);
            (FD, Path) ->
                unixsock:close(FD),
                file:delete(Path)
        end, FDs),
    prim_inert:stop(Poll),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
poll(Poll, Socket, Options) ->
    prim_inert:poll(Poll, Socket, Options).
