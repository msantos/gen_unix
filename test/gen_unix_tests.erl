%% Copyright (c) 2013-2015, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(gen_unix_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(SOCKDIR, "/tmp/gen_unix").

fd_array_test() ->
    FD = [9,10,11,12,13,14],
    FD = unixsock:fd(unixsock:fd(FD)).

ucred_struct_test() ->
    % Layout for struct ucred on this platform
    Cred = unixsock:cred([]),
    Cred = unixsock:cred(unixsock:cred(Cred)).

socket_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun run/1
    }.

start() ->
    file:make_dir(?SOCKDIR),
    {ok, Ref} = gen_unix:start(),
    Ref.

stop(Ref) ->
    gen_unix:stop(Ref).

run(Ref) ->
    [
        credpass(Ref),
        fdpass(Ref)
    ].

credpass(Ref) ->
    {ok, Socket0} = gen_unix:listen(Ref, ?SOCKDIR ++ "/cred"),
    spawn(fun() ->
        os:cmd("erl -detached -pa ../ebin ../deps/*/ebin ebin deps/*/ebin -s gen_unix_tests credsend -s init stop")
    end),
    {ok, Socket} = gen_unix:accept(Ref, Socket0),

    {Bufsz, Msgsz} = unixsock:msg(credrecv),

    ok = unixsock:setsockopt(Socket, {credrecv, open}),
    {ok, _Buf, _Flags, Ctl} = gen_unix:recvmsg(Ref, Socket, Bufsz, Msgsz),
    ok = unixsock:setsockopt(Socket, {credrecv, close}),

    % Check for common fields
    {ok, Ucred} = unixsock:msg(Ctl),
    Uid = proplists:get_value(uid, Ucred),
    Gid = proplists:get_value(gid, Ucred),
    Pid = proplists:get_value(pid, Ucred),
%    error_logger:info_report(Ucred),

    [
        ?_assertEqual(true, is_integer(Uid)),
        ?_assertEqual(true, is_integer(Gid)),
        ?_assertEqual(true, is_integer(Pid)),
        ?_assertNotEqual(
            {0, 4294967295, 4294967295},
            {Pid, Uid, Gid}
        )
    ].

fdpass(Ref) ->
    {ok, Socket0} = gen_unix:listen(Ref, ?SOCKDIR ++ "/fd"),
    spawn(fun() ->
        os:cmd("erl -detached -pa ../ebin ../deps/*/ebin ebin deps/*/ebin -s gen_unix_tests fdsend -s init stop")
    end),
    {ok, Socket} = gen_unix:accept(Ref, Socket0),

    {Bufsz, Msgsz} = unixsock:msg({fdrecv, 2}),

    {ok, _Buf, _Flags, Ctl} = gen_unix:recvmsg(Ref, Socket, Bufsz, Msgsz),

    {ok, [FD1,FD2]} = unixsock:msg(Ctl),
    true = is_integer(FD1) and (FD1 > 2),
    true = is_integer(FD2) and (FD2 > 2),

    %error_logger:info_report([{fd1, FD1}, {fd2, FD2}]),

    {ok, Socket1} = gen_udp:open(0, [binary, {fd, FD1}, {active, false}]),
    {ok, Socket2} = gen_udp:open(0, [binary, {fd, FD2}, {active, false}]),

    [
        ?_assertMatch({ok, {{127,0,0,1}, _, <<0,1,2,3,4,5,6,7,8,9>>}}, gen_udp:recv(Socket1, 20)),
        ?_assertMatch({ok, {{127,0,0,1}, _, <<0,1,2,3,4,5,6,7,8,9>>}}, gen_udp:recv(Socket2, 20))
    ].

credsend() ->
    {ok, Ref} = gen_unix:start(),
    {ok, Socket} = gen_unix:connect(Ref, ?SOCKDIR ++ "/cred"),
    {Buf, Msg} = unixsock:msg(credsend),
    ok = gen_unix:sendmsg(Ref, Socket, Buf, Msg),
    timer:sleep(1000).

fdsend() ->
    {ok, Ref} = gen_unix:start(),
    {ok, Socket} = gen_unix:connect(Ref, ?SOCKDIR ++ "/fd"),

    % Open 2 random UDP ports
    {ok, Socket1} = gen_udp:open(0, [{active,false}]),
    {ok, Socket2} = gen_udp:open(0, [{active,false}]),

    {ok, Port1} = inet:port(Socket1),
    {ok, Port2} = inet:port(Socket2),

    {ok, FD1} = inet:getfd(Socket1),
    {ok, FD2} = inet:getfd(Socket2),

    {Buf, Msg} = unixsock:msg({fdsend, [FD1, FD2]}),
    ok = gen_unix:sendmsg(Ref, Socket, Buf, Msg),

    ok = gen_udp:close(Socket1),
    ok = gen_udp:close(Socket2),

    % Open 2 new sockets
    {ok, Socket3} = gen_udp:open(0, [{active,false}]),
    {ok, Socket4} = gen_udp:open(0, [{active,false}]),

    ok = gen_udp:send(Socket3, {127,0,0,1}, Port1, <<0,1,2,3,4,5,6,7,8,9>>),
    ok = gen_udp:send(Socket4, {127,0,0,1}, Port2, <<0,1,2,3,4,5,6,7,8,9>>),
    timer:sleep(1000).
