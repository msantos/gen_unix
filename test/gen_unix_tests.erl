%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
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
    FD = gen_unix:fd(gen_unix:fd(FD)).

ucred_struct_test() ->
    % Layout for struct ucred on this platform
    Cred = gen_unix:cred([]),
    Cred = gen_unix:cred(gen_unix:cred(Cred)).

credpass_test() ->
    ok = file:make_dir(?SOCKDIR),
    {ok, Socket0} = gen_unix:listen(?SOCKDIR ++ "/s"),
    spawn(fun() ->
            os:cmd("erl -pa ../ebin ../deps/*/ebin -s gen_unix_tests credsend")
            end),
    {ok, Socket} = poll(fun() -> procket:accept(Socket0) end),
    {ok, Cred} = poll(fun() -> gen_unix:credrecv(Socket) end),

    % Do socket cleanup first
    ok = procket:close(Socket0),
    ok = file:delete(?SOCKDIR ++ "/s"),
    ok = file:del_dir(?SOCKDIR),

    % Check the struct ucred is valid; the only common field between
    % OS'es is the uid. Doesn't appear to be a way natively to get our
    % uid, so all we do is check that the result is an integer.
    Ucred = gen_unix:cred(Cred),
    Uid = proplists:get_value(uid, Ucred),
    error_logger:info_report(Ucred),
    true = is_integer(Uid).

poll(Fun) ->
    case Fun() of
        {ok, Buf} ->
            {ok, Buf};
        {error, eagain} -> 
            timer:sleep(10),
            poll(Fun);
        Error ->
            Error
    end.

credsend() ->
    {ok, Socket} = gen_unix:connect(?SOCKDIR ++ "/s"),
    ok = gen_unix:credsend(Socket).
