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
-module(gen_unix).
-include_lib("procket/include/procket.hrl").
-include_lib("procket/include/procket_msg.hrl").
%-include_lib("pkt/include/pkt.hrl").

-export([
    listen/1,
    connect/1,

    fdsend/2,
    fdrecv/1,

    credsend/1,
    credrecv/1
    ]).

-define(SCM_RIGHTS, 16#01).
-define(SCM_CREDENTIALS, 16#02).

% XXX testing only, move these to procket
-define(SO_PASSCRED, 16).

-record(ucred, {
        pid = 0,    % PID of sending process (pid_t)
        uid = 0,    % UID of sending process (uid_t)
        gid = 0     % GID of sending process (gid_t)
        }).

listen(Path) when is_list(Path) ->
    listen(list_to_binary(Path));
listen(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),
    Len = byte_size(Path),
    Sun = <<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
            Path/binary,
            0:((procket:unix_path_max()-Len)*8)>>,
    ok = procket:bind(Socket, Sun),
    ok = procket:listen(Socket, ?BACKLOG),
    {ok, Socket}.

connect(Path) when is_list(Path) ->
    connect(list_to_binary(Path));
connect(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),
    Len = byte_size(Path),
    Sun = <<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
            Path/binary,
            0:((procket:unix_path_max()-Len)*8)>>,
    ok = procket:connect(Socket, Sun),
    {ok, Socket}.

fdsend(Socket, FD) when is_integer(Socket), is_integer(FD) ->
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = ?SOL_SOCKET,
            type = ?SCM_RIGHTS,
            data = <<FD:4/native-unsigned-integer-unit:8>>
            }),
    {ok, Msg, _Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,        % must be empty (NULL) or eisconn
        iov = [<<"x">>],    % send 1 byte to differentiate success from EOF
        control = Cmsg
    }),
    procket:sendmsg(Socket, Msg, 0).

fdrecv(Socket) when is_integer(Socket) ->
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = 0,
            type = 0,
            data = <<0:4/native-unsigned-integer-unit:8>>
            }),
    {ok, Msg, Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,
        iov = [<<0:8>>],
        control = Cmsg
    }),
    case procket:recvmsg(Socket, Msg, 0) of
        {ok, 1, _Msghdr} ->
            fddata(procket:buf(proplists:get_value(msg_control, Res)),
                ?SOL_SOCKET, ?SCM_RIGHTS);
        {ok, N, _Msghdr} ->
            {error, {invalid_length, N}};
        {error, _} = Error ->
            Error
    end.

fddata({ok, Buf}, Level, Type) ->
    {Cmsg, _} = procket_msg:cmsghdr(Buf),
    case Cmsg of
        #cmsghdr{level = Level, type = Type} ->
            fd(Cmsg);
        _ ->
            {error, {invalid_cmsghdr, Level, Type}}
    end;
fddata(Error, _Level, _Type) ->
    Error.

fd(#cmsghdr{data = <<FD:4/native-unsigned-integer-unit:8>>}) ->
    {ok, FD};
fd(#cmsghdr{data = Data}) ->
    {error, {invalid_data, Data}}.

% struct ucred
% {
%     pid_t pid;            /* PID of sending process.  */
%     uid_t uid;            /* UID of sending process.  */
%     gid_t gid;            /* GID of sending process.  */
% };
credsend(Socket) when is_integer(Socket) ->
    {ok, Msg, _Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,        % must be empty (NULL) or eisconn
        iov = [<<"c">>],    % send 1 byte to differentiate success from EOF
        control = <<>>      % Automatically supply credentials (XXX portable?)
    }),
    procket:sendmsg(Socket, Msg, 0).

credrecv(Socket) when is_integer(Socket) ->
    ok = procket:setsockopt(Socket, ?SOL_SOCKET, ?SO_PASSCRED,
            <<1:4/native-unsigned-integer-unit:8>>),
    Sizeof_ucred = 4 + 4 + 4, % XXX check sizes
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = ?SOL_SOCKET,
            type = ?SCM_CREDENTIALS,
            data = <<0:(Sizeof_ucred * 8)>>
            }),
    {ok, Msg, Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,
        iov = [<<0:8>>],
        control = Cmsg
    }),
    case procket:recvmsg(Socket, Msg, 0) of
        {ok, 1, _Msghdr} ->
            creddata(procket:buf(proplists:get_value(msg_control, Res)),
                ?SOL_SOCKET, ?SCM_CREDENTIALS);
        {ok, N, _Msghdr} ->
            {error, {invalid_length, N}};
        {error, _} = Error ->
            Error
    end.

creddata({ok, Buf}, Level, Type) ->
    {Cmsg, _} = procket_msg:cmsghdr(Buf),
    case Cmsg of
        #cmsghdr{level = Level, type = Type} ->
            cred(Cmsg);
        _ ->
            {error, {invalid_cmsghdr, Level, Type}}
    end;
creddata(Error, _Level, _Type) ->
    Error.

cred(#cmsghdr{data = <<
        Pid:4/native-signed-integer-unit:8,
        Uid:4/native-unsigned-integer-unit:8,
        Gid:4/native-unsigned-integer-unit:8
        >>}) ->
    %{ok, [{pid, Pid}, {uid, Uid}, {gid, Gid}]};
    {ok, #ucred{pid = Pid, uid = Uid, gid = Gid}};
cred(#cmsghdr{data = Data}) ->
    {error, {invalid_data, Data}}.
