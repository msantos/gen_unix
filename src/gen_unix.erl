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
    fdrecv/1, fdrecv/2,
    fd/1,

    credsend/1, credsend/2,
    credrecv/1,
    cred/1
    ]).

-define(SCM_RIGHTS, 16#01).
-define(SCM_CREDENTIALS, 16#02).

% XXX testing only, move these to procket
-define(SO_PASSCRED, 16).


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

fdsend(Socket, FD) when is_list(FD) ->
    fdsend(Socket, fd(FD));
fdsend(Socket, FD) when is_integer(Socket), is_binary(FD) ->
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = ?SOL_SOCKET,
            type = ?SCM_RIGHTS,
            data = FD
            }),
    {ok, Msg, _Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,        % must be empty (NULL) or eisconn
        iov = [<<"x">>],    % send 1 byte to differentiate success from EOF
        control = Cmsg
    }),
    procket:sendmsg(Socket, Msg, 0).

fdrecv(Socket) ->
    fdrecv(Socket, 1).
fdrecv(Socket, NFD) when is_integer(Socket), is_integer(NFD) ->
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = 0,
            type = 0,
            data = <<0:(NFD * 4 * 8)>>
            }),
    {ok, Msg, Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,
        iov = [<<0:8>>],
        control = Cmsg
    }),
    case procket:recvmsg(Socket, Msg, 0) of
        {ok, 1, _Msghdr} ->
            data(procket:buf(proplists:get_value(msg_control, Res)),
                ?SOL_SOCKET, ?SCM_RIGHTS);
        {ok, N, _Msghdr} ->
            {error, {invalid_length, N}};
        {error, _} = Error ->
            Error
    end.

data({ok, Buf}, Level, Type) ->
    {Cmsg, _} = procket_msg:cmsghdr(Buf),
    case Cmsg of
        #cmsghdr{level = Level, type = Type, data = Data} ->
            {ok, Data};
        #cmsghdr{level = Level1, type = Type1} ->
            {error, {invalid_cmsghdr, [
                        {expected, {Level, Type}},
                        {received, {Level1, Type1}}
                        ]}}
    end;
data(Error, _Level, _Type) ->
    Error.

fd(FDs) when is_binary(FDs) ->
    [ FD || <<FD:4/native-unsigned-integer-unit:8>> <= FDs ];
fd(FDs) when is_list(FDs) ->
    << <<FD:4/native-unsigned-integer-unit:8>> || FD <- FDs >>.

credsend(Socket) when is_integer(Socket) ->
    credsend_1(Socket, <<>>).

credsend(Socket, Cred) when is_list(Cred) ->
    credsend(Socket, cred(Cred));
credsend(Socket, Cred) when is_integer(Socket), is_binary(Cred) ->
    Cmsg = procket_msg:cmsghdr(#cmsghdr{
            level = ?SOL_SOCKET,
            type = ?SCM_CREDENTIALS,
            data = Cred
            }),
    credsend_1(Socket, Cmsg).

credsend_1(Socket, Cmsg) when is_integer(Socket) ->
    {ok, Msg, _Res} = procket_msg:msghdr(#msghdr{
        name = <<>>,
        iov = [<<"c">>],
        control = Cmsg
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
            data(procket:buf(proplists:get_value(msg_control, Res)),
                ?SOL_SOCKET, ?SCM_CREDENTIALS);
        {ok, N, _Msghdr} ->
            {error, {invalid_length, N}};
        {error, _} = Error ->
            Error
    end.

cred(Data) ->
    cred(os:type(), Data).

% struct ucred
% {
%     pid_t pid;            /* PID of sending process.  */
%     uid_t uid;            /* UID of sending process.  */
%     gid_t gid;            /* GID of sending process.  */
% };
cred({unix, linux}, <<
        Pid:4/native-signed-integer-unit:8,
        Uid:4/native-unsigned-integer-unit:8,
        Gid:4/native-unsigned-integer-unit:8
        >>) ->
    [{pid, Pid}, {uid, Uid}, {gid, Gid}];
cred({unix, linux}, Fields) when is_list(Fields) ->
    Pid = proplists:get_value(pid, Fields, list_to_integer(os:getpid())),
    Uid = proplists:get_value(uid, Fields, 0), % XXX no way to get uid?
    Gid = proplists:get_value(gid, Fields, 0), % XXX or gid?
    <<Pid:4/native-signed-integer-unit:8,
      Uid:4/native-unsigned-integer-unit:8,
      Gid:4/native-unsigned-integer-unit:8>>;

% #define XU_NGROUPS  16
% #define XUCRED_VERSION  0
% struct xucred {
%     u_int   cr_version;     /* structure layout version */
%     uid_t   cr_uid;         /* effective user id */
%     short   cr_ngroups;     /* number of groups */
%     gid_t   cr_groups[XU_NGROUPS];  /* groups */
%     void    *_cr_unused1;       /* compatibility with old ucred */
% };
cred({unix, freebsd}, <<
        Version:4/native-unsigned-integer-unit:8,
        Uid:4/native-unsigned-integer-unit:8,
        Ngroups:2/native-signed-integer-unit:8,
        Rest/binary
        >>) ->
    Num = Ngroups * 4 * 8,
    <<Gr:Num, _/binary>> = Rest,
    Groups = [ N || <<N:4/native-unsigned-integer-unit:8>> <= Gr ],
    [{version, Version}, {uid, Uid}, {groups, Groups}];
cred({unix, freebsd}, Fields) when is_list(Fields) ->
    Size = erlang:system_info({wordsize, external}),
    Version = proplists:get_value(version, Fields, 0),
    Uid = proplists:get_value(uid, Fields, 0),
    Groups = proplists:get_value(groups, Fields, [0]),
    Ngroups = length(Groups),
    Gr = << <<N:4/native-unsigned-integer-unit:8>> || N <- Groups >>,
    Pad = (16 - Ngroups) * 8,
    <<Version:4/native-unsigned-integer-unit:8,
      Uid:4/native-unsigned-integer-unit:8,
      Ngroups:2/native-signed-integer-unit:8,
      Gr/binary, 0:Pad,
      0:Size/native-unsigned-integer-unit:8>>;

% OpenBSD
% struct ucred {
%     u_int   cr_ref;         /* reference count */
%     uid_t   cr_uid;         /* effective user id */
%     gid_t   cr_gid;         /* effective group id */
%     short   cr_ngroups;     /* number of groups */
%     gid_t   cr_groups[NGROUPS]; /* groups */
% };
%
% NetBSD
% struct uucred {
%     unsigned short  cr_unused;      /* not used, compat */
%     uid_t       cr_uid;         /* effective user id */
%     gid_t       cr_gid;         /* effective group id */
%     short       cr_ngroups;     /* number of groups */
%     gid_t       cr_groups[NGROUPS_MAX]; /* groups */
% };
cred({unix, _}, <<
        Ref:4/native-unsigned-integer-unit:8,
        Uid:4/native-unsigned-integer-unit:8,
        Gid:4/native-unsigned-integer-unit:8,
        Ngroups:2/native-signed-integer-unit:8,
        Rest/binary
        >>) ->
    Num = Ngroups * 4 * 8,
    <<Gr:Num, _/binary>> = Rest,
    Groups = [ N || <<N:4/native-unsigned-integer-unit:8>> <= Gr ],
    [{ref, Ref}, {uid, Uid}, {gid, Gid}, {groups, Groups}];
cred({unix, _}, Fields) when is_list(Fields) ->
    Ref = proplists:get_value(ref, Fields, 0),
    Uid = proplists:get_value(uid, Fields, 0),
    Gid = proplists:get_value(gid, Fields, 0),
    Groups = proplists:get_value(groups, Fields, [0]),
    Ngroups = length(Groups),
    Gr = << <<N:4/native-unsigned-integer-unit:8>> || N <- Groups >>,
    Pad = (16 - Ngroups) * 8, % NGROUPS_MAX = 16
    <<Ref:4/native-unsigned-integer-unit:8,
      Uid:4/native-unsigned-integer-unit:8,
      Gid:4/native-unsigned-integer-unit:8,
      Ngroups:2/native-signed-integer-unit:8,
      Gr/binary, 0:Pad>>.
