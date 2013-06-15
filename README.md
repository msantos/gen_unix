gen\_unix is an Erlang interface to Unix sockets.

Examples
========

* File descriptor passing

  Open 2 shell prompts and start an Erlang VM in each:

        # VM 1
        $ ./start.sh
        1> {ok, Listen} = gen_unix:listen("/tmp/test").
        {ok,8}

        # VM 2
        $ sudo ./start.sh
        1> {ok, Socket} = gen_unix:connect("/tmp/test").
        {ok,7}

        2> {ok, FD1} = procket:socket(inet,raw,icmp).
        {ok,8}

        3> {ok, FD2} = procket:socket(inet6,raw,icmp6).
        {ok,9}

        3> gen_unix:fdsend(S, [FD1, FD2]).
        ok

        # Open another shell and send a few ping's

        # Back in VM 1
        2> {ok, Socket} = gen_unix:accept(Listen).
        {ok,9}

        3> {ok, FD} = gen_unix:fdrecv(Socket, 2).
        {ok,<<10,0,0,0,11,0,0,0>>}

        4> [FD1, FD2] = gen_unix:fd(FD).
        "\n\v" % [10,11]

        5> procket:read(FD1, 1024).
        {ok,<<69,0,0,84,236,114,0,0,56,1,38,238,173,194,43,69,
              192,168,213,152,0,0,98,95,21,173,0,...>>}

* Credential passing

        # VM 1
        $ ./start.sh
        1> {ok, Listen} = gen_unix:listen("/tmp/test").
        {ok,8}

        # VM 2
        $ ./start.sh
        1> {ok, Socket} = gen_unix:connect("/tmp/test").
        {ok,7}

        2> gen_unix:credsend(S).
        ok

        # Back in VM 1
        2> {ok, Socket} = gen_unix:accept(Listen).
        {ok,9}

        3> {ok, Cred} = gen_unix:credrecv(Socket).
        {ok,<<66,50,0,0,232,3,0,0,232,3,0,0>>}

        4> gen_unix:cred(Cred).
        [{pid,12866},{uid,1000},{gid,1000}]
