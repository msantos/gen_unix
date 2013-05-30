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

        2> {ok, FD} = procket:socket(inet,raw,icmp).
        {ok,8}

        3> gen_unix:fdsend(S, FD).
        {ok,1,
            <<0,0,0,0,0,0,0,0,96,14,144,114,1,0,0,0,112,14,144,114,
                  16,0,0,0,0,0,...>>}

        # Open another shell and send a few ping's

        # Back in VM 1
        2> {ok, Socket} = procket:accept(Listen).
        {ok,9}

        3> {ok, FD} = gen_unix:fdrecv(Socket).
        {ok,<<10,0,0,0>>}

        4> gen_unix:fd(FD).
        10

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
        {ok,1,
            <<0,0,0,0,0,0,0,0,96,14,224,114,1,0,0,0,0,0,0,0,0,0,0,0,
              0,0,...>>}

        # Back in VM 1
        2> {ok, Socket} = procket:accept(Listen).
        {ok,9}

        3> {ok, Cred} = gen_unix:credrecv(Socket).
        {ok,<<66,50,0,0,232,3,0,0,232,3,0,0>>}

        4> gen_unix:cred(Cred).
        [{pid,12866},{uid,1000},{gid,1000}]
