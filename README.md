gen\_unix is an Erlang interface to Unix sockets.

Examples
========

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
    {ok,10}
    4> procket:read(FD, 1024).
    {ok,<<69,0,0,84,236,114,0,0,56,1,38,238,173,194,43,69,
          192,168,213,152,0,0,98,95,21,173,0,...>>}


