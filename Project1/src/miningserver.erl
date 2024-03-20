-module(miningserver).
-compile(export_all).

% Info : Generate Random String with the length defined by 'Length'
generateString(Length) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length("abcdefghijklmnopqrstuvxyz1234567890")),
                                   "abcdefghijklmnopqrstuvxyz1234567890")]
                            ++ Acc
                end, [], lists:seq(1, Length)).

% Info: Generate string of zeros to compare with the number of leading zeros for HashOutput
generateZeros(Length) ->
    string:copies("0",Length).

% Info: Calculating the Hash Output for the given HashInput    
generateHashValue(HashInput) ->
   io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,HashInput))]).

% Info: Check if HashOutput has leading Zeros 
valid_hashvalue(K, HashValue) ->
    LeadingZeros = string:substr(HashValue, 1, K),
    string:equal(LeadingZeros, generateZeros(K)).

% Info: Main Server Function to receive generated Hash Output with the defined leading Zeros.
server() ->
    receive
        {From, {HashValue, HashInput}} ->
            io:format("~s   ~s ~n", [HashInput, HashValue]),
            server();
        {_, finished} ->
            io:format("Calculations finished~n", []),
            server()

    end.

% Info: Function to communicate with Client
clientCom(K) ->
    receive
        {From, connected} ->
            io:format("Client [~p] connected~n", [From]),
            From ! {K}
    end,
    clientCom(K).


% Info: Mining Function for server to also mine for Hash Outputs
miner(_, 0, ServerNode) ->
    {miningserver, ServerNode} ! {self(), finished},
    measuretime_stop(),
    io:format("Hash Calculations finished by process ~p .~n", [self()]);

miner(K, N, ServerNode) ->
    Seq = generateString(8),
    HashInput = "jsrayan;" ++ Seq,
    HashValue = generateHashValue(HashInput),

    case valid_hashvalue(K, HashValue) of
        true ->
            {miningserver, ServerNode} ! {self(), {HashValue, HashInput}},
            miner(K, N-1, ServerNode);
        false ->
            miner(K, N-1, ServerNode)
    end.
    
% Info: Function to spawn multiple processes for mining
createMinerProcesses(0, _, _) ->
    io:format("Miners Spawned.~n", []);

createMinerProcesses(NoOfProcess, K, ServerNode) ->
    spawn(miningserver, miner, [K, 100000, ServerNode]),
    createMinerProcesses(NoOfProcess-1, K, ServerNode).

% Info: Function to start the server 
startserver() ->
    register(miningserver, spawn(miningserver, server, [])).

% Info: Function to set the number of leading zeros to be mined
setHashLimit(K) ->
    
    case whereis(clientCom) of
        undefined ->
            register(clientCom, spawn(miningserver, clientCom, [K]));
        _ -> 
            unregister(clientCom),
            register(clientCom, spawn(miningserver, clientCom, [K]))
    end.


% Info: Function to start the mining activity
startminer(ServerNode) ->
    {clientCom, ServerNode} ! {self(),connected},
    receive
        {K} ->
            createMinerProcesses(8, K, ServerNode),
            measuretime_start()
    after 3000 ->
        {timeout}

    end.


% Info: Functions to calculate the CPU and Real Times
measuretime_start() ->

    statistics(runtime),
    statistics(wall_clock).

measuretime_stop() ->

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 / 1000,
    U2 = Time2 / 1000,
    io:format("CPU time=~p seconds~n Real Time: ~p seconds~n",
    [U1,U2]).



