-module(miningclient).
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

% Info: Mining Function for client to mine for Hash Outputs
miner(_, 0, ServerNode) ->
    {miningserver, ServerNode} ! {self(), finished},
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
    spawn(miningclient, miner, [K, 1000000, ServerNode]),
    createMinerProcesses(NoOfProcess-1, K, ServerNode).

% Info: Function to start the mining activity
startminer(ServerNode) ->
    {clientCom, ServerNode} ! {self(),connected},
    receive
        {K} ->
            createMinerProcesses(8, K, ServerNode)
    after 3000 ->
        {timeout}

    end.