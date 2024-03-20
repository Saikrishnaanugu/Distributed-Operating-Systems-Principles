-module(simulator).
-export[start/0].

start() ->
    io:fwrite("\n\n Simulator Running\n\n"),
    
    {_, [Input_NumClients]} = io:fread("\nNumber of clients to simulate: ", "~s\n"),
    {_, [Input_MaxSubscribers]} = io:fread("\nMaximum Number of Subscribers a client can have: ", "~s\n"),
    {_, [Input_DisconnectClients]} = io:fread("\nPercentage of clients to disconnect to simulate periods of live connection and disconnection ", "~s\n"),

    % Convert to Integers
    NumClients = list_to_integer(Input_NumClients),
    MaxSubscribers = list_to_integer(Input_MaxSubscribers),


    Main_Table = ets:new(messages, [ordered_set, named_table, public]),
    createClients(1, NumClients, MaxSubscribers, Main_Table),
    
    %start time
    Start_Time = erlang:system_time(millisecond),
    %End time
    End_Time = erlang:system_time(millisecond),
    io:format("\nTime Taken to Converge: ~p milliseconds\n", [End_Time - Start_Time]).

% Function to spawn a client - and figure out its properties (UserName, NumTweets, NumSubscribe, PID)
createClients(Count, NumClients, MaxSubcribers, Main_Table) ->    
    UserName = Count,
    NumTweets = round(floor(MaxSubcribers/Count)),
    NumSubscribe = round(floor(MaxSubcribers/(NumClients-Count+1))) - 1,

    PID = spawn(client, tester, [UserName, NumTweets, NumSubscribe, false]),

    ets:insert(Main_Table, {UserName, PID}),
    if 
        Count == NumClients ->
            ok;
        true ->
            createClients(Count+1, NumClients, MaxSubcribers, Main_Table)
    end.