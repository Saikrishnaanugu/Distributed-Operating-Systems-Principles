-module(engine).
-import(maps, []).
-export[start/0].

start() ->
    io:format("\n Welcome to the new Twitter Service! \n"),
    DataTable = ets:new(messages, [ordered_set, named_table, public]),
    ClientMapping = ets:new(clients, [ordered_set, named_table, public]),

    % Wait for TCP Connections from client %
    {_, Socket} = gen_tcp:listen(1204, [binary, {keepalive, true}, {reuseaddr, true}, {active, false}]),
    await_connections(Socket, DataTable, ClientMapping).

await_connections(Listen, DataTable, ClientMapping) ->
    {_, Socket} = gen_tcp:accept(Listen),
    ok = gen_tcp:send(Socket, "YIP"),
    spawn(fun() -> await_connections(Listen, DataTable, ClientMapping) end),
    
    do_recv(Socket, DataTable, [], ClientMapping).

do_recv(Socket, DataTable, List, ClientMapping) ->
    io:format("Client message Received~n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data1} ->
            
            Data = re:split(Data1, ","),
            Type = binary_to_list(lists:nth(1, Data)),

            io:format("\n\nDATA: ~p\n\n ", [Data]),
            io:format("\n\nTYPE: ~p\n\n ", [Type]),

            if 
                Type == "register" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    PID = binary_to_list(lists:nth(3, Data)),
                    io:format("\nPID:~p\n", [PID]),
                    io:format("\nSocket:~p\n", [Socket]),
                    io:format("Type: ~p\n", [Type]),
                    io:format("\n~p wants to register an account\n", [UserName]),
                    
                    Output = ets:lookup(DataTable, UserName),
                    io:format("Output: ~p\n", [Output]),
                    if
                        Output == [] ->

                            ets:insert(DataTable, {UserName, [{"followers", []}, {"tweets", []}]}),      
                            ets:insert(ClientMapping, {UserName, Socket}),                
                            Array_List = ets:lookup(DataTable, UserName),
                            io:format("~p", [lists:nth(1, Array_List)]),

                            ok = gen_tcp:send(Socket, "New User has been registered"); 
                        true -> % Start Command again to register new name %
                            ok = gen_tcp:send(Socket, "Username is existing, please choose another!")
                    end,
                    do_recv(Socket, DataTable, [UserName], ClientMapping);

                Type == "tweet" ->
                    % Retrieve the UserName and the Tweet %
                    UserName = binary_to_list(lists:nth(2, Data)),
                    Tweet = binary_to_list(lists:nth(3, Data)),
                    io:format("\n ~p sent the following tweet: ~p", [UserName, Tweet]),
                    
                    Tupl = ets:lookup(DataTable, UserName),
                    io:format("Output: ~p\n", [Tupl]),
                    TupList = lists:nth(1, Tupl),
                    TupList2 = element(2, TupList),
                    TupList3 = maps:from_list(TupList2),
                    {_, CurrentFollowers} = maps:find("followers",TupList3),                         
                    {_, CurrentTweets} = maps:find("tweets",TupList3),

                    % If new tweet is sent, append the tweet to the current list of tweets %
                    NewTweets = CurrentTweets ++ [Tweet],
                    io:format("~p~n",[NewTweets]),
                    
                    ets:insert(DataTable, {UserName, [{"followers", CurrentFollowers}, {"tweets", NewTweets}]}),
                  
                    % Send the tweet to the followers of this User %
                    sendMessage(Socket, ClientMapping, Tweet, CurrentFollowers, UserName),
                    do_recv(Socket, DataTable, [UserName], ClientMapping);

                Type == "retweet" ->
                    SubsriberList = binary_to_list(lists:nth(2, Data)),
                    UserName = binary_to_list(lists:nth(3, Data)),
                    Subscriber = string:strip(SubsriberList, right, $\n),
                    io:format("User to retweet from: ~p\n", [Subscriber]),
                    Tweet = binary_to_list(lists:nth(4, Data)),
                    SubscriberEntry = ets:lookup(DataTable, Subscriber),
                    if
                        SubscriberEntry == [] ->
                            io:fwrite("No such name in Database!\n");
                        true ->
                            % Data about User
                            TupList = ets:lookup(DataTable, UserName),
                            TupList1 = lists:nth(1, TupList),
                            TupList2 = element(2, TupList1),
                            TupList3 = maps:from_list(TupList2),

                            % Data about Original Tweet User
                            TupList4 = lists:nth(1, SubscriberEntry),
                            TupList5 = element(2, TupList4),
                            TupList6 = maps:from_list(TupList5),
                            

                            {_, CurrentFollowers} = maps:find("followers",TupList3),
                            % Retweeting from the following 
                            {_, CurrentTweets} = maps:find("tweets",TupList6),
                            io:format("Retweet: ~p\n", [Tweet]),
                            CheckForTweet = lists:member(Tweet, CurrentTweets),
                            if
                                CheckForTweet == true ->
                                    NewTweet = string:concat(string:concat(string:concat("re:",Subscriber),"->"),Tweet),
                                    sendMessage(Socket, ClientMapping, NewTweet, CurrentFollowers, UserName);
                                true ->
                                    io:fwrite("Tweet does not exist!\n")
                            end     
                    end,
                    % Send the retweet details back to client %
                    do_recv(Socket, DataTable, [UserName], ClientMapping);

                Type == "subscribe" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    SubscribedUserName = binary_to_list(lists:nth(3, Data)),
                    Subscriber = string:strip(SubscribedUserName, right, $\n),

                    SubscriberEntry = ets:lookup(DataTable, Subscriber),
                    if
                        SubscriberEntry == [] ->
                            io:fwrite("Invalid User \n");
                        true ->

                            TupList = ets:lookup(DataTable, Subscriber),
                            TupList1 = lists:nth(1, TupList),
                            TupList2 = element(2, TupList1),

                            TupList3 = maps:from_list(TupList2),                            
                            {_, CurrentFollowers} = maps:find("followers",TupList3),
                            {_, CurrentTweets} = maps:find("tweets",TupList3),

                            % Add current user to the subscribe list of target user %
                            NewFollowers = CurrentFollowers ++ [UserName],
                            io:format("~p~n",[NewFollowers]),
                        
                            ets:insert(DataTable, {Subscriber, [{"followers", NewFollowers}, {"tweets", CurrentTweets}]}),

                            SubscriberEntry1 = ets:lookup(DataTable, Subscriber),

                            ok = gen_tcp:send(Socket, "User has been subscribed!"),

                            do_recv(Socket, DataTable, [UserName], ClientMapping)
                    end,
                    io:format("\n ~p wants to subscribe to ~p\n", [UserName, Subscriber]),
                    
                    ok = gen_tcp:send(Socket, "Subscribed!"),
                    do_recv(Socket, DataTable, [UserName], ClientMapping);

                Type == "query" ->
                    Choice = binary_to_list(lists:nth(3, Data)),
                    UserName = binary_to_list(lists:nth(2, Data)),
                    io:format("Query: The current username is -> ~p\n", [UserName]),
                    if
                        Choice == "1" ->
                            % Search for a specific User and his/her Tweets %
                            io:fwrite("Subscribed User Search\n"),
                            Sub_UserName = ets:first(DataTable),
                            Subscriber = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Subscriber]),
                            TupList = ets:lookup(DataTable, Subscriber),
                            TupList1 = lists:nth(1, TupList),
                            TupList2 = element(2, TupList1),
                            TupList3 = maps:from_list(TupList2),                            
                            {ok, CurrentTweets} = maps:find("tweets",TupList3),
                            io:format("\n ~p : ", [Subscriber]),
                            io:format("~p~n",[CurrentTweets]),
                            searchDataTable(DataTable, Subscriber, UserName);  

                        Choice == "2" ->
                            % Search for a specific HashTag from the input %
                            io:fwrite("Hashtag Search\n"),
                            Hashtag = binary_to_list(lists:nth(4, Data)),
                            io:format("Hashtag: ~p\n", [Hashtag]);
                        Choice == "3" ->
                             io:fwrite("Tweets mentioning me: \n");
                        true ->
                            io:format("Invalid Choice")
                    end,
                    io:format("\n ~p wants to query", [UserName]),
                    
                    do_recv(Socket, DataTable, [UserName], ClientMapping);
                true ->
                    io:fwrite("\n No Other Options! ")
            end;

        {error, closed} ->
            {ok, list_to_binary(List)};
        {error, Reason} ->
            io:fwrite("error: ~p", [Reason])
    end.

searchDataTable(DataTable, Key, UserName) ->
    Entry = ets:next(DataTable, Key),
    TupList = ets:lookup(DataTable, Entry),
    TupList1 = lists:nth(1, TupList),
    TupList2 = element(2, TupList1),
    TupList3 = maps:from_list(TupList2),                            
    {_, CurrentFollowers} = maps:find("followers",TupList3),
    IsaFollower = lists:member(UserName, CurrentFollowers),
    if
        IsaFollower == true ->
            {ok, CurrentTweets} = maps:find("tweets",TupList3),
            io:format("\n ~p : ", [Entry]),
            io:format("~p~n",[CurrentTweets]),
            searchDataTable(DataTable, Entry, UserName);
        true ->
            io:fwrite("\n No more tweets!\n")
    end,
    io:fwrite("\n Searching the whole table!\n").

sendMessage(Socket, ClientMapping, Tweet, Subscribers, UserName) ->
    if
        Subscribers == [] ->
            ok = gen_tcp:send(Socket, "Tweet has been sent\n"),
            io:fwrite("\nNo followers!\n");
        true ->

            [Client_To_Send | Remaining_List ] = Subscribers,
            io:format("Client to send: ~p\n", [Client_To_Send]),
            io:format("\nRemaining List: ~p~n",[Remaining_List]),
            Client_Socket_Row = ets:lookup(ClientMapping,Client_To_Send),
            Val3 = lists:nth(1, Client_Socket_Row),
            Client_Socket = element(2, Val3),
            io:format("\nClient Socket: ~p~n",[Client_Socket]),
            
            ok = gen_tcp:send(Client_Socket, ["New tweet received!\n",UserName,":",Tweet]),
            ok = gen_tcp:send(Socket, "Your tweet has been sent"),
            
            sendMessage(Socket, ClientMapping, Tweet, Remaining_List, UserName)
    end,
    io:fwrite("Send message!\n").

