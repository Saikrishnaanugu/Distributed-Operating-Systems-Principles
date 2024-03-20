-module(client).
-export[start/0, tester/4].



start() ->
    io:fwrite("\n New Client connecting\n"),
    Port = 1204,
    Address = "localhost",
    {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {packet, 0}]),
    io:fwrite("\n connection request to server sent\n"),
    command(Socket, "_").

command(Socket, User) ->
    % cyclic loop to get the action by the client
    receive
        {tcp, Socket, Data} ->
            io:fwrite(Data),
            User1 = userCommand(Socket, User),
            command(Socket, User1);
        {tcp, closed, Socket} ->
            io:fwrite(" Diconnected ") 
        end.

userCommand(Socket, User) ->
    
    % Wait for input from Client and perform action based on input
    {ok, [CommandType]} = io:fread("\nEnter the command: ", "~s\n"),
    io:fwrite(CommandType),

    % Action based on input from Client
    if 
        CommandType == "register" ->
            % Input user-name
            {ok, [User2]} = io:fread("\nEnter your Desired Username: ", "~s\n"),
            User1 = registerAcc(Socket, User2);
        CommandType == "tweet" ->
            if
                User == "_" ->
                    io:fwrite("No Account Detected\n"),
                    User1 = userCommand(Socket, User);
                true ->
                    Tweet = io:get_line("\nWhat's Happening?"),
                    User1 = tweet(Socket,User, Tweet)
            end;
        CommandType == "retweet" ->
            if
                User == "_" ->
                    io:fwrite("No Account Detected\n"),
                    User1 = userCommand(Socket, User);
                true ->
                    {ok, [Person_User]} = io:fread("\nUsername to re-post: ", "~s\n"),
                    Tweet = io:get_line("\nTweet to be retweeted: "),
                    retweet(Socket, User, Person_User, Tweet),
                    User1 = User
            end;
        CommandType == "subscribe" ->
            if
                User == "_" ->
                    io:fwrite("No Account Detected\n"),
                    User1 = userCommand(Socket, User);
                true ->
                    SubscribeUser = io:get_line("\nWho to subscribe to?:"),
                    userSubcribe(Socket, User, SubscribeUser),
                    User1 = User
            end;
        CommandType == "query" ->
            if
                User == "_" ->
                    io:fwrite("No Account Detected\n"),
                    User1 = userCommand(Socket, User);
                true ->
                    io:fwrite("\n What do you want to Query?\n"),

                    % Tweets from other Users &
                    io:fwrite("\n 1. Subscribed Users Tweets\n"),

                    % Tweets with a specific HashTag
                    io:fwrite("\n 2. Hashtag Search\n"),

                    % Tweets that has mentioned me %
                    io:fwrite("\n 3. My Mentions\n"),

                    {ok, [Choice]} = io:fread("\n What is the information to be queried: ", "~s\n"),
                    query_tweet(Socket, User, Choice),
                    User1 = User
            end;
        true ->
            io:fwrite("Invalid command!, Please Enter another command!\n"),
            User1 = userCommand(Socket, User)
    end,
    User1.


registerAcc(Socket, User) ->
    % Send username to Server to check for validity and registration
    ok = gen_tcp:send(Socket, [["register", ",", User, ",", pid_to_list(self())]]),
    io:fwrite("\nAccount has been Registered\n"),
    User.

tweet(Socket,User, Tweet) ->
    ok = gen_tcp:send(Socket, ["tweet", "," ,User, ",", Tweet]),
    io:fwrite("\nTweet Sent\n"),
    User.

retweet(Socket, User,Person_User, Tweet) ->
    ok = gen_tcp:send(Socket, ["retweet", "," ,Person_User, ",", User,",",Tweet]),
    io:fwrite("\nTweet has been retweeted\n").

userSubcribe(Socket, User, SubscribeUser) ->
    ok = gen_tcp:send(Socket, ["subscribe", "," ,User, ",", SubscribeUser]),
    io:fwrite("\nYou have Subscribed to ~p!\n", [SubscribeUser]).

query_tweet(Socket, User, Choice) ->
    if
        Choice == "1" ->
            ok = gen_tcp:send(Socket, ["query", "," ,User, ",", "1"]);
        Choice == "2" ->
            Hashtag = io:get_line("\n Hashtag to search: "),
            ok = gen_tcp:send(Socket, ["query", "," ,User, ",","2",",", Hashtag]);
        true ->
            ok = gen_tcp:send(Socket, ["query", "," ,User, ",", "3"])
    end,
    io:fwrite("Queried related tweets").

tester(User, Tweets, Subscribers, false) ->
    io:fwrite("\n Start Simulation \n"),

    Port = 1204,
    Address = "localhost",
    {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {packet, 0}]),
    
    registerAcc(Socket, User),

    receive
        {tcp, Socket, Data} ->
            io:format("User ~p is registered", [Data])
    end,
    testertweeting(Socket, User, Tweets, Subscribers).

testertweeting(Socket, User, Tweets, Subscribers) ->
    
    % Subscribe
    if 
        Subscribers > 0 ->
            SList = listOfSubscribers(1, Subscribers, []),
            zipf_subscribe(Socket, User, SList)
    end,

    % Randomly select a user to mention in tweet
    RandomUser = rand:uniform(list_to_integer(User)),
    tweet(Socket, User, {"~p mentioned @~p ",[User, RandomUser]}),

    % Hashtag
    tweet(Socket, User, {"~p says #HashTag in their tweet",[User]}).

listOfSubscribers(Count, Subscribers, List) ->
        if
            (Count == Subscribers) ->
                [count | List];
            true ->
                listOfSubscribers(Count+1, Subscribers, [Count | List])
        end.

zipf_subscribe(Socket, User, SubList) ->

    [{SubscribeUser}|RemainingList] = SubList,
    userSubcribe(Socket, User, SubscribeUser),
    zipf_subscribe(Socket, User, RemainingList).