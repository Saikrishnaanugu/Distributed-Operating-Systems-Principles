-module(engine).
-behavior(gen_server).
-export([start/0, init/1,schedule_periodic_computation_for_tweets_and_retweets/0]).


start() ->                                                      
    gen_server:start_link({local, engine}, engine, [], []).

init(_Args) ->

    schedule_periodic_computation_for_tweets_and_retweets(),

    ets:new(users, [bag, protected, named_table]),
    ets:new(hashTags, [bag, protected, named_table]),
    ets:new(tweets, [bag, protected, named_table]),
    ets:new(user_mention_tweets, [bag, protected, named_table]),
    ets:new(retweets, [bag, protected, named_table]),
    ets:new(user_list, [set, protected, named_table]),
    ets:new(client_zipf_details_per_client_node,[set, protected, named_table]),

    ets:insert_new(user_list,{"user_list",[]}),

    State = #{start_value=>1,number_of_tweets_before=>0, number_of_tweets_after=>0, number_of_retweets_before=>0,number_of_retweets_after=>0,hashTag=>[],count_numClients=>0,numClients=>0},
    {ok, State}.


schedule_periodic_computation_for_tweets_and_retweets() ->
        timer:send_after(5*1000, self(), periodic_computation_for_tweets_and_retweets).

handle_info(periodic_computation_for_tweets_and_retweets, State) ->
        Number_of_tweets = maps:get(number_of_tweets_after, State) - maps:get(number_of_tweets_before, State),
        Number_of_retweets=maps:get(number_of_retweets_after, State) - maps:get(number_of_retweets_before, State),
        
        Number_of_tweets_after=maps:get(number_of_tweets_after, State),
        Number_of_retweets_after=maps:get(number_of_retweets_after, State),

        State_retweets = maps:update(number_of_retweets_before, Number_of_retweets_after, State),
        State = maps:merge(State,State_retweets),

        State_tweets = maps:update(number_of_tweets_before, Number_of_tweets_after, State),
        State =  maps:merge(State,State_tweets),

        io:format("Number of tweets per 5 seconds = ~p ~n", [Number_of_tweets]),
        io:format("Number of retweets per 5 seconds = ~p ~n", [Number_of_retweets]),

        schedule_periodic_computation_for_tweets_and_retweets(),
        {noreply, State}.

handle_call({get_start_value}, From, State) ->
    StartValue = maps:get(start_value, State),
   {reply,StartValue,State};

handle_call({get_list_users}, From , State) ->
    Array_list = ets:lookup(user_list, "user_list"),
    Elem_tuple = element(1, Array_list),
    LList = element(1, Elem_tuple),
    %array_list=:ets.lookup(:user_list, "user_list")
    %elem_tuple=Enum.at(array_list,0)
    %list=elem(elem_tuple,1)
    {reply,LList,State}.


handle_cast({update_start_value,NewValue},State) ->
    State_start_value = maps:update(start_value, NewValue, State),
    State = maps:merge(State,State_start_value),
    {noreply,State};

handle_cast({created_user,Node_client,Password,Name_node,Id},State) ->

    Process_map = #{node_client => nil, hashTags => [], password => nil, has_subscribed_to => [], is_subscribed_by => [],name_node => nil, id => nil, no_of_zipf_tweets =>0, probability_of_zipf_functions=>0, number_of_subscribers=>0},

    State_name_node = maps:update(name_node, Name_node, Process_map),
    Process_map = maps:merge(Process_map,State_name_node),

    State_password = maps:update(password, Password, Process_map),
    Process_map = maps:merge(Process_map,State_password),


    State_node_client = maps:update(node_client, Node_client, Process_map),
    Process_map = maps:merge(Process_map,State_node_client),
    
    State_id = maps:update(id, Id, Process_map),
    Process_map = maps:merge(Process_map,State_id),

    %Update the user_list with the client and node tuple
    %{name_of_node,client_node_name}
    Array_list = ets:lookup(user_list, "user_list"),
    Elem_tuple = element(1, Array_list),
    LList = element(1, Elem_tuple),
    LList=LList++[{Name_node,Node_client,0}],
    ets:insert(user_list,{"user_list",LList}),

    %Added it to the users table
    ets:insert(users,{Name_node,Process_map}),

    {noreply,State};

handle_cast({got_tweet,Random_tweet,Random_hashTag,Name_of_user,Client_node_name,Reference,IsFreshUser},State) ->

    %Change Tweets Table
    
    Process_map_tweets_table = #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil, reference_node=>nil},

    Random_tweet_1 = maps:update(tweet, Random_tweet, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, Random_hashTag, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Reference_1),
          
    ets:insert(tweets,{Name_of_user,Process_map_tweets_table}),    

    %Change only HashTags 

    Process_map_hashTag_table = #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil,reference_node=>nil},

    Random_tweet_1 = maps:update(tweet, Random_tweet, Process_map_tweets_table),
    Process_map_hashTag_table = maps:merge(Process_map_hashTag_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, Random_hashTag, Process_map_tweets_table),
    Process_map_hashTag_table = maps:merge(Process_map_hashTag_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_tweets_table),
    Process_map_hashTag_table = maps:merge(Process_map_hashTag_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_tweets_table),
    Process_map_hashTag_table = maps:merge(Process_map_hashTag_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_tweets_table),
    Process_map_hashTag_table = maps:merge(Process_map_hashTag_table,Reference_1),
          
    ets:insert(hashTags,{Random_hashTag,Process_map_hashTag_table}),

    State_tweets = maps:update_with(number_of_tweets_after, fun(V) -> V+1 end, State),
    State = maps:merge(State,State_tweets),
        
    State_hashTag = maps:update_with(hashTag, fun(V) -> V++[random_hashTag] end, State),
    State = maps:merge(State,State_hashTag),
          
         
        %IO.inspect "#{inspect random_hashTag} #{inspect state[:hashTag]}"

        Client_name = Name_of_user,
        Client_node = Client_node_name,


        % Find the is subscribed user for the given client

        %Format of the output
        %[tweeter@user1: :"localhost-20@10.3.6.63",
        % tweeter@user2: :"localhost-20@10.3.6.63",
        % tweeter@user3: :"localhost-20@10.3.6.63",
        % tweeter@user6: :"localhost-20@10.3.6.63",
        % tweeter@user9: :"localhost-20@10.3.6.63"]
        %IO.inspect isFreshUser

        if
            IsFreshUser == false ->
            Is_subscribed_by = get_a_list_of_is_subscribed_by_for_given_client(Client_name,Client_node,State),
           %{:got_a_tweet,random_tweet,random_hashtag,name_of_user,client_node_name,_,client_name_x,client_node_name_x}
           %TODO%Enum.each(is_subscribed_by,fn({client_name_x,client_node_name_x,y}) -> GenServer.cast({client_name_x,client_node_name_x},{:got_a_tweet,random_tweet,random_hashTag,name_of_user,client_node_name,reference,client_name_x,client_node_name_x})  end)
            {noreply, State};
           true ->
            {noreply, State}

        end;

handle_cast({add_subscription_for_given_client_user,Random_node_choose,Node}, State) ->
         
        Client_name=element(1,Node),
        Client_node=element(2,Node),

        Array_list = ets:lookup(users, Client_name),
        Elem_tuple = element(1, Array_list),
        Users_tuple = element(1, Elem_tuple),
        
        Users_tuple_has_subscribed_to=maps:get(has_subscribed_to, Users_tuple),
        Users_tuple_has_subscribed_to= Users_tuple_has_subscribed_to ++ [Random_node_choose],
         
        State_random_has_subscribed_to = maps:update(has_subscribed_to, Users_tuple_has_subscribed_to, Users_tuple),
        Users_tuple = maps:merge(Users_tuple,State_random_has_subscribed_to),

        ets:delete(users,Client_name),
        ets:insert(users, {Client_name,Users_tuple}),

         {noreply,State};


handle_cast({got_retweet,Client_node_name,Name_of_user,Tweet,HashTag,Reference,Reference_node},State) ->


    %Change Tweets Table

    Process_map_tweets_table= #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil,reference_node=>nil},

    Random_tweet_1 = maps:update(tweet, Tweet, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, HashTag, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Reference_1),

    Reference_node_1 = maps:update(reference_node, Reference_node, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Reference_node_1),
          
          
    ets:insert(tweets,{Name_of_user,Process_map_tweets_table}),

       
        %Change only Retweets Table 

        Process_map_retweets_table = #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil,reference_node=>nil},

        Random_tweet_1 = maps:update(tweet, Tweet, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, HashTag, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,Reference_1),

    Reference_node_1 = maps:update(reference_node, Reference_node, Process_map_retweets_table),
    Process_map_retweets_table = maps:merge(Process_map_retweets_table,Reference_node_1),
          
        
    ets:insert(retweets,{Name_of_user,Process_map_retweets_table}),

        State_retweets = maps:update_with(number_of_retweets_after, fun(V) -> V+1 end, State),
        State = maps:merge(State,State_retweets),

        Client_name = Name_of_user,
        Client_node = Client_node_name,

        %Get its subscribed user and send the given retweet 
        Is_subscribed_by = get_a_list_of_is_subscribed_by_for_given_client(Client_name,Client_node,State),
        %TODO Enum.each(is_subscribed_by,fn({client_name_x,client_node_name_x,_}) -> GenServer.cast({client_name_x,client_node_name_x},{:retweet,tweet,hashTag,client_name_x,client_node_name_x,reference,reference_node,client_node_name,name_of_user})  end)

        {noreply,state};

handle_cast({got_mention_tweet,Client_node_name,Name_of_user,Tweet,HashTag,Reference,Reference_node},State) ->


    %Change Tweets Table

    Process_map_tweets_table = #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil,reference_node=>nil},

    Random_tweet_1 = maps:update(tweet, Tweet, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, HashTag, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Reference_1),

    Reference_node_1 = maps:update(reference_node, Reference_node, Process_map_tweets_table),
    Process_map_tweets_table = maps:merge(Process_map_tweets_table,Reference_node_1),

        
    ets:insert(tweets,{Name_of_user,Process_map_tweets_table}),


        %Change only user Mentioned Tweets 

    Process_map_user_mentioner_tweets_table = #{tweet => nil, hashTag => nil, name_of_user => nil, client_node_name => nil, reference => nil,reference_node=>nil},

    Random_tweet_1 = maps:update(tweet, Tweet, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,Random_tweet_1),

    HashTag_1 = maps:update(hashTag, HashTag, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,HashTag_1),
    
    Name_of_user_1 = maps:update(name_of_user, Name_of_user, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,Name_of_user_1),
    
    Client_node_name_1 = maps:update(client_node_name, Client_node_name, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,Client_node_name_1),

    Reference_1 = maps:update(reference, Reference, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,Reference_1),

    Reference_node_1 = maps:update(reference_node, Reference_node, Process_map_user_mentioner_tweets_table),
    Process_map_user_mentioner_tweets_table = maps:merge(Process_map_user_mentioner_tweets_table,Reference_node_1),
        
    ets:insert(user_mention_tweets,{Reference,Process_map_user_mentioner_tweets_table}),

    State_tweets = maps:update_with(number_of_tweets_after, fun(V) -> V+1 end, State),
    State = maps:merge(State,State_tweets),

    State_hashTag = maps:update_with(hashTag, fun(V) -> V++[hashTag] end, State),
    State = maps:merge(State,State_hashTag),
          

    Client_name = Name_of_user,
    Client_node = Client_node_name,

        %Get its subscribed user and send the given tweet
        Is_subscribed_by = get_a_list_of_is_subscribed_by_for_given_client(Client_name,Client_node,State),
        %TODO Enum.each(is_subscribed_by,fn({client_name_x,client_node_name_x,_}) -> GenServer.cast({client_name_x,client_node_name_x},{:got_a_tweet,tweet,hashTag,name_of_user,client_node_name,reference,client_name_x,client_node_name_x})  end)

        %Send the mention tweet to user to the reference
        gen_server:cast({Reference,Reference_node},{got_a_tweet_with_mention,Reference,Reference_node,Name_of_user,Client_node_name,Tweet,HashTag}),

        {noreply,State};

handle_cast({add_is_subscribed_foxr_given_client,Random_node_choose,Node},State) ->


         Client_name=element(random_node_choose,1),
         Client_node=element(random_node_choose,2),

        Array_list = ets:lookup(users, Client_name),
        Elem_tuple = element(1, Array_list),
        Users_tuple = element(1, Elem_tuple),
         
        Users_tuple_has_subscribed_by=maps:get(is_subscribed_by, Users_tuple),
        Users_tuple_has_subscribed_by= Users_tuple_is_subscribed_by ++ [Random_node_choose],

        users_tuple_number_of_subscribers=users_tuple[:number_of_subscribers]
        users_tuple_number_of_subscribers=users_tuple_number_of_subscribers+1;
         
         {_,state_random_is_subscribed_by}=Map.get_and_update(users_tuple,:is_subscribed_by, fn current_value -> {current_value,users_tuple_is_subscribed_to} end)
         users_tuple=Map.merge(users_tuple,state_random_is_subscribed_by)

         {_,state_number}=Map.get_and_update(users_tuple,:number_of_subscribers, fn current_value -> {current_value,users_tuple_number_of_subscribers} end)
         users_tuple=Map.merge(users_tuple,state_number)

         :ets.delete(:users, client_name)
         :ets.insert(:users, {client_name,users_tuple})

         {:noreply,state}
end

def handle_cast({:assign_hashTags_to_user,numHashTags,element}, state) do
         
        #process_map=%{:node_client => nil, :hashTags => [], :password => nil, :has_subscribed_to => [], :is_subscribed_by => [],:name_node => nil, :id => nil, :no_of_zipf_tweets =>0, :probability_of_zipf_functions=>0, :number_of_subscribers=>0 }


         client_name=elem(element,0)
         client_node=elem(element,1)

         array_list=:ets.lookup(:users, client_name)
         elem_tuple=Enum.at(array_list,0)
         users_tuple=elem(elem_tuple,1)
        #{:ok,%{:start_value=>1,:number_of_tweets_before=>0, :number_of_tweets_after=>0, :number_of_retweets_before=>0,:hashTag=>[]}}


         list_of_preferred_hashtags_for_user=Enum.take_random(state[:hashTag],numHashTags)

         users_tuple_hashTags=users_tuple[:hashTags]
         users_tuple_hashTags=users_tuple_hashTags++list_of_preferred_hashtags_for_user
         
         {_,state_random_hashTags}=Map.get_and_update(users_tuple,:hashTags, fn current_value -> {current_value,users_tuple_hashTags} end)
         users_tuple=Map.merge(users_tuple,state_random_hashTags)

         :ets.delete(:users, client_name)
         :ets.insert(:users, {client_name,users_tuple})
         {:noreply,state}

end


       

get_a_list_of_is_subscribed_by_for_given_client(Client_name,Client_node,State) ->
        Array_list = ets:lookup(users,Client_name),
        Elem_tuple = element(1, Array_list),
        Users_tuple = element(1, Elem_tuple),
        Is_subscribed_by = maps:get(is_subscribed_by, Users_tuple),
        Is_subscribed_by.