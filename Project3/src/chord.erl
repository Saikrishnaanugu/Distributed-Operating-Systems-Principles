-module(chord).
-export([start/2,chordnetwork/2,node/4,supervisor/2]).

randomNode(Node_id, []) -> Node_id;
randomNode(_, ExistingNodes) -> lists:nth(rand:uniform(length(ExistingNodes)), ExistingNodes).

start(NumOfNodes, NumOfRequest) ->
    register(server, spawn(chord, chordnetwork, [NumOfNodes, NumOfRequest])).

chordnetwork(NumOfNodes, NumOfRequest) ->
    M = calc_M(NumOfNodes),
    [NodeList, State] = create([], round(math:pow(2,M)), M, NumOfNodes, dict:new()),
    
    create_fingerTable(State,M),
    
    stabilize(NodeList, State),

    register(supervisor, spawn(chord, supervisor, [NumOfNodes * NumOfRequest, 0])),

    lookup_request(NodeList, NumOfRequest, M, State),

    TotalHops = getHopCount(),
    AvgHops = TotalHops/(NumOfNodes * NumOfRequest),
    io:format("\nAvg Hops = ~p ~n", [AvgHops]),
    killswitch(NodeList, State).

create_fingerTable(State,M) ->
    FingerTables = get_fingerTable(State, dict:to_list(State), dict:new(),M),
    send_fingerTable(dict:fetch_keys(FingerTables), State, FingerTables).

getLength(Key, Key, _, Length) ->
    Length;
getLength(Key, NodeId, M, Length) ->
    getLength(Key, (NodeId + 1) rem trunc(math:pow(2, M)), M, Length + 1).



successor_node(Key, NodeList, State) ->
    case lists:member(Key, NodeList) of
        true -> Key;
        _ -> immediateSuccessor(Key, NodeList, -1, 10000000, State)
    end.

immediateSuccessor(_, [], SmallestNode, _, _) ->
    SmallestNode;
immediateSuccessor(Key, NodeList, SmallestNode, Value, State) ->
    [First| Tail] = NodeList,
    Length = getLength(Key, First, dict:fetch(m, State), 0),
    if
        Length < Value ->
            immediateSuccessor(Key, Tail, First, Length, State);
        true -> 
            immediateSuccessor(Key, Tail, SmallestNode, Value, State)
    end.

node_listen(NodeState) ->
    Hash = dict:fetch(id, NodeState),
    receive
        {fingertable, FingerTable} -> 
            UpdatedState = dict:store(fingertable, FingerTable, NodeState);
        {lookup, Id, Key, HopsCount, Pid} ->
            NodeValue = successor_node(Key, dict:fetch_keys(dict:fetch(fingertable ,NodeState)), NodeState),
            UpdatedState = NodeState,
            if     
                (Hash == Key) -> 
                    supervisor ! {completed, Hash, HopsCount, Key};
                (NodeValue == Key) and (Hash =/= Key) -> 
                    supervisor ! {completed, Hash, HopsCount, Key};    
            true ->
                dict:fetch(NodeValue, dict:fetch(fingertable, NodeState)) ! {lookup, Id, Key, HopsCount + 1, self()}
                end;
        {kill} ->
            UpdatedState = NodeState,
            exit("received exit signal");
        {state, Pid} -> Pid ! NodeState,
                        UpdatedState = NodeState;
        {stabilize, State} -> 
                        UpdatedState = NodeState
    end, 
    node_listen(UpdatedState).

node(Hash, M, NodeList, NodeState) -> 
    FingerTable = lists:duplicate(M, randomNode(Hash, NodeList)),
    NodeStateUpdated = dict:from_list([{id, Hash}, {predecessor, nil}, {fingertable, FingerTable}, {next, 0}, {m, M}]),
    node_listen(NodeStateUpdated).

calc_M(NumOfNodes) ->
    trunc(math:ceil(math:log2(NumOfNodes))).

get_Pid(Hash, State) -> 
    case dict:find(Hash, State) of
        error -> nil;
        _ -> dict:fetch(Hash, State)
    end.

join_node(NodeList, TotalNodes, M, State) ->
    RemainingHashes = lists:seq(0, TotalNodes - 1, 1) -- NodeList,
    RandomValue = lists:nth(rand:uniform(length(RemainingHashes)), RemainingHashes),
    Pid = spawn(chord, node, [RandomValue, M, NodeList, dict:new()]),
    Hashed_Pid = binary:decode_unsigned(crypto:hash(sha256,pid_to_list(Pid))) rem round(math:pow(2,M)), 
    %io:format("~p ~n", [Hashed_Pid]),
    [Hashed_Pid, dict:store(Hashed_Pid, Pid, State)].

supervisor(0, HopsCount) ->
    server ! {totalhops, HopsCount};

supervisor(NumOfRequests, HopsCount) ->
    receive 
        {completed, Pid, HopCount, Key} ->
            %io:format("Node ID ~p completed request with ~p Hops ~n", [Pid, HopCount]),
            supervisor(NumOfRequests - 1, HopsCount + HopCount)
    end.

lookup_node(Key, NodeList, State) ->
    if
        NodeList == [] ->
            {done};
    true ->
        [First | Tail] = NodeList,
        Pid = get_Pid(First, State),
        Pid ! {lookup, First, Key, 0, self()},
        lookup_node(Key, Tail, State)
    end.


lookup_request(_, 0, _, _) ->
    ok;
lookup_request(NodeList, NumOfRequest, M, State) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(NodeList)), NodeList),
    lookup_node(Key, NodeList, State),
    lookup_request(NodeList, NumOfRequest - 1, M, State).

killswitch(NodeList, State) -> 
    if
        NodeList == [] ->
            {done};
    true -> 
        [First | Tail] = NodeList,
        get_Pid(First, State) ! {kill},
        killswitch(Tail, State)
    end.

getHopCount() ->
    receive
        {totalhops, HopsCount} ->
            HopsCount
        end.

stabilize(NodeList, State) ->
    Pid = get_Pid(lists:nth(rand:uniform(length(NodeList)), NodeList), State),

    case Pid of
        nil -> stabilize(NodeList, State);
        _ -> Pid ! {stabilize, State}
    end.

create(NodeList, _, _, 0, State) -> 
    [NodeList, State];
create(NodeList, TotalNodes, M, NumOfNodes, State) ->
    [Hash, NewState] = join_node(NodeList, TotalNodes,  M, State),
    create(lists:append(NodeList, [Hash]), TotalNodes, M, NumOfNodes-1, NewState).


successor(_, _, Index , Index, NodeId, M) ->
    NodeId;

successor(Hash, State, Index, Current, NodeId, M) -> 
    case dict:find((NodeId + 1) rem trunc(math:pow(2, M)), State) of
        error ->
             successor(Hash, State, Index, Current, (NodeId + 1) rem trunc(math:pow(2, M)),M);
        _ -> successor(Hash, State, Index, Current + 1, (NodeId + 1) rem trunc(math:pow(2, M)),M)
    end.

get_fingerTable(State, List, Dict,M) ->
    if
        List == [] ->
            Dict;
    true ->
        [First | Tail] = List,
        FingerTables = get_finger_table(First, State,M, 0,[]),
        get_fingerTable(State, Tail, dict:store(element(1, First), FingerTables, Dict), M)
    end.

get_finger_table(_, _, M, M,FingerTable) ->
    FingerTable;
get_finger_table(Node, State, M, I, FingerTable) ->
    Hash = element(1, Node),
    Successor = successor(Hash, State, trunc(math:pow(2, I)), 0, Hash, M),
    get_finger_table(Node, State, M, I + 1, FingerTable ++ [{Successor, dict:fetch(Successor, State)}]).

send_fingerTable(NodeList, State, FingerTables) ->
    if
        NodeList == [] ->
            {done};
    true ->
        [First|Tail] = NodeList,
        Pid = dict:fetch(First ,State),
        Pid ! {fingertable, dict:from_list(dict:fetch(First, FingerTables))},
        send_fingerTable(Tail, State, FingerTables)
    end.





