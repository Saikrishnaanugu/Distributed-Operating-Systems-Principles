-module(mainpushsum).
-import('topology',[findNeighbours/2,delete/1]).
-export([start/2,supervisor/1,killswitch/1,pushsumspreader/4]).

start(NumProc, Topo) ->
    if%To round of the number of nodes if a 2D topology is chosen
        Topo == "2dgrid" ->
            UpdateNum = round(math:pow(round(math:sqrt(NumProc)), 2)),
            io:fwrite("\nNum of Nodes rounded off to ~w", [UpdateNum]),
            initiate(UpdateNum, Topo); 
        Topo == "imp2d" ->
            UpdateNum = round(math:pow(round(math:sqrt(NumProc)), 2)),
            io:fwrite("\nNum of Nodes rounded off to ~w", [UpdateNum]),
            initiate(UpdateNum, Topo); 
    true ->
        initiate(NumProc, Topo)

    end.
    
initiate(NumProc, Topo) ->
    F = fun(X) -> spawn_link(fun() -> pushsumspreader(X, 1, 0, Topo) end) end,
    PList = [F(X) || X <- lists:seq(1,NumProc)],
    spawn(mainpushsum,supervisor,[PList]).


supervisor(PList) ->
    GossipStartTime = statistics(wall_clock),
    io:fwrite("\nstart time is ~w", [GossipStartTime]),
    FirstProcessId = lists:nth(rand:uniform(length(PList)),PList),
    FirstProcessId ! {gossip,PList},
    case whereis(killswitch) of
        undefined ->
            register(killswitch,spawn(mainpushsum,killswitch,[PList]));
        _ -> 
            unregister(killswitch),
            register(killswitch,spawn(mainpushsum,killswitch,[PList]))
    end.

killswitch(PList) ->
    receive
        {From, finished} -> 
            UpdateList = lists:delete(From, PList),
            if(UpdateList == []) ->
                {_, Time2}  = statistics(wall_clock),
                io:fwrite("\nconvergence time is ~w milliseconds~n",[Time2]),
                exit(From, ok);
            true ->
                killswitch(UpdateList)
            end
    end.

%pushsumspreader(_,_,40,_) ->
%   self() ! {finished};
    
pushsumspreader(S, W, Count, Topology) ->      
    receive
        {Si, Wi, PList} -> %If Si,Wi is received , add to current S,W and half it. Send new value to next actor based on topology
            Nei_list = findNeighbours(Topology,PList),       
            
            TotalS = Si + S,
            TotalW = Wi + W,
            %io:fwrite("Cur: ~w NRatio: ~w ORatio: ~w PList: ~w Nei_List: ~w ~n", [self(),(TotalS/TotalW), (S/W),PList, Nei_list]),

            if
                (abs((TotalS/TotalW) - (S/W)) < (0.0000000001)) ->
                    UpdateCount = Count + 1;
                true ->
                    UpdateCount = Count
            end,

            NextProcessId = lists:nth(rand:uniform(length(Nei_list)),Nei_list),
             %io:fwrite("\nThe process ~w yet to receive ~w gossips , sending to new pid ~w", [self(),Count, NextProcessId]),
             %io:fwrite("The process ~w sending to new pid ~w count ~w ~n", [self(), NextProcessId, UpdateCount]),
             %Sends half of new S and W to PID
            if 
                UpdateCount == 3 ->
                killswitch ! {self(), finished},
                NextProcessId ! {TotalS/2, TotalW/2, PList};
            true ->
                NextProcessId ! {TotalS/2, TotalW/2, PList}
            end;

        {gossip, PList} ->
            Nei_list = findNeighbours(Topology,PList),
            %Sends half of new S and W to PID
            TotalS =  S,
            TotalW =  W,
            UpdateCount = Count,

            NextProcessId = lists:nth(rand:uniform(length(Nei_list)),Nei_list),
            %io:fwrite("First Neighbor ~w ~w ~w ~w ~n", [self(),NextProcessId,PList, Nei_list]),
            NextProcessId ! {TotalS/2, TotalW/2, PList}
    end,
    pushsumspreader(TotalS/2, TotalW/2, UpdateCount, Topology).






