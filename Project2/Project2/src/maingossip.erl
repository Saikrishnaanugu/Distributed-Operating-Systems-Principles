-module(maingossip).
-import('topology',[findNeighbours/2]).
-export([start/2,supervisor/1,killswitch/1,actor/2]).

start(NumProc, Topo) ->
    if %To round of the number of nodes if a 2D topology is chosens
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
    %creating the number of nodes as per input
    F = fun(X) -> spawn_link(fun() -> actor(Topo, 0) end) end,
    PList = [F(X) || X <- lists:seq(1,NumProc)],
    register(supervisor,spawn(maingossip,supervisor,[PList])).

supervisor(ProcList) ->
    GossipStartTime = statistics(wall_clock),
    io:fwrite("\nstart time is ~w", [GossipStartTime]),
    FirstProcessId = lists:nth(rand:uniform(length(ProcList)),ProcList),
    FirstProcessId ! {gossip,ProcList},
    case whereis(killswitch) of
        undefined ->
            register(killswitch,spawn(mainpushsum,killswitch,[ProcList]));
        _ -> 
            unregister(killswitch),
            register(killswitch,spawn(mainpushsum,killswitch,[ProcList]))
    end.

killswitch(PList) ->
    receive
        {From, finished} -> 
            UpdateList = lists:delete(From, PList),
            %io:fwrite("UpdateList ~w", [UpdateList]),
            if(UpdateList == []) ->
                {_, Time2}  = statistics(wall_clock),
                io:fwrite("\nconvergence time is ~w milliseconds~n",[Time2]),
                exit(From, ok);
            true ->
                killswitch(UpdateList)
            end
    end.

actor(Topology, Count)->
    receive
        {gossip,ProcList} ->   
            Nei_list = findNeighbours(Topology,ProcList),
            %io:fwrite("ProcList List ~w ~w ~w ", [self(),ProcList, Nei_list]),
            NextProcessId = lists:nth(rand:uniform(length(Nei_list)),Nei_list),
            %io:fwrite("The process ~w  sending to new pid ~w count ~w ~n", [self(), NextProcessId, Count]),
            if Count == 10 ->
                killswitch ! {self(), finished},
                %io:fwrite("\nmessage sent", []),
                NextProcessId ! {gossip,ProcList};
            true ->
                NextProcessId ! {gossip,ProcList}
            end    
    end,
actor(Topology, Count+1).




