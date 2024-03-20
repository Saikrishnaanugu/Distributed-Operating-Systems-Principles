-module('topology').
-export([findNeighbours/2]).

findNeighbours(TopologyName, ProcList) ->

    ListSize = length(ProcList),

    case TopologyName of
        "full" ->
            lists:delete(self(),ProcList);
        "line" -> 
            Length = length(ProcList),
            if Length == 1 -> [];
            true -> 
            FirstProcess = lists:nth(1, ProcList),
            LastProcess = lists:nth(length(ProcList), ProcList),
                if
                    FirstProcess == self() ->
                        [lists:nth(2,ProcList)];
                    LastProcess == self() ->
                        [lists:nth(Length-1, ProcList)];
                
                true ->
                    Index = string:str(ProcList,[self()]),
                    [lists:nth(Index-1,ProcList), lists:nth(Index+1, ProcList)]
                end
            end;
        "2dgrid" -> %Based on List , impose 2D behaviour so as to determine the neighbors
            N = round(math:sqrt(ListSize)),
            Index = string:str(ProcList,[self()]),

            %Bottom Neighbor
            if
                Index + N > ListSize ->
                    BNeighbor = [];
            true ->
                BNeighbor = [lists:nth(Index+N, ProcList)]
            end,

            %Top Neighbor
            if
                Index - N < 1 ->
                    TNeighbor = [];
            true ->
                TNeighbor = [lists:nth(Index-N, ProcList)]
            end,
            %Right Neighbor
            if
                Index rem N == 0 ->
                    RNeighbor = [];
            true ->
                RNeighbor = [lists:nth(Index+1, ProcList)]
            end,
            %Left Neighbor
            if
                Index rem N == 1 ->
                    LNeighbor = [];
            true ->
                LNeighbor = [lists:nth(Index-1, ProcList)]
            end,

            BNeighbor ++ TNeighbor ++ RNeighbor ++ LNeighbor;

        "imp2d" ->
            N = round(math:sqrt(ListSize)),
            Index = string:str(ProcList,[self()]),

            %Bottom Neighbor
            if
                Index + N > ListSize ->
                    BNeighbor = [];
            true ->
                BNeighbor = [lists:nth(Index+N, ProcList)]
            end,

            %Top Neighbor
            if
                Index - N < 1 ->
                    TNeighbor = [];
            true ->
                TNeighbor = [lists:nth(Index-N, ProcList)]
            end,
            %Right Neighbor
            if
                Index rem N == 0 ->
                    RNeighbor = [];
            true ->
                RNeighbor = [lists:nth(Index+1, ProcList)]
            end,
            %Left Neighbor
            if
                Index rem N == 1 ->
                    LNeighbor = [];
            true ->
                LNeighbor = [lists:nth(Index-1, ProcList)]
            end,

            RandomList = ProcList -- [BNeighbor ++ TNeighbor ++ RNeighbor ++ LNeighbor],
            BNeighbor ++ TNeighbor ++ RNeighbor ++ LNeighbor ++ [lists:nth(rand:uniform(length(RandomList)),RandomList)]
    end.
