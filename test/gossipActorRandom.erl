-module(gossipActorRandom).
-export([spreadGossip/2]).

spreadGossip(C,Y)->
            receive 
                {M,gossip}->
                    if
                        Y==0->
                            io:format("recieved gossip ~n"),
                            io:format("sending gossip ~w~n",[C]),
                            X=rand:uniform(M),
                            list_to_atom("actor"++integer_to_list(X)) ! {M,gossip},
                            if
                                C==10 ->
                                    spreadGossip(C,1);
                                true ->
                                spreadGossip(C+1,0) 
                            end;
                        true->
                            spreadGossip(C,1)
                    end
                end.