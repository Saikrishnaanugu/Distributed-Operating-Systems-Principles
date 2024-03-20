-module(gossipActorTwo).
-export([spreadGossip/3`]).

spreadGossip(N,C,Y)->
    receive 
        {M,gossip}->
            if
                Y==0 ->
                    io:format("recieved gossip ~n"),
                    io:format("sending gossip ~w~n",[C]),
                    if
                        N==1 ->
                            list_to_atom("actor"++integer_to_list(N+1)) ! {M,gossip}; 
                        true ->
                            if
                                N==M ->
                                    list_to_atom("actor"++integer_to_list(N-1)) ! {M,gossip};
                                true ->
                                    list_to_atom("actor"++integer_to_list(N+1)) ! {M,gossip}, 
                                    list_to_atom("actor"++integer_to_list(N-1)) ! {M,gossip}
                            end  
                    end,
                    if
                        C<10 ->
                            spreadGossip(N,C+1,0);
                        true ->
                            spreadGossip(N,C,1) 
                    end;
                true->
                    spreadGossip(N,C,1)
            end
        end.