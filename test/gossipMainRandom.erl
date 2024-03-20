-module(gossipMainRandom).
-export([startGossip/0, registerActors/2]).

startGossip()->
    {ok, N}=io:read("Enter number of actors you want in the network"),
    registerActors(N,N).

registerActors(0,M)->
    X=rand:uniform(M),
    list_to_atom("actor"++integer_to_list(X)) ! {M,gossip};
registerActors(N,M)->
    if
        N==0 ->
            ok;
        true ->
            Id=integer_to_list(N),
            register(list_to_atom("actor"++Id),spawn(gossipActorRandom,spreadGossip,[0,0])),
            registerActors(N-1,M)
    end.