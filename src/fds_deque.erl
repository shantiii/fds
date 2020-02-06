-module(fds_deque).

new() -> {fds_ft:new(), 0}.

in(E,{Ft, Count}) -> {fds_ft:push(E, Ft), Count+1}.

out({_Ft, 0}) -> error;
out({Ft, Count}) ->
    {ok, E, NewFt} = fds_ft:pop(Ft),
    {E, {NewFt, Count-1}}.

count({_Ft, Count}) -> Count.
