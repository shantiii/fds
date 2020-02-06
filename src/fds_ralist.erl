-module(fds_ralist).

-export([new/0, cons/2, head/1, tail/1, lookup/2, update/3]).
-export([foldl/3, count/1]).

%% Core Data Structure: Random-Access List
%%
%% The random-access list is a list (efficient push pop from one end) that also
%% allows for efficient random access and update of its elements.
%%
%% Runtime Complexity Rundown
%%
%% O(1)
%%  new()
%%  cons(Item, List)
%%  head(List)
%%  tail(List)
%%
%% O(log(n))
%%  lookup(Index, List)
%%  update(Index, Item, List)
%%  count(List)
%%
%% O(n)
%%  foldl(Fun, InitialAcc, List)
%%  count(List)

-type ratree(T) :: {T} | {T, ratree(T), ratree(T)}.
-type ranode(T) :: {pos_integer(), ratree(T)}.

-type ralist(T) :: [ranode(T)].

%% TODO:
%% * implement map
%% *  make foldl more efficient by using pattern matching
%% * replace tuple of one element with just a single element for the Leaf case

-spec new() -> ralist(term()).
new() -> [].

-spec cons(T, ralist(T)) -> ralist(T).
cons(Elem, [{Weight, T1}, {Weight, T2} | Rest]) ->
    [{ Weight*2+1, {Elem, T1, T2} }| Rest];
cons(Elem, RAList) ->
    [{1, {Elem}} | RAList].

-spec head(ralist(T)) -> T.
head([]) ->
    throw(badarg);
head([{1, {Elem}} | _Rest]) ->
    Elem;
head([{_Weight, {Elem,_Tree1,_Tree2}} | _Rest]) ->
    Elem.

-spec tail(ralist(T)) -> ralist(T).
tail([]) ->
    throw(badarg);
tail([{1, _Tree} | Rest]) ->
    Rest;
tail([{Weight, {_Elem, Tree1, Tree2}} | Rest]) ->
    [{Weight div 2, Tree1}, {Weight div 2, Tree2} | Rest].

-spec lookup(non_neg_integer(), ralist(T)) -> T.
lookup(_Index, []) ->
    erlang:error(badarg);
lookup(Index, [{Weight, Tree} | _Rest]) when Index < Weight ->
    lookup_tree(Index, Weight, Tree);
lookup(Index, [{Weight, _Tree} | Rest]) ->
    lookup(Index - Weight, Rest).

lookup_tree(0, 1, {Elem}) ->
    Elem;
lookup_tree(0, _Weight, {Elem, _Tree1, _Tree2}) ->
    Elem;
lookup_tree(Index, Weight, {_Elem, Tree1, _Tree2}) when Index - 1 < Weight div 2 ->
    lookup_tree(Index - 1, Weight div 2, Tree1);
lookup_tree(Index, Weight, {_Elem, _Tree1, Tree2}) ->
    lookup_tree(Index - 1 - Weight div 2, Weight div 2, Tree2).

-spec update(non_neg_integer(), T, ralist(T)) -> ralist(T).
update(_Index, _Elem, []) ->
    erlang:error(badarg);
update(Index, Elem, [{Weight, Tree} | Rest]) when Index < Weight ->
    [{Weight, update_tree(Index, Elem, Weight, Tree)} | Rest];
update(Index, Elem, [{Weight, Tree} | Rest]) ->
    [{Weight, Tree} | update(Index - Weight, Elem, Rest)].

update_tree(0, NewElem, 1, {_OldElem}) ->
    {NewElem};
update_tree(0, NewElem, _Weight, {_OldElem, Tree1, Tree2}) ->
    {NewElem, Tree1, Tree2};
update_tree(Index, NewElem, Weight, {Elem, Tree1, Tree2}) when Index - 1 < Weight div 2 ->
    {Elem, update_tree(Index - 1, NewElem, Weight div 2, Tree1), Tree2};
update_tree(Index, NewElem, Weight, {Elem, Tree1, Tree2}) ->
    {Elem, Tree1, update_tree(Index - 1 - Weight div 2, NewElem, Weight div 2, Tree2)}.

-spec foldl(fun((T, term()) -> term()), term(), ralist(T)) -> term().
foldl(_Fun, Acc, []) ->
    Acc;
foldl(Fun, Acc, RAList) ->
    foldl(Fun, Fun(head(RAList), Acc), tail(RAList)).

-spec count(ralist(term())) -> non_neg_integer().
count([]) ->
    0;
count([{Weight, _Tree} | Rest]) ->
    Weight + count(Rest).

% Tree = {Elem} OR {Elem, LTree, RTree}
% RAList = [{Count, Tree}]

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ?assertMatch([], new()).

speed_test() ->
    % compare to list
    L = lists:seq(1,100),
    F = fun() ->
                FT = lists:foldl(fun cons/2, new(), L),
                L2 = foldl(fun(E,X)->[E|X]end, [], FT)
    end,
    {Time, L2} = timer:tc(F),
    ?debugVal(Time),
    ?assertMatch(L, L2).

lol_test() ->
    DataSize = 1000,
    QueryCount = 10000,
    % compare to list
    L = lists:seq(1,DataSize),
    RA = lists:foldl(fun cons/2, new(), L),

    Times = fun
        Foo(0, F) -> ok;
        Foo(X, F) -> F(), Foo(X-1, F)
    end,

    F1 = fun () -> lists:nth(rand:uniform(DataSize), L) end,
    F2 = fun () -> lookup(rand:uniform(DataSize)-1, RA) end,

    {ListTime, _} = timer:tc(Times, [QueryCount, F1]),
    {RATime, _} = timer:tc(Times, [QueryCount, F2]),
    ?debugVal(ListTime),
    ?debugVal(RATime).

-endif.
