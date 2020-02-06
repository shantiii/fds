-module(fds_ft).

-export([new/0, push/2, pop/1, peek/1, drop/1, foldl/3]).
-export([count/1]).

%% Core Data Structure: Finger-Tree
%%
%% The finger tree is a double-ended sequence representation that allows for
%% efficient access at either end.
%%
%% Runtime Complexity Rundown
%%
%% O(1)
%% new()
%% push(Item, Seq)
%% pop(Seq)
%% peek(Seq)
%% drop(Seq)
%%
%% O(n)
%%  foldl(Fun, InitialAcc, Seq)
%%  count(Seq)

new() -> ft0.

%% Tail is stored reversed
%-type ft() :: ft0 | {ft1, any()} | {ft2, [any()], ft(), [any()]}.

push(NewElem, ft0) -> {ft1, NewElem};
push(NewElem, {ft1, OldElem})-> {ft2, [NewElem], ft0, [OldElem]};
push(NewElem, {ft2, Head, Inner, Tail}) when length(Head) < 4 -> {ft2, [NewElem | Head], Inner, Tail};
push(NewElem, {ft2, [A, B, C, D], Inner, Tail}) -> {ft2, [NewElem, A], push({B, C, D}, Inner), Tail}.

pop(ft0) -> error;
pop({ft1, Elem}) -> {ok, Elem, ft0};
pop({ft2, [First], ft0, [Second]}) -> {ok, Second, {ft1, First}};
pop({ft2, [A| Rest], ft0, [Second]}) -> {ok, Second, {ft2, [A], ft0, lists:reverse(Rest)}};
pop({ft2, Head, Inner, RTail=[THead|TTail]}) when length(RTail) > 1 -> {ok, THead, {ft2, Head, Inner, TTail}};
pop({ft2, Head, Inner, [Elem]}) ->
    Remainder =
    case Inner of
        ft0 ->
            case Head of
                [A] -> {ft1, A};
                [A| Rest] -> {ft2, [A], ft0, lists:reverse(Rest)}
            end;
        _NodeFt ->
            {ok, Node, NewInner} = pop(Inner),
            {ft2, Head, NewInner, lists:reverse(tuple_to_list(Node))}
    end,
    {ok, Elem, Remainder}.

peek(ft0) -> error;
peek({ft1, Elem}) -> {ok, Elem};
peek({ft2, _Head, _Inner, Tail}) -> {ok, hd(Tail)}.

drop(ft0) -> error;
drop(Ft) ->
    {ok, _Elem, NewFt} = pop(Ft),
    {ok, NewFt}.

%rpush()
%rpop()
%rpop(ft0) -> error;
%rpop({ft1, Elem}) -> {ok, Elem, ft0};
%rpeek()
%
%reverse()
%concat()

foldl(_Fun, Acc, ft0) -> Acc;
foldl(Fun, Acc, Ft) ->
    {ok, Elem, NewFt} = pop(Ft),
    foldl(Fun, Fun(Elem, Acc), NewFt).

%rfold()

reverse(Ft) -> reverse(Ft, 0).

reverse(ft0, _Fliplevel) -> ft0;
reverse({ft1, Elem}, Fliplevel) -> {ft1, nodeflip(Elem, Fliplevel)};
reverse({ft2, Head, Inner, Tail}, Fliplevel) ->
    NewHead = [nodeflip(E, Fliplevel) || E <- Tail],
    NewTail = [nodeflip(E, Fliplevel) || E <- Head],
    {ft2, NewHead, reverse(Inner, Fliplevel+1), NewTail}.

nodeflip(E, 0) -> E;
nodeflip({A, B, C}, Fliplevel) ->
    NewA = nodeflip(A, Fliplevel-1),
    NewB = nodeflip(B, Fliplevel-1),
    NewC = nodeflip(C, Fliplevel-1),
    {C, B, A}.

count(Ft) -> foldl(fun(_Elem, Acc) -> Acc+1 end, 0, Ft).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

qfold(_Fun, Acc, {[],[]}) -> Acc;
qfold(Fun, Acc, Queue) ->
    {{value, E}, NewQueue} = queue:out(Queue),
    qfold(Fun, Fun(E, Acc), NewQueue).


basic_test() ->
    L = lists:seq(1,1000),
    FT = lists:foldl(fun push/2, new(), L),
    L2 = foldl(fun(E,X)->[E|X]end, [], FT),
    ?assertMatch(L, lists:reverse(L2)).

queue_test() ->
    L = lists:seq(1,100000),
    F = fun() ->
                FT = lists:foldl(fun queue:in/2, queue:new(), L),
                L2 = qfold(fun(E,X)->[E|X]end, [], FT)
    end,
    {QTime, L2} = timer:tc(F),
    ?debugVal(QTime),
    ?assertMatch(L, lists:reverse(L2)).

speed_test() ->
    L = lists:seq(1,100000),
    F = fun() ->
                FT = lists:foldl(fun push/2, new(), L),
                L2 = foldl(fun(E,X)->[E|X]end, [], FT)
    end,
    {Time, L2} = timer:tc(F),
    ?debugVal(Time),
    ?assertMatch(L, lists:reverse(L2)).

-endif.
