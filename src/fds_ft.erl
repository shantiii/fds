-module(fds_ft).

-compile(inline).
-compile(inline_list_funcs).
%-compile({inline_size, 1000}).

-export([new/0, push/2, pop/1, peek/1, drop/1, foldl/3]).
-export([rpush/2, rpop/1, rpeek/1, rdrop/1, foldr/3]).
-export([from_list/1, to_list/1, count/1]).
-export([reverse/1, concat/2]).

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
%%


%%  a finger tree of type t() requires understanding a few basic types:
%%  empty tree (the atom ft0)
%%  monoid tree (the tuple { Term})
%%  deep tree (the tuple { Head, Inner, RTail})
%%  	Head is a list of t() between 1 and 4 elements long. It cannot be empty, nor longer than 4 digits
%%      RTail is the same, on the other end of the structure, but stored in reverse (to make peek() faster)
%%      Inner is a fingertree of Nodes of t(). A node is a tuple or triple of t().

-type t(T) :: ft0 | { T} | { [T], any(), [T]}.

new() -> ft0.

%% Tail is stored reversed
%-type ft() :: ft0 | { any()} | { [any()], ft(), [any()]}.

% @doc Insert element into tail (right side) of queue.
push(NewElem, ft0) ->
    { NewElem};
push(NewElem, { OldElem}) ->
    { [OldElem], ft0, [NewElem]};
push(NewElem, { Head, Inner, [Last, T1, T2, T3]}) ->
    { Head, push(nodeify(T3, T2, T1), Inner), [NewElem, Last]};
push(NewElem, { Head, Inner, [_|_]=RTail}) ->
    { Head, Inner, [NewElem|RTail]}.

%% @doc Insert element into head (left side) of queue.
rpush(NewElem, ft0) -> { NewElem};
rpush(NewElem, { OldElem})-> { [NewElem], ft0, [OldElem]};
rpush(NewElem, { Head, Inner, Tail}) when length(Head) < 4 -> { [NewElem | Head], Inner, Tail};
rpush(NewElem, { [A, B, C, D], Inner, Tail}) -> { [NewElem, A], push({B, C, D}, Inner), Tail}.

%% @doc Remove element from head (left side) of queue, and return both.
pop(ft0) ->
    error;
pop({ Elem}) ->
    {ok, Elem, ft0};
pop({ [First], ft0, [Second]}) ->
    {ok, First, { Second}};
pop({ [First], ft0, [Last, Second]}) ->
    {ok, First, { [Second], ft0, [Last]}};
pop({ [First], ft0, [Last, Third, Second]}) ->
    {ok, First, { [Second], ft0, [Last, Third]}};
pop({ [First], ft0, [Last, Fourth, Third, Second]}) ->
    {ok, First, { [Second], ft0, [Last, Fourth, Third]}};
pop({ [First], Inner, RTail}) ->
    {ok, Node, NewInner} = pop(Inner),
    {ok, First, { unnodeify(Node), NewInner, RTail}};
pop({ [First|Rest], Inner, RTail}) ->
    {ok, First, { Rest, Inner, RTail}}.

%% @doc Remove element from tail (right side) of queue, and return both
rpop(ft0) ->
    error;
rpop({ Elem}) ->
    {ok, Elem, ft0};
rpop({ [First], ft0, [Last]}) ->
    {ok, Last, { First}};
rpop({ [First, Second], ft0, [Last]}) ->
    {ok, Last, { [First], ft0, [Second]}};
rpop({ [First, Second, Third], ft0, [Last]}) ->
    {ok, Last, { [First], ft0, [Third, Second]}};
rpop({ [First, Second, Third, Fourth], ft0, [Last]}) ->
    {ok, Last, { [First], ft0, [Fourth, Third, Second]}};
rpop({ Head, Inner, [Last]}) ->
    {ok, Node, NewInner} = rpop(Inner),
    {ok, Last, { Head, NewInner, runnodeify(Node)}};
rpop({ Head, Inner, [Last|Rest]}) ->
    {ok, Last, { Head, Inner, Rest}}.


%% @doc Peek at the first (left-most) element

-spec peek(t(T)) -> error | {ok, T}.
peek(ft0) -> error;
peek({ Elem}) -> {ok, Elem};
peek({ [Elem|_Rest], _Inner, _RTail}) -> {ok, Elem}.

%% @doc Peek at the last (right-most) element

-spec rpeek(t(T)) -> error | {ok, T}.
rpeek(ft0) -> error;
rpeek({ Elem}) -> {ok, Elem};
rpeek({ _Head, _Inner, [Elem,_Rest]}) -> {ok, Elem}.

%% @doc Remove element at head (left side) of queue
-spec drop(t(T)) -> error | {ok, t(T)}.
drop(ft0) -> error;
drop(Ft) ->
    {ok, _Elem, NewFt} = pop(Ft),
    {ok, NewFt}.

%% @doc Remove element at tail (right side) of queue
-spec rdrop(t(T)) -> error | {ok, t(T)}.
rdrop(ft0) -> error;
rdrop(Ft) ->
    {ok, _Elem, NewFt} = rpop(Ft),
    {ok, NewFt}.


nodeify(A, B) -> {A, B}.
nodeify(A, B, C) -> {A, B, C}.
unnodeify({A, B}) -> [A, B];
unnodeify({A, B, C}) -> [A, B, C].
runnodeify({A, B}) -> [B, A];
runnodeify({A, B, C}) -> [C, B, A].

%% @doc pop the left side value
%rpop(ft0) -> error;
%rpop({ Elem}) -> {ok, Elem, ft0};
%rpop({ [Elem], Inner, RTail}) -> 
%	Remainder =
%	case Inner of
%		ft0 ->
%			ok;
%		NodeFt ->
%			{ok, Node, NewInner} = rpop(NodeFt),
%			{ tuple_to_list(Node), NewInner, RTail}
%	end
%
%	{ok, Elem, NewFt};
%rpop({ [Elem, Next|Rest], Inner, RTail}) -> {ok, Elem, { [Next|Rest], Inner, RTail}};
%
%rdrop(ft0) -> error;
%rdrop(Ft) ->
%	{ok, _Elem, NewFt} = rpop(Ft),
%	{ok, NewFt}.

%reverse()
%concat()

foldl(_Fun, Acc, ft0) -> Acc;
foldl(Fun, Acc, Ft) ->
    {ok, Elem, NewFt} = pop(Ft),
    foldl(Fun, Fun(Elem, Acc), NewFt).

foldr(_Fun, Acc, ft0) -> Acc;
foldr(Fun, Acc, Ft) ->
    {ok, Elem, NewFt} = rpop(Ft),
    foldr(Fun, Fun(Elem, Acc), NewFt).

%rfold()

count(Ft) -> count(Ft, 0).

count(ft0, _N) ->
    0;
count({Single}, N) ->
    nodecount(Single, N);
count({Head, Inner, RTail}, N) ->
    ListCount = fun(E, Sum) -> Sum + nodecount(E, N) end,
    lists:foldl(ListCount, 0, Head) + 
    lists:foldl(ListCount, 0, RTail) + 
    + count(Inner, N+1).

nodecount(_, 0) -> 1;
nodecount({A, B}, 1) -> 2;
nodecount({A, B, C}, 1) -> 3;
nodecount({A, B}, N) -> nodecount(A, N-1) + nodecount(B, N-1);
nodecount({A, B, C}, N) -> nodecount(A, N-1) + nodecount(B, N-1) + nodecount(C, N-1).


to_list(Ft) -> foldr(fun(Elem, Acc) -> [Elem|Acc] end, [], Ft).

reverse(Ft) -> reverse(Ft, 0).

reverse(ft0, _Fliplevel) -> ft0;
reverse({Elem}, Fliplevel) -> { nodeflip(Elem, Fliplevel)};
reverse({Head, Inner, Tail}, Fliplevel) ->
    NewHead = [nodeflip(E, Fliplevel) || E <- Tail],
    NewTail = [nodeflip(E, Fliplevel) || E <- Head],
    { NewHead, reverse(Inner, Fliplevel+1), NewTail}.


nodeflip(E, 0) -> E;
nodeflip({A, B}, Fliplevel) ->
    {nodeflip(B, Fliplevel-1), nodeflip(A, Fliplevel-1)};
nodeflip({A, B, C}, Fliplevel) ->
    {nodeflip(C, Fliplevel-1), nodeflip(B, Fliplevel-1), nodeflip(A, Fliplevel-1)}.

concat(Ft1, Ft2)->
    concat_with_middle(Ft1, [], Ft2).

concat_with_middle(ft0, [], Ft) ->
    Ft;
concat_with_middle(Ft, [], ft0) ->
    Ft;
concat_with_middle(ft0, [Node1], Ft) ->
    concat_with_middle(ft0, [], rpush(Node1, Ft));
concat_with_middle(ft0, [Node1,Node2], Ft) ->
    concat_with_middle(ft0, [], rpush(Node1, rpush(Node2, Ft)));
concat_with_middle(ft0, [Node1,Node2,Node3], Ft) ->
    concat_with_middle(ft0, [], rpush(Node1, rpush(Node2, rpush(Node3, Ft))));
concat_with_middle(Ft, [Node1|Rest], ft0) ->
    concat_with_middle(push(Node1, Ft), Rest, ft0);
concat_with_middle({ Head1, Inner1, RTail1}, Middle, { Head2, Inner2, RTail2}) ->
    Nodes = mknod(lists:reverse(RTail1) ++ Middle ++ Head2),
    { Head1, concat_with_middle(Inner1, Nodes, Inner2), RTail2}.

mknod([]) -> [];
mknod([A,B]) -> [{A,B}];
mknod([A,B,C]) -> [{A,B,C}];
mknod([A,B,C,D]) -> [{A,B},{C,D}];
mknod([A,B,C|Rest]) -> [{A,B,C}|mknod(Rest)].


from_list(List) -> lists:foldl(fun push/2, new(), List).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

qfold(_Fun, Acc, {[],[]}) -> Acc;
qfold(Fun, Acc, Queue) ->
    {{value, E}, NewQueue} = queue:out(Queue),
    qfold(Fun, Fun(E, Acc), NewQueue).

basic_test() ->
    L = lists:seq(1,1000),
    FT = lists:foldl(fun push/2, new(), L),
    Q = lists:foldl(fun queue:in/2, queue:new(), L),
    FTlist = to_list(FT),
    Qlist = queue:to_list(Q),
    ?assertEqual(Qlist, FTlist).

queue_test() ->
    L = lists:seq(1,100000),
    F = fun() ->
                Queue = lists:foldl(fun queue:in/2, queue:new(), L)
        end,
    {QTime, L2} = timer:tc(F),
    ?debugVal(QTime).

speed_test() ->
    L = lists:seq(1,100000),
    F = fun() ->
                FT = from_list(L)
        end,
    {Time, L2} = timer:tc(F),
    ?debugVal(Time).

-endif.
