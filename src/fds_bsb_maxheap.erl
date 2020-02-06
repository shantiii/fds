-module(fds_bsb_maxheap).

%% Core Data Structure: Bootstrapped Skew-Binomial Max-Heap
%%
%% This heap is a heap that allows for efficient insertion, merging, as well as
%% peeking and popping of the largest (by Erlang term order) element.
%%
%% Notes:
%%  This is largely the same as the min heap, just with one comparison reversed.
%%
%%  Credits to Chris Okasaki for a phenomenal book.
%%
%%  TODO make this actually operate on the minimum elements

-export([new/0, insert/2, peek_min/1, delete_min/1, merge/2]).

-record(tree, {root, rank=0, elem_rest=[], rest=[]}).
-record(bsheap, {root, hheap=[]}).

%% Public Interface

-spec new() -> any().
new() -> bs_new().

merge(H1, H2) -> bs_merge(H1,H2).

insert(E, H) -> bs_insert(E, H).

-spec peek_min(any()) -> any().
peek_min(H) -> bs_lookup_min(H).

delete_min(H) -> bs_delete_min(H).

%% Bootstrapped Heap Interface

bs_new() -> empty.

bs_merge(empty, H=#bsheap{}) -> H;
bs_merge(H=#bsheap{},empty) -> H;
bs_merge(H1=#bsheap{root=X,hheap=SBHeap},H2=#bsheap{root=Y}) when X =< Y ->
  H1#bsheap{hheap=sb_insert(H2,SBHeap)};
bs_merge(H1=#bsheap{},H2=#bsheap{}) ->
  bs_merge(H2,H1).

bs_insert(E, H) -> bs_merge(#bsheap{root=E},H).

bs_lookup_min(empty) -> error;
bs_lookup_min(#bsheap{root=X}) -> {ok,X}.

bs_delete_min(empty) -> error;
bs_delete_min(#bsheap{hheap=[]}) -> {ok,empty};
bs_delete_min(#bsheap{hheap=SBHeap}) ->
  {ok,#bsheap{root=Root,hheap=SB1}}=sb_lookup_min(SBHeap),
  {ok,SB2}=sb_delete_min(SBHeap),
  {ok,#bsheap{root=Root,hheap=sb_merge(SB1,SB2)}}.

%% Skew Binomial Heap Interface

sb_insert(E, [T1,T2|TRest]) when T1#tree.rank =:= T2#tree.rank ->
  [skew_link(E, T1, T2)|TRest];
sb_insert(E, Trees) ->
  [#tree{root=E}|Trees].

sb_lookup_min(Trees) -> do_lookup_min(Trees).

sb_merge(T1s, T2s) -> merge_trees(normalize(T1s),normalize(T2s)).

merge_trees([], Ts) -> Ts;
merge_trees(Ts, []) -> Ts;
merge_trees([T1|T1s], [T2|T2s]) when T1#tree.rank =:= T2#tree.rank ->
  ins_tree(sbt_link(T1,T2),merge_trees(T1s, T2s));
merge_trees([T1|T1s], [T2|T2s]) when T1#tree.rank < T2#tree.rank ->
  [T1|merge_trees(T1s, [T2|T2s])];
merge_trees([T1|T1s], [T2|T2s]) ->
  [T2|merge_trees([T1|T1s], T2s)].

normalize([]) -> [];
normalize([T|Ts]) -> ins_tree(T, Ts).

sb_delete_min([]) -> error;
sb_delete_min(Trees) ->
  {#tree{elem_rest=Xs,rest=C},Ts}=tree_get_min(Trees),
  M=merge_trees(lists:reverse(C),normalize(Ts)),
  {ok,lists:foldl(fun sb_insert/2, M, Xs)}.

%% Skew Binomial Tree Functions

%% sbt_link takes 2 trees with rank R and merges them into a new tree with rank
%% R+1, making the subtree with worse priority a child of the subtree with lower priority
sbt_link(X=#tree{rank=Rank, root=XRoot}, Y=#tree{rank=Rank, root=YRoot}) when XRoot =< YRoot ->
      X#tree{rank=X#tree.rank+1,rest=[Y|X#tree.rest]};
sbt_link(X=#tree{rank=Rank}, Y=#tree{rank=Rank}) ->
      Y#tree{rank=X#tree.rank+1,rest=[X|Y#tree.rest]}.

%% skew_link is a 3-way merge between a single element, E, and two trees of the same rank R, to create a tree of rank R+1
skew_link(E, X, Y) ->
  N=#tree{root=NRoot,elem_rest=NRest}=sbt_link(X, Y),
  if
    E =< NRoot ->
      N#tree{root=E,elem_rest=[NRoot|NRest]};
    true ->
      N#tree{elem_rest=[E|NRest]}
  end.

tree_get_min([X]) -> {X, []};
tree_get_min([X|Xs]) ->
  {Y, Ys} = tree_get_min(Xs),
  if
    X#tree.root =< Y#tree.root ->
      {X,Xs};
    true ->
      {Y,[X|Ys]}
  end.

do_lookup_min([]) -> error;
do_lookup_min([#tree{root=Root}]) -> {ok,Root};
do_lookup_min([#tree{root=Root}|Rest]) ->
  {ok,lists:foldl(fun(#tree{root=R},Acc) -> min(R,Acc) end, Root, Rest)}.

ins_tree(T, []) -> [T];
ins_tree(T1, [T2|Ts]) when T1#tree.rank < T2#tree.rank -> [T1,T2|Ts];
ins_tree(T1, [T2|Ts]) -> ins_tree(sbt_link(T1,T2), Ts).

%% EUnit Tests

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

foldl(_Fun, Acc0, empty) -> Acc0;
foldl(Fun, AccIn, QIn) ->
  {ok, E} = peek_min(QIn),
  Acc = Fun(E, AccIn),
  {ok, Q} = delete_min(QIn),
  foldl(Fun, Acc, Q).

to_list(Q) -> foldl(fun(E,L) -> [E|L] end, [], Q).

enqueue_test() ->
  Value = 3,
  A0 = new(),
  A1 = insert(Value, A0),
  ?assertMatch(error, peek_min(A0)),
  ?assertMatch({ok, Value}, peek_min(A1)).

repeatedly(_Fun, 0) -> [];
repeatedly(Fun, N) -> [Fun()| repeatedly(Fun, N-1)].

usage_test() ->
  Value = repeatedly(fun() -> rand:uniform(10) end, 10),
  Q = lists:foldl(fun insert/2, new(), Value),
  Returned = to_list(Q),
  Sorted = lists:sort(fun(A,B) -> B =< A end, Value),
  ?assertMatch(Sorted, Returned).

window_test() ->
  ?debugVal(rand:export_seed()),
  N = 1000,
  K = 100,
  Values = repeatedly(fun() -> rand:uniform(1000) end, N),
  MaxK = lists:sublist(lists:reverse(lists:sort(Values)), K),
  ?debugVal(MaxK),
  ?debugVal(length(MaxK)),
  First = lists:foldl(fun insert/2, new(), lists:sublist(Values, K)),
  Rest = lists:sublist(Values, K+1, N-K),
  Q = lists:foldl(fun(E, Q0) -> {ok, Q1} = delete_min(insert(E, Q0)), Q1 end, First, Rest),
  ResultSet = to_list(Q),
  ?debugVal(ResultSet),
  ?debugVal(length(ResultSet)),
  % we test for superset membership here because the "naive" MaxK loses items.
  ?assertMatch(MaxK, ResultSet).

-endif.
