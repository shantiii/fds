-module(fds_arl).

-export([new/0, append/2, lookup/2, update/3, count/1]).

%% data structure consists of a "reversed" random-access-list
new() ->
    {0, fds_ralist:new()}.

append(Elem, {Count, RAL}) ->
    {Count+1, fds_ralist:cons(Elem, RAL)}.

lookup(Index, {Count, RAL}) ->
    fds_ralist:lookup(Count - Index - 1, RAL).

update(Index, Elem, {Count, RAL}) ->
    {Count, fds_ralist:update(Count - Index - 1, Elem, RAL)}.

count({Count, _RAL}) ->
    Count.
