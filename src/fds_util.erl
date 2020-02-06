-module(fds_util).

-export([term_size/1]).

%% function to get the term size of ... a term.
%%
%% This does NOT include
%% * the size of atom data in the atom table,
%% * the true size of a binary as a binary can have 3-6 bytes of overhead, and refer to another, uncopied binary
%% * the true size of a large (>32 entry) map, as it is probabilistic. It instead reports the lower bound
%% * the size of the erlang process context that a PID points to
%% * the size of the erlang port context that a Port points to
%% * the size of the erlang node context that a Reference points to
%% * the true size of a fun, as the overhead can range from 9-13 words, and there is no way to measure the size of the environment
%% * the size of an ets table overhead / entry

term_size(Term) -> term_words(Term) * erlang:system_info({wordsize, external}).

term_words(Term) when is_integer(Term) -> 1;

%% TODO: large integers

term_words(Term) when is_atom(Term) -> 1;

term_words(Term) when is_float(Term) ->
    case erlang:system_info(wordsize) of
        4 -> 4;
        8 -> 3
    end;

term_words(Term) when is_binary(Term) -> 3 + byte_size(Term);

term_words([]) -> 1;
term_words([Term|Rest]) -> 1 + term_words(Term) + term_words(Rest);

term_words(Term) when is_tuple(Term) ->
    2 + lists:sum([ term_words(element(Pos, Term)) || Pos <- lists:seq(1, tuple_size(Term))]);

term_words(Term) when is_map(Term) andalso map_size(Term) =< 32 ->
    maps:fold(fun(Key, Value, TotalWords) -> TotalWords + term_words(Key) + term_words(Value) end, 5, Term);
term_words(Term) when is_map(Term) andalso map_size(Term) =< 32 ->
    maps:fold(fun(Key, Value, TotalWords) -> TotalWords + term_words(Key) + term_words(Value) end, round(map_size(Term) * 1.6), Term);

term_words(Term) when is_pid(Term) orelse is_port(Term) ->
    Local = node(),
    case node(Term) of
        Local -> 1;
        _Remote -> 6
    end;

term_words(Term) when is_reference(Term) ->
    Local = node(),
    case {node(Term), erlang:system_info(wordsize)} of
        {Local, 4} -> 5;
        {_Remote, 4} -> 12;
        {Local, 8} -> 4;
        {_Remote, 8} -> 10
    end;

term_words(Term) when is_function(Term) ->
    9.
