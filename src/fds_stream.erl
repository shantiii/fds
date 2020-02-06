-module(fds_stream).

-compile(export_all).

%-type stream(T) :: maybe_improper_list(T, {fun((G) -> T).

fold(Fun, Acc, ZList) ->
    List = case ZList of
               Lazy when is_function(Lazy) -> Lazy();
               Real when is_list(Real) -> Real
           end,
    case List of
        [] -> Acc;
        [Elem | Rest] -> fold(Fun, Fun(Elem, Acc), Rest)
    end.

empty() -> fun() -> [] end.

repeatedly(Generator) -> fun ZL() -> [Generator()|ZL] end.

recursive(Fun, Acc0) ->
    fun() ->
            Acc1 = Fun(Acc0),
            [Acc1| recursive(Fun, Acc1)]
    end.

map(Fun, ZList) ->
    fun() ->
            List = case ZList of
                       Lazy when is_function(Lazy) -> Lazy();
                       Real when is_list(Real) -> Real
                   end,
            case List of
                [] -> [];
                [Elem | Rest] -> [Fun(Elem) | map(Fun, Rest)]
            end
    end.

filter(Fun, ZList) ->
    fun() ->
            List = case ZList of
                       Lazy when is_function(Lazy) -> Lazy();
                       Real when is_list(Real) -> Real
                   end,
            case List of
                [] -> [];
                [Elem | Rest] ->
                    case Fun(Elem) of
                        true -> [Elem | filter(Fun, Rest)];
                        false -> filter(Fun, Rest)
                    end
            end
    end.

take(0, _ZList) -> [];
take(_N, []) -> [];
take(N, ZList) ->
    List = case ZList of
               Lazy when is_function(Lazy) -> take(N, Lazy());
               Real when is_list(Real) -> Real
           end,
    case List of
        [] -> [];
        [Elem | Rest] -> [Elem | take(N-1, Rest)]
    end.

drop(0, ZList) -> ZList;
drop(_N, []) -> [];
drop(N, ZList) ->
    fun()->
            List = case ZList of
                       Lazy when is_function(Lazy) -> Lazy();
                       Real when is_list(Real) -> Real
                   end,
            case List of
                LazyL when is_function(LazyL) -> drop(N, LazyL());
                [] -> [];
                [_Elem | Rest] -> drop(N-1, Rest)
            end
    end.


test_nothing(N) ->
    Seq0 = recursive(fun(X) -> X+1 end, N),
    Seq1 = map(fun(X) -> X*2 end, Seq0),
    Seq2 = filter(fun(X) -> X rem 10 == 0 end, Seq1),
    Seq3 = drop(10, Seq2),
    Seq4 = take(10, Seq3).
