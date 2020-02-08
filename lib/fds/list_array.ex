defmodule FDS.ListArray do
  defstruct [:ralist, :count]
  @type t :: %__MODULE__{ralist: :fds_ralist.ralist(any()), count: non_neg_integer()}

  @spec new() :: t
  def new() do
    %__MODULE__{ralist: :fds_ralist.new(), count: 0}
  end

  @spec push(t, term()) :: t
  def push(%__MODULE__{ralist: ralist, count: count} = list_array, item) do
    %{list_array | ralist: :fds_ralist.cons(item, ralist), count: count + 1}
  end

  @spec peek(t) :: :error | {:ok, term()}
  def peek(%__MODULE__{count: 0}), do: :error
  def peek(%__MODULE__{ralist: ralist}), do: {:ok, :fds_ralist.head(ralist)}

  @spec drop(t) :: :error | {:ok, t()}
  def drop(%__MODULE__{count: 0}), do: :error
  def drop(%__MODULE__{ralist: ralist, count: count}) do
    {:ok, %__MODULE__{ralist: :fds_ralist.tail(ralist), count: count-1}}
  end

  @spec pop(t) :: :error | {:ok, term(), t()}
  def pop(%__MODULE__{count: 0}), do: :error
  def pop(%__MODULE__{ralist: ralist, count: count}) do
    {:ok, :fds_ralist.head(ralist), %__MODULE__{ralist: :fds_ralist.tail(ralist), count: count-1}}
  end

  @spec get(t, non_neg_integer) :: :error | {:ok, term()}
  def get(%__MODULE__{}, index) when index < 0, do: :error
  def get(%__MODULE__{count: count}, index) when index >= count, do: :error
  def get(%__MODULE__{ralist: ralist, count: count}, index) do
    {:ok, :fds_ralist.lookup(index, ralist)}
  end

  @spec set(t, non_neg_integer, term()) :: :error | {:ok, t()}
  def set(%__MODULE__{}, index, _value) when index < 0, do: :error
  def set(%__MODULE__{count: count}, index, _value) when index >= count, do: :error
  def set(%__MODULE__{ralist: ralist, count: count} = list_array, index, value) do
    {:ok, %{list_array | ralist: :fds_ralist.update(index, value, ralist)}}
  end

  @spec foldl(t, term(), (term(), term() -> term())) :: term()
  def foldl(%__MODULE__{ralist: ralist}, acc, fun) do
    :fds_ralist.foldl(fun, acc, ralist)
  end

  @spec to_list(t) :: [term()]
  def to_list(%__MODULE__{} = list_array) do
    :lists.reverse foldl(list_array, [], &([&1|&2]))
  end

  @spec count(t) :: non_neg_integer
  def count(%__MODULE__{count: count}), do: count
end

defimpl Collectable, for: FDS.ListArray do
  def into(%FDS.ListArray{ralist: ralist, count: count}) do
    {{ralist, count}, &do_into/2}
  end

  defp do_into(_ralist_and_count, :halt), do: :ok
  defp do_into({ralist, count}, :done), do: %FDS.ListArray{ralist: ralist, count: count}
  defp do_into({ralist, count}, {:cont, item}), do: {:fds_ralist.cons(item, ralist), count+1}
end

defimpl Enumerable, for: FDS.ListArray do
  defp do_reduce(_ralist, {:halt, acc}, _fun), do: {:halted, acc}
  defp do_reduce(ralist, {:suspend, acc}, fun), do: {:suspended, acc, &do_reduce(ralist, &1, fun)}
  defp do_reduce([], {:cont, acc}, _fun), do: {:done, acc}
  defp do_reduce(ralist, {:cont, acc}, fun) do
    do_reduce(:fds_ralist.tail(ralist), fun.(:fds_ralist.head(ralist), acc), fun)
  end

  def reduce(%FDS.ListArray{ralist: ralist}, acc, fun) do
    do_reduce(ralist, acc, fun)
  end

  def count(%FDS.ListArray{count: count}), do: {:ok, count}
  def member?(%FDS.ListArray{}, _item), do: {:error, @protocol}
  def slice(%FDS.ListArray{}), do: {:error, @protocol}
end

defimpl Inspect, for: FDS.ListArray do
  import Inspect.Algebra
  def inspect(list_array, opts) do
    container_doc("#ListArray<[", FDS.ListArray.to_list(list_array), "]>", opts, &Inspect.Algebra.to_doc/2, separator: ",", break: :maybe)
  end
end
