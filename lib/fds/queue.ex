# TODO: queue compat
# TODO: dequeue compat
defmodule FDS.Queue do
  defstruct [:ft, :count]

  @type fds_ft :: any
  @type t :: %__MODULE__{ft: fds_ft, count: non_neg_integer}

  def new(), do: %__MODULE__{ft: :fds_ft.new(), count: 0}

  @spec push_back(t, term()) :: t
  def push_back(%__MODULE__{ft: ft, count: count}, item) do
    %__MODULE__{ft: :fds_ft.push(item, ft), count: count + 1}
  end

  def peek_front(%__MODULE__{count: 0}), do: :error
  def peek_front(%__MODULE__{ft: ft}) do
    :fds_ft.peek(ft)
  end

  def drop_front(%__MODULE__{count: 0}), do: :error
  def drop_front(%__MODULE__{ft: ft, count: count}) do
    {:ok, new_ft} = :fds_ft.drop(ft)
    {:ok, %__MODULE__{ft: new_ft, count: count - 1}}
  end

  def pop_front(%__MODULE__{count: 0}), do: :error
  def pop_front(%__MODULE__{ft: ft, count: count}) do
    {:ok, item, new_ft} = :fds_ft.pop(ft)
    {:ok, item, %__MODULE__{ft: new_ft, count: count - 1}}
  end

  def foldl(%__MODULE__{ft: ft}, acc, fun) do
    :fds_ft.foldl(fun, acc, ft)
  end

  def to_list(%__MODULE__{ft: ft}) do
    :lists.reverse :fds_ft.foldl(&[&1|&2], [], ft)
  end
end

defimpl Collectable, for: FDS.Queue do
  def into(%FDS.Queue{ft: ft, count: count}) do
    {{ft, count}, &do_into/2}
  end

  defp do_into(_ft_and_count, :halt), do: :ok
  defp do_into({ft, count}, :done), do: %FDS.Queue{ft: ft, count: count}
  defp do_into({ft, count}, {:cont, item}), do: {:fds_ft.push(item, ft), count+1}
end

defimpl Enumerable, for: FDS.Queue do
  defp do_reduce(_ft, {:halt, acc}, _fun), do: {:halted, acc}
  defp do_reduce(ft, {:suspend, acc}, fun), do: {:suspended, acc, &do_reduce(ft, &1, fun)}
  defp do_reduce(:ft0, {:cont, acc}, _fun), do: {:done, acc}
  defp do_reduce(ft, {:cont, acc}, fun) do
    {:ok, item, new_ft} = :fds_ft.pop(ft)
    do_reduce(new_ft, fun.(item, acc), fun)
  end

  def reduce(%FDS.Queue{ft: ft}, acc, fun) do
    do_reduce(ft, acc, fun)
  end

  def count(%FDS.Queue{count: count}), do: {:ok, count}
  def member?(%FDS.Queue{}, _item), do: {:error, @protocol}
  def slice(%FDS.Queue{}), do: {:error, @protocol}
end

defimpl Inspect, for: FDS.Queue do
  import Inspect.Algebra
  def inspect(queue, opts) do
    container_doc("#Queue<[", FDS.Queue.to_list(queue), "]>", opts, &Inspect.Algebra.to_doc/2, separator: ",", break: :maybe)
  end
end
