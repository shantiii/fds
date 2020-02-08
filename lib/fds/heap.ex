
defmodule FDS.Heap do
  defstruct [:mod, :heap, :count]

  #TODO: fix this typespec
  @type t :: term()

  @spec new(:min | :max) :: t
  def new(min_or_max), do: %__MODULE__{mod: heap_module(min_or_max), heap: heap_module(min_or_max).new(), count: 0}

  @spec push(t, term()) :: t
  def push(%__MODULE__{mod: mod, heap: heap, count: count}, item) do
    %__MODULE__{mod: mod, heap: mod.insert(item, heap), count: count + 1}
  end

  def peek(%__MODULE__{count: 0}), do: :error
  def peek(%__MODULE__{mod: mod, heap: heap}) do
    mod.peek(heap)
  end

  def drop(%__MODULE__{count: 0}), do: :error
  def drop(%__MODULE__{mod: mod, heap: heap, count: count}) do
    {:ok, new_heap} = mod.delete(heap)
    {:ok, %__MODULE__{mod: mod, heap: new_heap, count: count - 1}}
  end

  def pop(%__MODULE__{count: 0}), do: :error
  def pop(%__MODULE__{mod: mod, heap: heap, count: count}) do
    {:ok, item} = mod.peek(heap)
    {:ok, new_heap} = mod.delete(heap)
    {:ok, item, %__MODULE__{mod: mod, heap: new_heap, count: count - 1}}
  end

  def foldl(%__MODULE__{mod: mod, heap: heap}, acc, fun) do
    mod.foldl(fun, acc, heap)
  end

  def to_list(%__MODULE__{mod: mod, heap: heap}) do
    :lists.reverse mod.foldl(&[&1|&2], [], heap)
  end

  defp heap_module(:min), do: :fds_bsb_minheap
  defp heap_module(:max), do: :fds_bsb_maxheap
end

defimpl Collectable, for: FDS.Heap do
  def into(%FDS.Heap{mod: mod, heap: heap, count: count}) do
    {{mod, heap, count}, &do_into/2}
  end

  defp do_into(_heap_and_count, :halt), do: :ok
  defp do_into({mod, heap, count}, :done), do: %FDS.Heap{mod: mod, heap: heap, count: count}
  defp do_into({mod, heap, count}, {:cont, item}), do: {mod, mod.insert(item, heap), count+1}
end

defimpl Enumerable, for: FDS.Heap do
  defp do_reduce(_mod_and_heap, {:halt, acc}, _fun), do: {:halted, acc}
  defp do_reduce(mod_and_heap, {:suspend, acc}, fun), do: {:suspended, acc, &do_reduce(mod_and_heap, &1, fun)}
  defp do_reduce({_mod, :empty}, {:cont, acc}, _fun), do: {:done, acc}
  defp do_reduce({mod, heap}, {:cont, acc}, fun) do
    {:ok, item, new_heap} = mod.pop(heap)
    do_reduce({mod, new_heap}, fun.(item, acc), fun)
  end

  def reduce(%FDS.Heap{mod: mod, heap: heap}, acc, fun) do
    do_reduce({mod, heap}, acc, fun)
  end

  def count(%FDS.Heap{count: count}), do: {:ok, count}
  def member?(%FDS.Heap{}, _item), do: {:error, @protocol}
  def slice(%FDS.Heap{}), do: {:error, @protocol}
end

defimpl Inspect, for: FDS.Heap do
  import Inspect.Algebra

  defp module_name(:fds_bsb_minheap), do: "MinHeap"
  defp module_name(:fds_bsb_maxheap), do: "MaxHeap"

  def inspect(heap, opts) do
    container_doc("#"<> module_name(heap.mod) <>"<[", FDS.Heap.to_list(heap), "]>", opts, &Inspect.Algebra.to_doc/2, separator: ",", break: :maybe)
  end
end
