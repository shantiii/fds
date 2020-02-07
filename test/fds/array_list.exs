defmodule FDS.ArrayList.Test do
  use ExUnit.Case

  alias FDS.ArrayList
  test "ArrayList.new/0" do
    assert %ArrayList{} = empty = ArrayList.new()
    assert Enum.empty?(empty)
    assert Enum.count(empty) == 0
    assert ArrayList.to_list(empty) == []
    assert ArrayList.foldr(empty, 0, &(&1+&2)) == 0
    assert ArrayList.get(empty, 0) == :error
  end

  test "ArrayList.push/2" do
    al =
      ArrayList.new()
      |> ArrayList.push(1)
      |> ArrayList.push(2)
      |> ArrayList.push(3)

    assert ArrayList.peek(al) == {:ok, 3}
    assert ArrayList.get(al, 0) == {:ok, 1}
    assert ArrayList.get(al, 1) == {:ok, 2}
    assert ArrayList.get(al, 2) == {:ok, 3}
    assert ArrayList.get(al, 3) == :error
  end


  test "to_list/1" do
    al =
      ArrayList.new()
      |> ArrayList.push(1)
      |> ArrayList.push(2)
      |> ArrayList.push(3)

    assert [1, 2, 3] = ArrayList.to_list(al)
  end

  test "Enum.into works" do
    alias FDS.ArrayList
    assert Enum.into(1..10, ArrayList.new()) != ArrayList.new()
  end

  test "Enum.reduce works" do
    alias FDS.ArrayList
    assert Range.new(1,10) |> Enum.into(ArrayList.new()) |> Stream.map(&(&1 * 2)) |> Enum.sum() == 110
  end
end
