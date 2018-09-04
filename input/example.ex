defmodule Data.Example do
  # use GenServer
  # protocol String.Chars not implemented for {:use, [line: 2], [{:__aliases__, [line: 2], [:GenServer]}]}
  
  def add(a, b) do
    a + b
  end

  def is_val?(v) do
    a = v + 2
    a1 = a * 2

    a1
  end

  def sum_list(l) do
    l
    |> List.foldl(0, fn el, acc -> el + acc end)
  end

  def demo do
    1
  end

  defp private(c) do
    {:ok, c}
  end

  # defstruct [:name, :age]
  # protocol String.Chars not implemented for {:defstruct, [line: 29], [[:name, :age]]}
end