defmodule Doppelganger.Parse.DExports do
  alias Doppelganger.Parse.DFunction

  @spec it(Tuple.t) :: String.t
  def it(ast) do
    with {_mod, _line, [_aliases, module_body]} <- ast,
         [do: {_block, _l, elements}] <- module_body do
      elements
      |> Enum.reject(&unexported?/1)
      |> Enum.map(fn fun -> DFunction.export(fun) end)
      |> Enum.reject(fn ex -> ex == nil end)
      |> Enum.sort()
      |> Enum.map(fn ex -> "    " <> ex end)
      |> Enum.join(",\n")
    end
  end

  defp unexported?(el) do
    with type <- el |> elem(0) do
      case type do
        :def -> false
        _    -> true
      end
    end
  end
end
