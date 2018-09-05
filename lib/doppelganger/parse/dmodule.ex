defmodule Doppelganger.Parse.DModule do
  alias Doppelganger.Parse.{
    DBehaviour,
    DExports,
    DFunction,
    DStruct
  }

  defstruct [:name, :body, :exports]

  defimpl String.Chars do
    def to_string(%{name: name, body: body, exports: exports}) do
      IO.inspect(exports, label: "in to string exports")
      "-module(#{name}).\n\n-export([\n#{exports}\n]).\n\n#{body}"
    end

    def to_string(_) do
      raise "remember to call it/1 in order to parse before applying to_string/1 to a module"
    end
  end

  @spec name(Tuple.t()) :: String.t()
  def name(ast) do
    with {:defmodule, _line, [{:__aliases__, _line1, aliases}, _body]} <- ast do
      aliases
      |> Enum.map(fn a -> a |> Atom.to_string() end)
      |> Enum.map(fn a -> a |> Macro.underscore() end)
      |> Enum.join("_")
      |> String.replace_leading("_", "")
      |> String.replace("/", "_")
    end
  end

  @spec body(Tuple.t()) :: String.t()
  def body(ast) do
    with {_mod, _line, [_aliases, module_body]} <- ast,
         [do: {_block, _l, elements}] <- module_body do
      elements
      |> IO.inspect()
      |> Enum.map(&delegate/1)
      |> Enum.map(fn el -> to_string(el) end)
      |> Enum.join("\n")
    end
  end

  def delegate(el) do
    with mod <- el |> elem(0) do
      delegate(mod, el)
    end
  end

  def delegate(:def, el), do: DFunction.it(el)
  def delegate(:defp, el), do: DFunction.it(el)
  def delegate(:defstruct, el), do: DStruct.it(el)
  def delegate(:use, el), do: DBehaviour.it(el)
  def delegate(other, _el), do: raise("#{other} not supported")

  def exports(ast) do
    ast
    |> DExports.it()
  end

  def it(ast) do
    with name <- name(ast),
         body <- body(ast),
         exports <- exports(ast) do
      IO.inspect(exports, label: "expooooorts")
      %__MODULE__{name: name, body: body, exports: exports}
    end
  end
end
