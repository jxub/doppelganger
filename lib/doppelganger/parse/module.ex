defmodule Doppelganger.Parse.Module do
  defstruct [:name, :body, :exports]

  defimpl String.Chars do
    def to_string(%{name: name, body: body, exports: exports}) do
      "-module(#{name}).\n\n-export([\n#{exports}\n]).\n\n#{body}"
    end

    def to_string(_) do
      raise "remember to call it/1 in order to parse before applying to_string/1 to a module"
    end
  end

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
    with el1 <- el |> Tuple.to_list(),
         mod <- el1 |> Enum.at(0) do
      case mod do
        :def ->
          Parse.Function.it(el)

        :defp ->
          Parse.Function.it(el)

        :defstruct ->
          Parse.Struct.it(el)

        :use ->
          Parse.Behaviour.it(el)

        type ->
          raise "#{type} not supported"
      end
    end
  end

  def exports(ast, exports \\ []) do
    with {_mod, _line, [_aliases, module_body]} <- ast,
         [do: {_block, _l, elements}] <- module_body do
      elements
      |> IO.inspect()
      |> Enum.reject(fn el ->
        with type <- el |> Tuple.to_list() |> Enum.at(0) do
          case type do
            :def ->
              true

            :defp ->
              true

            _ ->
              false
          end
        end
      end)
      |> IO.inspect()
      |> Enum.map(fn fun -> Parse.Function.export(fun) end)
      |> IO.inspect()
      |> Enum.reject(fn ex -> ex == nil end)
      |> Enum.sort()
      # left-pad
      |> Enum.map(fn ex -> "    " <> ex end)
      |> Enum.join(",\n")
    end
  end

  def it(ast) do
    with name <- name(ast),
         body <- body(ast),
         exports <- exports(ast) do
      %__MODULE__{name: name, body: body, exports: exports}
    end
  end
end
