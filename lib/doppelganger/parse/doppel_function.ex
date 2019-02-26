defmodule Doppelganger.Parse.DoppelFunction do
  alias Doppelganger.Scope

  alias Doppelganger.Parse.{
    DoppelChar,
    DoppelLine
  }

  defstruct [:name, :args, :body, :public]

  defimpl String.Chars do
    def to_string(%{name: name, args: args, body: body, public: public}) do
      "#{name}(#{args}) -> \n    #{body}. \n"
    end

    def to_string(_) do
      raise Doppelganger.Util.parse_error(__MODULE__)
    end
  end

  def it(fun) do
    with {mod, _line, [{name, _line1, args}, [do: body]]} <- fun,
         args = args(args),
         body = body(body),
         public = public?(mod) do
      %__MODULE__{name: name, args: args, body: body, public: public}
    end
  end

  def args(a) when is_nil(a), do: ""

  def args(a) do
    a
    |> Enum.map(fn arg -> with {name, _line, _default} <- arg, do: Atom.to_string(name) end)
    |> Enum.map(fn arg -> arg |> argument() end)
    |> Enum.join(", ")
  end

  def argument(a) when is_atom(a) do
    a |> Atom.to_string() |> String.downcase()
  end

  def argument(a) when is_binary(a) do
    a |> String.capitalize()
  end

  def argument(a) when is_list(a) do
    with [{op, [_line], elems}] <- a do
      case op do
        :| ->
          with [elem, rest] <- elems,
               {el, _line, val} <- elem,
               {el1, _line, val1} <- rest,
               els <- argument(el),
               els1 <- argument(el1) do
            "[" <> els <> "|" <> els1 <> "]"
          end

        _ ->
          raise "implement in DoppelFunction.argument/1 (same logic as above?"
      end
    end
  end

  def argument(a) when is_tuple(a) do
    with {item, {item2, _line, _val}} <- a do
      "{" <> argument(item) <> ", " <> argument(item2) <> "}"
    end
  end

  def argument(a) when is_map(a) do
    raise "implement argument for maps"
  end

  def body(b) do
    with lines <- b |> Macro.to_string() |> String.split("\n"),
         tokens <- lines |> tokenize() do
      Enum.join(lines, ",\n")
    end
  end

  def tokenize([line | rest]) do
    tokenize(line)

    tokenize(rest)
  end

  def tokenize(line) when is_bitstring(line) do
    nil
  end

  def tokenize([]) do
    nil
  end

  def public?(:def), do: true
  def public?(:defp), do: false
  def public?(_), do: raise("not a function")

  def arity({_def_or_defp, _line, [{_name, _line1, args}, _body]}) when is_nil(args), do: 0

  def arity({_def_or_defp, _line, [{_name, _line1, args}, _body]}) when is_list(args),
    do: length(args)

  def arity(el), do: raise("#{IO.inspect(el)} not a function")

  def export({:defp, _line, [{name, _line1, args}, _body]} = fun), do: nil

  def export({:def, _line, [{name, _line1, args}, _body]} = fun) do
    with arity <- arity(fun) do
      "#{name}/#{arity}"
    end
  end

  def export(el), do: raise("#{IO.inspect(el)} not a function")
end
