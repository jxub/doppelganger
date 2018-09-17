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
    |> Enum.map(fn arg -> String.capitalize(arg) end)
    |> Enum.join(", ")
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
