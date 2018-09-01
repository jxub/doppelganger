defmodule Doppelganger.Parse do
  alias __MODULE__

  ## TODO: handle spec parsing

  @moduledoc """
  Also handle:

  stdlib functions translation
  behaviour translations
  records
  """

  # import Record, [only: defrecordp]

  # Record.defrecordp(state, :state, functions: nil)

  defmodule Function do
    defmodule Info do
      defstruct [:name, :args, :body, :public]

      defimpl String.Chars do
        def to_string(%{name: name, args: args, body: body, public: public}) do
          "#{name}(#{args}) -> \n    #{body}. \n"
        end

        def to_string(_),
          do:
            raise(
              "remember to call it/1 in order to parse before applying to_string/1 to a function"
            )
      end
    end

    def it(fun) do
      with {mod, _line, [{name, _line1, args}, [do: body]]} <- fun,
           args = args(args),
           body = body(body),
           public = public?(mod) do
        %Info{name: name, args: args, body: body, public: public}
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
      b
      |> Macro.to_string()
      |> String.split()
      |> Parse.Char.it()
    end

    def public?(:def), do: true
    def public?(:defp), do: false
    def public?(_), do: raise("not a function")

    def arity({_def_or_defp, _line, [{_name, _line1, args}, _body]}) when is_nil(args), do: 0

    def arity({_def_or_defp, _line, [{_name, _line1, args}, _body]}) when is_list(args),
      do: length(args)

    def export({:defp, _line, [{name, _line1, args}, _body]} = fun), do: nil

    def export({:def, _line, [{name, _line1, args}, _body]} = fun) do
      with arity <- arity(fun) do
        "#{name}/#{arity}"
      end
    end
  end

  defmodule Char do
    def it(chars) do
      chars
      |> Enum.map(fn ch ->
        case ch |> String.starts_with?(":") do
          true ->
            # atom
            ch |> String.slice(1..1500)

          false ->
            # value
            ch |> String.capitalize()
        end
      end)
    end
  end

  defmodule Struct do
    def it({:defstruct, [_line], [kvs]}) when is_list(kvs) do
    end

    defmodule Info do
      defstruct [:name, :kvs]

      defimpl String.Chars do
        def to_string(%{name: name, kvs: kvs}) do
          with kvs <- kvs |> Enum.join(", \n") do
            "-record(name,\n{#{kvs}\n})"
          end
        end

        def to_string(_),
          do:
            raise(
              "remember to call it/1 in order to parse before applying to_string/1 to a struct"
            )
      end
    end
  end

  defmodule Behaviour do
    def it(bh) do
      nil
    end
  end

  defmodule Module do
    def name(ast) do
      with {:defmodule, _line, [{:__aliases__, _line1, aliases}, _body]} <- ast do
        aliases
        |> Enum.map(fn a -> a |> Atom.to_string() end)
        |> Enum.map(fn a -> a |> String.downcase() end)
        |> Enum.join("_")
        |> String.replace_leading("_", "")
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
           [do: {_block, _l, functions}] <- module_body do
        functions
        |> Enum.reject(fn fun -> fun |> Tuple.to_list() |> Enum.at(0) != :def or :defp end)
        |> Enum.map(fn fun -> Parse.Function.export(fun) end)
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
        "-module(#{name}).\n\n-export([\n#{exports}\n]).\n\n#{body}"
      end
    end
  end
end
