defmodule Doppelganger.Parse.DoppelVariable do
  defstruct [:variable, :value]

  @moduledoc """
  ** (ArgumentError) argument error
    :erlang.byte_size(true)
    lib/doppelganger/parse/doppel_variable.ex:10: String.Chars.Doppelganger.Parse.DoppelVariable.to_string/1
    (elixir) lib/enum.ex:1314: Enum."-map/2-lists^map/1-0-"/2
    (elixir) lib/enum.ex:1314: Enum."-map/2-lists^map/1-0-"/2
    
    
    TODO:


    convert value to printable string
  """

  def it({:@, [_line], [{variable, _line2, [value]}]}) do
    %__MODULE__{variable: variable, value: value}
  end

  defimpl String.Chars do
    def to_string(%{variable: variable, value: value}) when is_atom(variable) do
      "-define(" <> string(variable) <> ", " <> string(value) <> ")"
    end

    def to_string(%{variable: variable, value: value}) when is_binary(variable) do
      "-define(" <> string(variable) <> ", " <> string(value) <> ")"
    end

    def to_string(%{variable: variable, value: value}) when is_boolean(variable) do
      "-define(" <> string(variable) <> ", " <> string(value) <> ")"
    end

    defp string(v) when is_boolean(v) do
      case v do
        true -> "true"
        false -> "false"
      end
    end

    defp string(v) when is_atom(v) do
      v |> Atom.to_string()
    end

    defp string(v) when is_binary(v) do
      v
    end
  end
end
