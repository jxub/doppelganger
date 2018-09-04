defmodule Doppelganger.Parse.Char do
  defstruct [:self, :type]

  def it(char) do
    case String.starts_with?(char, ":") do
      true ->
        %__MODULE__{self: char |> String.slice(1..1500), type: :atom}

      false ->
        %__MODULE__{self: char |> String.capitalize(), type: :value}
    end
  end

  defmodule Atom do
    defstruct [:self]

    def is?(ch) do
      if String.starts_with?(ch, ":") do
        true
      else
        false
      end
    end

    defimpl String.Chars do
      def to_string(%{self: self}) do
        self
        |> String.slice(1..1500)
        |> String.downcase()
      end
    end
  end

  defmodule Call do
    defstruct [:module, :fun]
  end

  def function?(ch) do
  end

  defimpl String.Chars do
    def to_string(%{self: self, type: type}) do
      "#{self}"
    end

    def to_string(_) do
      raise Doppelganger.Util.parse_error(__MODULE__)
    end
  end
end
