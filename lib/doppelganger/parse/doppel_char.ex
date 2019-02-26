defmodule Doppelganger.Parse.DoppelChar do
  defstruct [:self, :type]

  def it(char) when is_atom(char) do
    %__MODULE__{self: char |> Atom.to_string(), type: :atom}
  end

  def it(char) when is_binary(char) do
    %__MODULE__{self: char |> String.capitalize(), type: :value}
  end

  def it(_) do
    raise Doppelganger.Util.parse_error(__MODULE__)
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

  def function?(_ch) do
  end

  defimpl String.Chars do
    def to_string(%{self: self, type: _type}) do
      "#{self}"
    end

    def to_string(_) do
      raise Doppelganger.Util.parse_error(__MODULE__)
    end
  end
end
