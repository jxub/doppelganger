defmodule Doppelganger.Parse.DStruct do
  defstruct [:name, :kvs]

  def it({:defstruct, [_line], [kvs]}) when is_list(kvs) do
    %__MODULE__{name: "struct module", kvs: kvs}
  end

  defimpl String.Chars do
    def to_string(%{name: name, kvs: kvs}) do
      with kvs <- kvs |> Enum.join(", \n") do
        "-record(name,\n{#{kvs}\n})"
      end
    end

    def to_string(_) do
      raise "remember to call it/1 in order to parse before applying to_string/1 to a struct"
    end
  end
end
