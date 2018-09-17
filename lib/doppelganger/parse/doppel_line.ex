defmodule Doppelganger.Parse.DoppelLine do
  alias Doppelganger.Parse.DoppelChar

  def it(line) do
    with chars <- line |> String.split() do
      parse(chars, "")
    end
  end

  defp parse([char | chars], result) do
    with result <- result <> DoppelChar.it(char) do
      parse(chars, result)
    end
  end

  defp parse([], result) do
    result
  end
end
