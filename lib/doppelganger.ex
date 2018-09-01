defmodule Doppelganger do
  @moduledoc """
  Documentation for Doppelganger.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Doppelganger.hello
      :world

  """
  alias Doppelganger.Parse

  @path "input/example.ex"
  @out_dir "output"

  def main(args \\ []) do
    with {parsed, _args, _invalid} <- OptionParser.parse(["--file"], []),
         file_name <- parsed |> Keyword.get(:file, @path),
         file <- File.read!(@path) do
      {:ok, ast} = Code.string_to_quoted(file)

      ast |> to_erlang_code()
    end
  end

  def to_erlang_code(ast) do
    with module <- Parse.Module.name(ast),
         body <- Parse.Module.it(ast),
         fname <- module <> ".erl",
         fpath <- Path.join(@out_dir, fname) |> Path.absname() do
      case File.write(fpath, body) do
        :ok ->
          :ok

        {:error, reason} ->
          IO.puts(reason)
      end
    end
  end
end
