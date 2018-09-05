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
  alias Doppelganger.Parse.DModule

  @path "input/example.ex"
  @out_dir "output"

  def main(args \\ []) do
    with {parsed, _args, _invalid} <- OptionParser.parse(["--file"], []),
         file_name <- parsed |> Keyword.get(:file, @path),
         file <- File.read!(@path) do
      {:ok, ast} = Code.string_to_quoted(file)

      ast |> to_erlang()
    end
  end

  def to_erlang(ast) do
    with module <- DModule.name(ast),
         body <- DModule.it(ast) |> to_string(),
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

  def to_elixir(_) do
    raise "to be implemented"
  end
end
