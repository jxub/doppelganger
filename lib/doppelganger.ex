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
  alias Doppelganger.Parse.DoppelModule

  @path "input/example.ex"
  @out_dir "output_total"

  def main(args \\ []) do
    with {parsed, _args, _invalid} <- OptionParser.parse(["--path"], strict: [source: :string]),
         {target_lang, _args, _invalid} <-
           OptionParser.parse(["--target"], strict: [source: :string]),
         file_name <- parsed |> Keyword.get(:path, @path),
         file <- File.read!(@path) do
      case target_lang do
        "elixir" ->
          {:ok, ast} = Code.string_to_quoted(file)
          ast |> to_erlang()

        "erlang" ->
          raise "implement!"
      end

      {:ok, ast} = Code.string_to_quoted(file)

      ast |> to_erlang()
    end
  end

  def to_erlang(ast) do
    with module <- DoppelModule.name(ast),
         body <- DoppelModule.it(ast) |> to_string(),
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

  def main2(dir_or_file) do
  end

  def dir_to_erlang(dir) do
    with files <- File.ls!(dir),
         elixir_files <- files |> Enum.filter(fn file -> file |> String.ends_with?(".ex") end) do
      elixir_files
      |> Enum.map(fn file -> File.read!(Path.join(dir, file)) end)
      |> Enum.map(fn content -> content |> Code.string_to_quoted!() end)
      |> Enum.each(fn ast -> ast |> to_erlang() end)
    end
  end

  def to_elixir(_) do
    raise "to be implemented"
  end
end
