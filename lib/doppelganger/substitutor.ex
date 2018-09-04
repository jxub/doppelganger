defmodule Doppelganger.Substitutor do
  use Task

  def start_link(arg) do
    Task.start_link(__MODULE__, :run, [arg])
  end

  def run({key, value, ast}) do
    # replace the attribute before converting to erlang
    nil
  end
end
