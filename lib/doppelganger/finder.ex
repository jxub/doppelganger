defmodule Doppelganger.Finder do
  use Task

  def start_link(arg) do
    Task.start_link(__MODULE__, :run, [arg])
  end

  def run(arg) do
    # ...
  end

  def run({elem, code}) do
    
  end

  def run({:struct, code}) do
    
  end
end