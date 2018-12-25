defmodule Doppelganger.Scope do
  use GenServer

  # Client

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def add(pid, variable) do
    GenServer.call(pid, {:add, variable})
  end

  def remove(pid, variable) do
    GenServer.call(pid, {:remove, variable})
  end

  # Server (callbacks)

  @impl true
  def init([]) do
    {:ok, []}
  end

  @impl true
  def handle_call({:add, variable}, _from, state) do
    {:reply, :added, [variable | state]}
  end

  @impl true
  def handle_call({:remove, variable}, _from, state) do
    with state <- state |> List.delete(variable) do
      {:reply, :removed, state}
    end
  end
end
