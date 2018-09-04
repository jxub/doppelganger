defmodule Doppelganger.Supervisor do
  use Supervisor

  alias Doppelganger.{
    Finder,
    Substitutor
  }

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init([]) do
    children = [
      {Finder, []},
      {Substitutor, []}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
