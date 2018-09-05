defmodule Doppelganger.Parse.DBehaviour do
  defstruct [:erl_behaviour]

  def it({:use, _line, [{:__aliases__, _line1, [behaviour]}]}) do
    behaviour
    |> implements?()
    |> translate()
  end

  def implements?(behaviour) do
    # TODO: check if there are all required callbacks
    # get public functions with finder
    # else raise detailed exception
    behaviour
  end


  defp get_requred_callbacks(:GenServer) do
    # callback function: arity
    [handle_call: 3, handle_cast: 2, init: 1]
  end

  defp get_requred_callbacks(:GenEvent) do
    [handle_call: 2, handle_event: 2, init: 1]
  end

  defp get_requred_callbacks(:gen_statem) do
    [init: 1, callback_mode: 0]
  end

  defp get_requred_callbacks(:gen_fsm) do
    [handle_event: 3, handle_sync_event: 4, init: 1]
  end

  defp get_requred_callbacks(other) do
    raise "#{other} callback checking not implemented yet"
  end

  defimpl String.Chars do
    def to_string(%{erl_behaviour: erl_behaviour}) do
      "-behaviour(#{erl_behaviour}).\n"
    end
  end

  def translate(:GenServer), do: %__MODULE__{erl_behaviour: :gen_server}
  def translate(:GenEvent), do: %__MODULE__{erl_behaviour: :gen_event}
  def translate(:gen_statem), do: %__MODULE__{erl_behaviour: :gen_statem}
  def translate(:gen_fsm), do: %__MODULE__{erl_behaviour: :gen_fsm}
  def translate(:gen_sctp), do: %__MODULE__{erl_behaviour: :gen_sctp}
  def translate(:gen_tcp), do: %__MODULE__{erl_behaviour: :gen_tcp}
  def translate(:gen_udp), do: %__MODULE__{erl_behaviour: :gen_udp}
  def translate(:gen_server), do: %__MODULE__{erl_behaviour: :gen_server}
end
