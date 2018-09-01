defmodule Doppelganger.Util do
  def default(val, default) do
    if not val do
      default
    else
      val
    end
  end
end
