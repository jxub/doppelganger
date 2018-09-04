defmodule Doppelganger.Util do
  def default(val, default) do
    if not val do
      default
    else
      val
    end
  end

  def parse_error(module) do
    "remember to call it/1 in order to parse before applying to_string/1 to #{module}"
  end
end
