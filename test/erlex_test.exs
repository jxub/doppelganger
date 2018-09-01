defmodule ErlexTest do
  use ExUnit.Case
  doctest Doppelganger

  test "greets the world" do
    assert Doppelganger.hello() == :world
  end
end
