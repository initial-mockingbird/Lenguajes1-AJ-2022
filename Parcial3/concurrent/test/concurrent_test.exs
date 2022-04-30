defmodule ConcurrentTest do
  use ExUnit.Case
  doctest Concurrent

  test "greets the world" do
    assert Concurrent.hello() == :world
  end
end
