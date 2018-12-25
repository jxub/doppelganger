-module(data_example).

-export([
    add/2,
    demo/0,
    is_val?/1,
    sum_list/1
]).

-behaviour(gen_server).

add(A, B) -> 
    a + b. 

is_val?(V) -> 
    (,
  a = v + 2,
  a1 = a * 2,
  a1,
). 

sum_list(L) -> 
    l |> List.foldl(0, fn el, acc -> el + acc end). 

demo() -> 
    1. 

private(C) -> 
    {:ok, c}. 
