@type literal() :: {:const, number()} | {:const, atom()} | {:var, atom()}
@type expr() :: {:add, expr(), expr()} | {:mul, expr(), expr()} | {:exp, expr(), expr()} | literal()

defmodule Derivative do
    def deriv({:const, _}, _), do: 0
    def deriv({:var, v}, v), do: 1
    def deriv({:var, y}, _), do: 0
    def deriv({:mul, e1, e2}, v), do: {:add, {:mul, deriv(e1, v), e2}, {:mul, e1, deriv(e2, v)}}
    def deriv({:add, e1, e2}, v), do: {:add, deriv(e1, v), deriv(e2, v)}
end