defmodule Bench do
  def run do
    {cli_num, ""} = Integer.parse(Enum.at(System.argv(), 0))

    fib_res = sum_fibs(cli_num)

    IO.puts(fib_res)
  end

  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(n - 1) + fib(n - 2)

  def sum_fibs(n), do: sum_range(0, 0, n)

  defp sum_range(0, 0, n), do: sum_range(1, 0, n)

  defp sum_range(i, acc, n) when i < n do
    sum_range(i + 1, acc + fib(i), n)
  end

  defp sum_range(_, acc, _), do: acc
end
