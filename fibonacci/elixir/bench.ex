defmodule Bench do
  def run do
    cli_num = System.argv() |> Enum.at(0) |> String.to_integer()
    fib_res = sum_fibs(cli_num)
    IO.puts(fib_res)
  end

  defp sum_fibs(i \\ 0, max, total \\ 0)
  defp sum_fibs(i, max, total) when i < max, do: sum_fibs(i + 1, max, total + fib(i))
  defp sum_fibs(_i, _max, total), do: total

  defp fib(0), do: 0
  defp fib(1), do: 1
  defp fib(n), do: fib(n - 1) + fib(n - 2)
end
