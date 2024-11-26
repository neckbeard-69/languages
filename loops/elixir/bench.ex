defmodule Bench do
  def run do
    # Get an input number from the command line
    {cliNum, ""} = Integer.parse(Enum.at(System.argv(), 0))

    # Get a random number 0 <= r < 10k
    random_num = 0..10_000 |> Enum.random()

    result = outer_loop(random_num, cli_num)

    # Print a single element of the array
    IO.puts(elem(result, random_num))
  end

  # This is the outer loop via recursion

  defp inner_loop(cli_num) do
    inner_loop_body(0, 0, cli_num)
  end

  defp inner_loop_body(0, 0, cli_num) do
    inner_loop_body(1, 0, cli_num)
  end

  # 100k inner loop iterations, per outer loop iteration
  defp inner_loop_body(i, acc, cli_num) when i < 100_000 do
    # Simple sum
    inner_loop_body(i + 1, rem(i, cli_num) + acc, cli_num)
  end

  defp inner_loop_body(_, acc, _) do
    acc
  end

  # This is the outer loop via recursion

  defp outer_loop(random_num, cli_num) do
    # Array of 10k elements initialized to 0
    outer_loop_body(0, Tuple.duplicate(0, 10_000), random_num, cli_num)
  end

  defp outer_loop_body(0, acc, random_num, cli_num) do
    # Add a random value to each element in array
    val = random_num + inner_loop(cli_num)
    outer_loop_body(1, Tuple.insert_at(acc, 0, val), random_num, cli_num)
  end

  # 10k outer loop iterations
  defp outer_loop_body(i, acc, random_num, cli_num) when i < 10_000 do
    # Add a random value to each element in array
    val = random_num + inner_loop(cli_num)
    outer_loop_body(i + 1, Tuple.insert_at(acc, i, val), random_num, cli_num)
  end

  defp outer_loop_body(_, acc, _, _) do
    acc
  end
end
