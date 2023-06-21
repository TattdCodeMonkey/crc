defmodule ChecksumXOR do
  import Bitwise

  def calc(input) do
    calc(input, 0)
  end

  @doc false
  defp calc(<<>>, sum) do
    sum
  end
  defp calc(<< c, rest :: binary() >>, sum) do
    calc(rest, bxor(sum, c))
  end

end
