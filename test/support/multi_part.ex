defmodule MultiPart do
  @moduledoc false

  # Calculates the CRC of `input` with `mod.init/1`, `mod.update/2` and
  # `mod.final/1`, feeding the input in two parts split at `split_at`
  # (wrapped to the input size, so any non-negative integer is valid).
  def calculate(mod, model, input, split_at) do
    at = rem(split_at, byte_size(input) + 1)
    <<first::binary-size(^at), rest::binary>> = input

    model
    |> mod.init()
    |> mod.update(first)
    |> mod.update(rest)
    |> mod.final()
  end
end
