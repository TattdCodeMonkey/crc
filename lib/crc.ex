defmodule Crc do
  def init() do
      :erlang.load_nif("./priv/crc", 0)
  end

  def calculate_16_ccitt(<<data :: binary>>) do
    _calc_16_ccitt(data)
  end

  defp _calc_16_ccitt(_data), do: "CRC16-CCITT NIF not loaded"
end
