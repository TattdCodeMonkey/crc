defmodule CRC8KOOP do
  use Bitwise

  # Adapted from ./pycrc.py --generate=c --algorithm=bbb --model=crc-8-koop

  def calc(input) do
    init()
    |> update(input)
    |> final()
  end

  def init(seed \\ 0xff) do
    seed ^^^ 0xff
  end

  def update(crc, <<>>) do
    crc &&& 0xff
  end
  def update(crc, << d, data :: binary() >>) do
    c = reflect(d, 8)
    crc = do_update(crc, c, 0)
    update(crc &&& 0xff, data)
  end

  def final(crc) do
    do_final(crc, 0)
  end

  @doc false
  defp do_final(crc, 8) do
    crc = reflect(crc, 8)
    (crc ^^^ 0xff) &&& 0xff
  end
  defp do_final(crc, i) do
    bit = crc &&& 0x80
    crc = crc <<< 1
    if bit === 0 do
      do_final(crc, i + 1)
    else
      do_final(crc ^^^ 0x4d, i + 1)
    end
  end

  @doc false
  defp do_update(crc, _c, 8) do
    crc
  end
  defp do_update(crc, c, i) do
    bit = crc &&& 0x80
    crc = (crc <<< 1) ||| ((c >>> (7 - i)) &&& 0x01)
    if bit === 0 do
      do_update(crc, c, i + 1)
    else
      do_update(crc ^^^ 0x4d, c, i + 1)
    end
  end

  @doc false
  defp reflect(data, data_len) do
    ret = data &&& 0x01
    reflect(data, data_len, 1, ret)
  end

  @doc false
  defp reflect(data, data_len, i, ret) when i < data_len do
    data = data >>> 1
    ret = (ret <<< 1) ||| (data &&& 0x01)
    reflect(data, data_len, i + 1, ret)
  end
  defp reflect(_data, _data_len, _i, ret) do
    ret
  end

end
