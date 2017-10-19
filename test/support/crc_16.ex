defmodule CRC16 do
  use Bitwise

  @crc16 0x8005

  def calc(input) do
    init()
    |> update(input)
    |> final()
  end

  def init() do
    0x00
  end

  def update(crc, <<>>) do
    crc &&& 0xffff
  end
  def update(crc, << c, buf :: binary() >>) do
    update(do_update(crc, c, 0), buf)
  end

  def final(crc) do
    do_final(crc, 0)
  end

  @doc false
  defp do_final(crc, 16) do
    crc = crc &&& 0xffff
    do_final_reverse(crc, 0x8000, 0x0001, 0)
  end
  defp do_final(crc, i) do
    flag = crc >>> 15
    crc = (crc <<< 1) &&& 0xffff
    if flag === 0 do
      do_final(crc, i + 1)
    else
      do_final(crc ^^^ @crc16, i + 1)
    end
  end

  @doc false
  defp do_final_reverse(_crc, 0, _j, acc) do
    acc &&& 0xffff
    # acc
  end
  defp do_final_reverse(crc, i, j, acc) do
    if (i &&& crc) === 0 do
      do_final_reverse(crc, i >>> 1, j <<< 1, acc)
    else
      do_final_reverse(crc, i >>> 1, j <<< 1, acc ||| j)
    end
  end

  @doc false
  defp do_update(crc, _c, 8) do
    crc &&& 0xffff
  end
  defp do_update(crc, c, read) do
    flag = crc >>> 15
    crc = ((crc <<< 1) &&& 0xffff) ||| ((c >>> read) &&& 0x01)
    if flag === 0 do
      do_update(crc, c, read + 1)
    else
      do_update(crc ^^^ @crc16, c, read + 1)
    end
  end

end
