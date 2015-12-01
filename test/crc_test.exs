defmodule CRCTest do
  use ShouldI
  doctest CRC

  setup context do
    Dict.put(context, :data, <<0x01, 0x02, 0x44, 0xFA, 0x82, 0x1A, 0xD5>>)
  end

  test("calculate correct crc with  default 0xFFFF", context) do
    assert CRC.ccitt_16(context.data) == 0x8F3D
  end

  test("calculate correct crc with 0x1D0F", context) do
    assert CRC.ccitt_16(context.data, 0x1D0F) == 0x6681
  end

  test("calculate correct crc 0x1D0F", context) do
    assert CRC.ccitt_16_1D0F(context.data) == 0x6681
  end

  test("calculate correct crc xmodem", context) do
    assert CRC.ccitt_16_xmodem(context.data) == 0x7EF3
  end
end
