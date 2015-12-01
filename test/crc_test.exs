defmodule CRCTest do
  use ExUnit.Case
  doctest CRC

  defp test_data, do: <<0x01, 0x02, 0x44, 0xFA, 0x82, 0x1A, 0xD5>>

  test "calculate correct crc with  default 0xFFFF" do
    assert CRC.ccitt_16(test_data) == 0x8F3D
  end

  test "calculate correct crc with 0x1D0F" do
    assert CRC.ccitt_16(test_data, 0x1D0F) == 0x6681
  end

  test "calculate correct crc 0x1D0F" do
    assert CRC.ccitt_16_1D0F(test_data) == 0x6681
  end

  test "calculate correct crc xmodem" do
    assert CRC.ccitt_16_xmodem(test_data) == 0x7EF3
  end
end
