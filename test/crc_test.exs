defmodule CRCTest do
  use ExUnit.Case
  doctest CRC

  defp test_data, do: <<0x01, 0x02, 0x44, 0xFA, 0x82, 0x1A, 0xD5>>

  test "calculate correct CRC-16 ccitt with default 0xFFFF" do
    assert CRC.ccitt_16(test_data) == 0x8F3D
  end

  test "calculate correct CRC-16 ccitt with 0x1D0F" do
    assert CRC.ccitt_16(test_data, 0x1D0F) == 0x6681
  end

  test "calculate correct CRC-16 ccitt 0x1D0F" do
    assert CRC.ccitt_16_1D0F(test_data) == 0x6681
  end

  test "calculate correct CRC-16 ccitt xmodem" do
    assert CRC.ccitt_16_xmodem(test_data) == 0x7EF3
  end

  # test "calculate correct CRC-16" do
  #   assert CRC.crc_16(test_data) == 0x22BB
  # end

  test "calculate correct CRC-16 Modbus" do
    assert CRC.crc_16_modbus(test_data) == 0x22A0
  end

  # test "calculate correct CRC-16 Sick" do
  #   assert CRC.crc_16_sick(test_data) == 0x79D7
  # end
  #
  # test "calculate correct CRC-DNP" do
  #   assert CRC.crc_dnp(test_data) == 0x8DCC
  # end
end
