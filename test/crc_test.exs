defmodule CRCTest do
  use ExUnit.Case
  doctest CRC

  defp test_data, do: "123456789"

  test "calculate correct CRC-16 ccitt with default 0xFFFF" do
    assert CRC.ccitt_16(test_data) == 0x29B1
  end

  test "calculate correct CRC-16 ccitt with 0x1D0F" do
    assert CRC.ccitt_16(test_data, 0x1D0F) == 0xE5CC
  end

  test "calculate correct CRC-16 ccitt 0x1D0F" do
    assert CRC.ccitt_16_1D0F(test_data) == 0xE5CC
  end

  test "calculate correct CRC-16 ccitt xmodem" do
    assert CRC.ccitt_16_xmodem(test_data) == 0x31C3
  end

  test "calculate correct CRC-16" do
    assert CRC.crc_16(test_data) == 0xBB3D
  end

  test "calculate correct CRC-16 Modbus" do
    assert CRC.crc_16_modbus(test_data) == 0x4B37
  end

  # test "calculate correct CRC-16 Sick" do
  #   assert CRC.crc_16_sick(test_data) == 0x56A6
  # end
  #
  # test "calculate correct CRC-DNP" do
  #   assert CRC.crc_dnp(test_data) == 0x82EA
  # end

  test "calculate correct checksum (xor)" do
    assert CRC.checksum_xor(test_data) == 0x31
  end
end
