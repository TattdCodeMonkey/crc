defmodule CRCTest do
  use ExUnit.Case
  doctest CRC

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  test "calculate correct CRC-8" do
    assert CRC.crc_8(@test_data_01) == 0x7B
  end

  test "calculate correct checksum (xor)" do
    assert CRC.checksum_xor(@test_data_01) == 0x31
  end

  # test "calculate correct CRC-16 Sick" do
  #   assert CRC.crc_16_sick(test_data) == 0x56A6
  # end
  #
  # test "calculate correct CRC-DNP" do
  #   assert CRC.crc_dnp(test_data) == 0x82EA
  # end
end
