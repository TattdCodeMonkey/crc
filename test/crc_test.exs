defmodule CRCTest do
  use ExUnit.Case
  doctest CRC

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  test "calculate correct CRC-8" do
    assert CRC.crc_8(@test_data_01) == 0x7B
  end
  
  test "CRC-8 test data 2" do
    assert CRC.crc_8(@test_data_02) == 0x90
  end

  test "calculate correct checksum (xor)" do
    assert CRC.checksum_xor(@test_data_01) == 0x31
  end
end
