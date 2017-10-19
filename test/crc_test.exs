defmodule CRCTest do
  use ExUnit.Case
  use PropCheck
  doctest CRC

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  test "calculate correct CRC-8" do
    assert CRC.crc_8(@test_data_01) == 0x7B
    assert CRC.crc_8(@test_data_01) == CRC8KOOP.calc(@test_data_01)
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.crc_8(large_input) == 0xF4
    assert CRC.crc_8(large_input) == CRC8KOOP.calc(large_input)
  end
  
  test "CRC-8 test data 2" do
    assert CRC.crc_8(@test_data_02) == 0x90
    assert CRC.crc_8(@test_data_02) == CRC8KOOP.calc(@test_data_02)
  end

  test "calculate correct checksum (xor)" do
    assert CRC.checksum_xor(@test_data_01) == 0x31
    assert CRC.checksum_xor(@test_data_01) == ChecksumXOR.calc(@test_data_01)
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.checksum_xor(large_input) == 0x31
    assert CRC.checksum_xor(large_input) == ChecksumXOR.calc(large_input)
  end

  property "CRC-8/KOOP" do
    forall input in binary() do
      CRC.crc_8(input) == CRC8KOOP.calc(input)
    end
  end

  property "XOR Checksum" do
    forall input in binary() do
      CRC.checksum_xor(input) == ChecksumXOR.calc(input)
    end
  end
end
