defmodule CRC_16_Test do
  use ExUnit.Case
  use PropCheck

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  # ANSI CRC-16
  test "calculate correct CRC-16" do
    assert CRC.crc_16(@test_data_01) == 0xBB3D
    assert CRC.crc_16(@test_data_01) == CRC16.calc(@test_data_01)
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.crc_16(large_input) == 0xF8F6
    assert CRC.crc_16(large_input) == CRC16.calc(large_input)
  end

  test "calculate correct CRC-16- test data 2" do
    assert CRC.crc_16(@test_data_02) == 0xE9D9
    assert CRC.crc_16(@test_data_02) == CRC16.calc(@test_data_02)
  end

  property "ANSI CRC-16" do
    forall input in binary() do
      CRC.crc_16(input) == CRC16.calc(input)
    end
  end

  # CCITT
  test "calculate correct CRC-16 ccitt with default 0xFFFF" do
    assert CRC.ccitt_16(@test_data_01) == 0x29B1
  end

  test "calculate correct CRC-16 ccitt with default 0xFFFF - test data 2" do
    assert CRC.ccitt_16(@test_data_02) == 0x8796
  end

  test "calculate correct CRC-16 ccitt with 0x1D0F" do
    assert CRC.ccitt_16(@test_data_01, 0x1D0F) == 0xE5CC
  end

  test "calculate correct CRC-16 ccitt with 0x1D0F - test data 2" do
    assert CRC.ccitt_16(@test_data_02, 0x1D0F) == 0x6E2A
  end

  test "calculate correct CRC-16 ccitt xmodem" do
    assert CRC.ccitt_16_xmodem(@test_data_01) == 0x31C3
  end

  test "calculate correct CRC-16 ccitt xmodem - test data 2" do
    assert CRC.ccitt_16_xmodem(@test_data_02) == 0x7658
  end

  # Kermit
  test "calculate correct CRC-16 kermit - test data 1" do
    assert CRC.ccitt_16_kermit(@test_data_01) == 0x2189
  end

  test "calculate correct CRC-16 kermit - test data 2" do
    assert CRC.ccitt_16_kermit(@test_data_02) == 0xf90c
  end

  # Modbus
  test "calculate correct CRC-16 Modbus" do
    assert CRC.crc_16_modbus(@test_data_01) == 0x4B37
  end

  test "calculate correct CRC-16 Modbus - test data 2" do
    assert CRC.crc_16_modbus(@test_data_02) == 0xE9C2
  end

  # Sick
  test "calcuate correct CRC-16 Sick" do
    assert CRC.crc_16_sick(@test_data_01) == 0x56A6
  end

  test "calculate correct CRC-16 Sick - test data 2" do
    assert CRC.crc_16_sick(@test_data_02) == 0x1CB4
  end
end
