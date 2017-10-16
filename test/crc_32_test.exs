defmodule CRC_32_Test do
  use ExUnit.Case
  use PropCheck

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  # CRC-32
  test "calculate correct CRC-32" do
    assert CRC.crc_32(@test_data_01) == 0xCBF43926
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.crc_32(large_input) == 0x82F4264E
  end

  test "calculate correct CRC-32 test data 2" do
    assert CRC.crc_32(@test_data_02) == 0x312A6AA6
  end
end
