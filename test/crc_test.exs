defmodule CRCTest do
  use ExUnit.Case
  use PropCheck
  use Bitwise
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

  # TODO: remove the "new matches old" property tests once old implementations have been removed

  property "new CRC-8/KOOP matches old CRC-8/KOOP" do
    forall input in binary() do
      CRC.crc(:crc_8_koop, input) === CRC.crc_8(input, 0x00)
    end
  end

  property "new CRC-16 matches old CRC-16" do
    forall input in binary() do
      CRC.crc(:crc_16, input) === CRC.crc_16(input)
    end
  end

  property "new CRC-16/AUG-CCITT matches old CRC-16/AUG-CCITT" do
    forall input in binary() do
      CRC.crc(:crc_16_aug_ccitt, input) === CRC.ccitt_16_1D0F(input)
    end
  end

  property "new CRC-16/CCITT-FALSE matches old CRC-16/CCITT-FALSE" do
    forall input in binary() do
      CRC.crc(:crc_16_ccitt_false, input) === CRC.ccitt_16(input)
    end
  end

  # property "new DNP matches old DNP" do
  #   forall input in binary() do
  #     CRC.crc(:crc_16_dnp, input) === CRC.crc_16_dnp(input)
  #   end
  # end

  property "new KERMIT matches old KERMIT" do
    forall input in binary() do
      CRC.crc(:kermit, input) === CRC.ccitt_16_kermit(input)
    end
  end

  property "new MODBUS matches old MODBUS" do
    forall input in binary() do
      CRC.crc(:modbus, input) === CRC.crc_16_modbus(input)
    end
  end

  property "new SICK matches old SICK" do
    forall input in binary() do
      CRC.crc(:sick, input) === CRC.crc_16_sick(input)
    end
  end

  property "new XMODEM matches old XMODEM" do
    forall input in binary() do
      CRC.crc(:xmodem, input) === CRC.ccitt_16_xmodem(input)
    end
  end

  property "new CRC-32 matches old CRC-32" do
    forall input in binary() do
      CRC.crc(:crc_32, input) === CRC.crc_32(input)
    end
  end

  test "fast module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_fast, %{ display: :failed }) == []
  end

  test "pure module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_pure, %{ display: :failed }) == []
  end

  test "slow module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_slow, %{ display: :failed }) == []
  end

  test "fast module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_fast, %{ display: :failed }) == []
  end

  test "pure module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_pure, %{ display: :failed }) == []
  end

  test "slow module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_slow, %{ display: :failed }) == []
  end

  property "matching CRC for known models" do
    models = Map.keys(:crc_nif.crc_list())
    forall {model, input} in {oneof(models), binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and :crc_fast.calc(model, input) === :crc_pure.calc(model, input)
    end
  end

  property "matching CRC for unknown models" do
    model_gen_unsafe = let {
      width,
      poly,
      init,
      refin,
      refout,
      xorout
    } <- {
      integer(1, 64),
      such_that(n <- integer(), when: n > 0),
      integer(),
      boolean(),
      boolean(),
      integer()
    } do
      msb_mask = 1 <<< (width - 1)
      crc_mask = 1 ||| ((msb_mask - 1) <<< 1)
      %{
        width: width,
        poly: poly &&& crc_mask,
        init: init &&& crc_mask,
        refin: refin,
        refout: refout,
        xorout: xorout &&& crc_mask
      }
    end
    model_gen = such_that(%{ poly: poly } <- model_gen_unsafe, when: (poly > 0) and rem(poly, 2) != 0)
    forall {model, input} <- {model_gen, binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and :crc_fast.calc(model, input) === :crc_pure.calc(model, input)
    end
  end

  property "matching SICK for unknown models" do
    model_gen_unsafe = let {
      width,
      poly,
      init,
      refin,
      refout,
      xorout
    } <- {
      return(16),
      such_that(n <- integer(), when: n > 0),
      integer(),
      boolean(),
      boolean(),
      integer()
    } do
      msb_mask = 1 <<< (width - 1)
      crc_mask = 1 ||| ((msb_mask - 1) <<< 1)
      %{
        width: width,
        poly: poly &&& crc_mask,
        init: init &&& crc_mask,
        refin: refin,
        refout: refout,
        xorout: xorout &&& crc_mask,
        sick: true
      }
    end
    model_gen = such_that(%{ poly: poly } <- model_gen_unsafe, when: (poly > 0) and rem(poly, 2) != 0)
    forall {model, input} <- {model_gen, binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and :crc_fast.calc(model, input) === :crc_pure.calc(model, input)
    end
  end
end
