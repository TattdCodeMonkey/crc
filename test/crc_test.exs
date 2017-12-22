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

  property "new CRC-16/CCITT-FALSE matches old CRC-16/CCITT-FALSE for any init" do
    forall {init, input} in {integer(0, 0xffff), binary()} do
      CRC.crc(%{extend: :crc_16_ccitt_false, init: init}, input) === CRC.ccitt_16(input, init)
    end
  end

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

  if System.get_env("PYCRC_BIN") do
    property "verifies against pycrc" do
      models = Map.keys(:crc_nif.crc_list())
      # Remove unsupported pycrc models
      models = models -- [
        :crc_16_sick
      ]
      infos = for model <- models, into: %{}, do: {model, :crc_fast.info(:crc_fast.init(model))}
      f = fn (x, bits) ->
        "0x" <> Base.encode16(<< x :: unsigned-big-integer-unit(1)-size(bits) >>, case: :lower)
      end
      forall {model, input} in {oneof(models), binary()} do
        %{
          bits: bits,
          width: width,
          poly: poly,
          init: init,
          refin: refin,
          refout: refout,
          xorout: xorout
        } = infos[model]
        command = <<
          "#{System.get_env("PYCRC_BIN")}",
          " --algorithm=bbb",
          " --check-hexstring=\"#{Base.encode16(input)}\"",
          " --width=#{width}",
          " --poly=#{f.(poly, bits)}",
          " --xor-in=#{f.(init, bits)}",
          " --reflect-in=#{if refin, do: "True", else: "False"}",
          " --reflect-out=#{if refout, do: "True", else: "False"}",
          " --xor-out=#{f.(xorout, bits)}"
        >>
        results =
          command
          |> :erlang.binary_to_list()
          |> :os.cmd()
          |> :erlang.list_to_binary()
          |> String.trim()
        results =
          case results do
            << "0x", rest :: binary() >> -> rest
            _ -> results
          end
        crc_le = :erlang.binary_to_integer(results, 16)
        fast_challenge = :crc_fast.calc(model, input)
        pure_challenge = :crc_pure.calc(model, input)
        slow_challenge = :crc_slow.calc(model, input)
        if fast_challenge === crc_le do
          fast_challenge === crc_le and pure_challenge === crc_le and slow_challenge === crc_le
        else
          size =
            if rem(width, 8) != 0 do
              bits
            else
              width
            end
          crc_le_bin = << crc_le :: unsigned-little-integer-unit(1)-size(size) >>
          << crc_be :: unsigned-big-integer-unit(1)-size(size) >> = crc_le_bin
          fast_challenge === crc_be and pure_challenge === crc_be and slow_challenge === crc_be
        end
      end
    end
  end

  if System.get_env("REVENG_BIN") do
    property "verifies against reveng" do
      models = Map.keys(:crc_nif.crc_list())
      # Remove unsupported RevEng models
      models = models -- [
        :crc_8_koop,
        :crc_16_sick,
        :crc_64_jones
      ]
      infos = for model <- models, into: %{}, do: {model, :crc_fast.info(:crc_fast.init(model))}
      names = for {model, %{ name: name }} <- infos, into: %{} do
        name =
          case name do
            "CRC-16/A" -> "CRC-A"
            "CRC-16/MODBUS" -> "MODBUS"
            "CRC-16/X-25" -> "X-25"
            "CRC-16/XMODEM" -> "XMODEM"
            "CRC-32/JAMCRC" -> "JAMCRC"
            "CRC-32/XFER" -> "XFER"
            _ -> name
          end
        {model, name}
      end
      sizes = for {model, %{ bits: bits }} <- infos, into: %{}, do: {model, bits}
      widths = for {model, %{ width: width }} <- infos, into: %{}, do: {model, width}
      forall {model, input} in {oneof(models), binary()} do
        command = "#{System.get_env("REVENG_BIN")} -c -m \"#{names[model]}\" \"#{Base.encode16(input)}\""
        results =
          command
          |> :erlang.binary_to_list()
          |> :os.cmd()
          |> :erlang.list_to_binary()
          |> String.trim()
        results =
          case results do
            << "0x", rest :: binary() >> -> rest
            _ -> results
          end
        crc_le = :erlang.binary_to_integer(results, 16)
        fast_challenge = :crc_fast.calc(model, input)
        pure_challenge = :crc_pure.calc(model, input)
        slow_challenge = :crc_slow.calc(model, input)
        if fast_challenge === crc_le do
          fast_challenge === crc_le and pure_challenge === crc_le and slow_challenge === crc_le
        else
          size =
            if rem(widths[model], 8) != 0 do
              sizes[model]
            else
              widths[model]
            end
          crc_le_bin = << crc_le :: unsigned-little-integer-unit(1)-size(size) >>
          << crc_be :: unsigned-big-integer-unit(1)-size(size) >> = crc_le_bin
          fast_challenge === crc_be and pure_challenge === crc_be and slow_challenge === crc_be
        end
      end
    end
  end
end
