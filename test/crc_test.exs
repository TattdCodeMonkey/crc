defmodule CRCTest do
  use ExUnit.Case
  use PropCheck
  import Bitwise
  doctest CRC

  @test_data_01 "123456789"
  @test_data_02 "abcdefg"

  test "calculate correct CRC-8" do
    assert apply(CRC, :crc_8, [@test_data_01]) == 0x7B
    assert apply(CRC, :crc_8, [@test_data_01]) == CRC8KOOP.calc(@test_data_01)
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert apply(CRC, :crc_8, [large_input]) == 0xF4
    assert apply(CRC, :crc_8, [large_input]) == CRC8KOOP.calc(large_input)
  end

  test "CRC-8 test data 2" do
    assert apply(CRC, :crc_8, [@test_data_02]) == 0x90
    assert apply(CRC, :crc_8, [@test_data_02]) == CRC8KOOP.calc(@test_data_02)
  end

  test "calculate correct checksum (xor)" do
    assert CRC.checksum_xor(@test_data_01) == 0x31
    assert CRC.checksum_xor(@test_data_01) == ChecksumXOR.calc(@test_data_01)
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.checksum_xor(large_input) == 0x31
    assert CRC.checksum_xor(large_input) == ChecksumXOR.calc(large_input)
  end

  test "calculate" do
    assert CRC.calculate(@test_data_01, :crc_16) == 0xBB3D
    large_input = :binary.copy(@test_data_01, 1024 * 40 + 1)
    assert CRC.calculate(large_input, :crc_16) == 0xF8F6
  end

  test "multi-part calculation" do
    streamed =
      CRC.init(:crc_32)
      |> CRC.update("1234")
      |> CRC.update(["56", "789"])
      |> CRC.final()

    assert streamed == CRC.calculate(@test_data_01, :crc_32)
  end

  test "info/1 accepts a model or a resource" do
    expected = %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFF,
      check: 0xCBF43926,
      residue: 0xDEBB20E3,
      sick: false
    }

    assert CRC.info(:crc_32) == expected
    assert CRC.info(CRC.init(:crc_32)) == expected
    assert CRC.info(:crc_32) == CRC.Pure.info(:crc_32)
  end

  test "residue/1 accepts a model or a resource" do
    assert CRC.residue(:crc_32) == 0xDEBB20E3
    assert CRC.residue(CRC.init(:crc_32)) == 0xDEBB20E3
  end

  test "list/0 returns every model with its name" do
    assert {:crc_32, "CRC-32"} in CRC.list()
    assert length(CRC.list()) == map_size(:crc_nif.crc_list())
  end

  test "list/1 filters on key or name" do
    assert CRC.list("^crc_32$") == [{:crc_32, "CRC-32"}]
    assert {:crc_16_modbus, "CRC-16/MODBUS"} in CRC.list("MODBUS")
    assert_raise ArgumentError, fn -> CRC.list("(") end
  end

  property "CRC.Pure matches CRC for the v1 API" do
    models = Enum.map(CRC.list(), &elem(&1, 0))

    forall {model, input} <- {oneof(models), binary()} do
      CRC.Pure.calculate(input, model) === CRC.calculate(input, model) and
        CRC.Pure.checksum_xor(input) === CRC.checksum_xor(input)
    end
  end

  # Each deprecated function must match the replacement named in its
  # deprecation message. They are called with apply/3 so the test suite
  # compiles without deprecation warnings.
  property "deprecated functions match their documented replacements" do
    forall {input, seed16, seed8} <- {binary(), integer(0, 0xFFFF), integer(0, 0xFF)} do
      koop_init = :crc_pure.crc_reflect(bxor(seed8, 0xFF), 8)

      [
        {apply(CRC, :crc, [:crc_32, input]), CRC.calculate(input, :crc_32)},
        {apply(CRC, :crc_final, [
           apply(CRC, :crc_update, [apply(CRC, :crc_init, [:crc_16]), input])
         ]), CRC.calculate(input, :crc_16)},
        {apply(CRC, :crc_8, [input]), CRC.calculate(input, %{extend: :crc_8_koop, init: 0})},
        {apply(CRC, :crc_8, [input, seed8]),
         CRC.calculate(input, %{extend: :crc_8_koop, init: koop_init})},
        {apply(CRC, :crc_16, [input]), CRC.calculate(input, :crc_16)},
        {apply(CRC, :ccitt_16, [input]), CRC.calculate(input, :crc_16_ccitt_false)},
        {apply(CRC, :ccitt_16, [input, seed16]),
         CRC.calculate(input, %{extend: :crc_16_ccitt_false, init: seed16})},
        {apply(CRC, :ccitt_16_kermit, [input]), CRC.calculate(input, :crc_16_kermit)},
        {apply(CRC, :ccitt_16_kermit, [input, seed16]),
         CRC.calculate(input, %{extend: :crc_16_kermit, init: seed16})},
        {apply(CRC, :ccitt_16_xmodem, [input]), CRC.calculate(input, :xmodem)},
        {apply(CRC, :ccitt_16_1D0F, [input]),
         CRC.calculate(input, %{extend: :crc_16_ccitt_false, init: 0x1D0F})},
        {apply(CRC, :crc_16_dnp, [input]), CRC.calculate(input, :crc_16_dnp)},
        {apply(CRC, :crc_16_modbus, [input]), CRC.calculate(input, :crc_16_modbus)},
        {apply(CRC, :crc_16_sick, [input]), CRC.calculate(input, :crc_16_sick)},
        {apply(CRC, :crc_32, [input]), CRC.calculate(input, :crc_32)}
      ]
      |> Enum.all?(fn {deprecated, replacement} -> deprecated === replacement end)
    end
  end

  property "CRC-8/KOOP" do
    forall input in binary() do
      apply(CRC, :crc_8, [input]) == CRC8KOOP.calc(input)
    end
  end

  property "XOR Checksum" do
    forall input in binary() do
      CRC.checksum_xor(input) == ChecksumXOR.calc(input)
    end
  end

  property "new CRC-8/KOOP matches old CRC-8/KOOP" do
    forall input in binary() do
      apply(CRC, :crc, [:crc_8_koop, input]) === apply(CRC, :crc_8, [input, 0x00])
    end
  end

  test "fast module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_fast, %{display: :failed}) == []
  end

  test "pure module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_pure, %{display: :failed}) == []
  end

  test "slow module verifies all checks" do
    assert :crc_algorithm.verify_check(:crc_slow, %{display: :failed}) == []
  end

  test "fast module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_fast, %{display: :failed}) == []
  end

  test "pure module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_pure, %{display: :failed}) == []
  end

  test "slow module verifies all residues" do
    assert :crc_algorithm.verify_residue(:crc_slow, %{display: :failed}) == []
  end

  property "matching CRC for known models" do
    models = Map.keys(:crc_nif.crc_list())

    forall {model, input} in {oneof(models), binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and
        :crc_fast.calc(model, input) === :crc_pure.calc(model, input) and
        :crc_pure.calc(model, input) === CRC.Pure.calculate(input, model)
    end
  end

  property "matching CRC for unknown models" do
    model_gen_unsafe =
      let {
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
        crc_mask = 1 ||| (msb_mask - 1) <<< 1

        %{
          width: width,
          poly: poly &&& crc_mask,
          init: init &&& crc_mask,
          refin: refin,
          refout: refout,
          xorout: xorout &&& crc_mask
        }
      end

    model_gen = such_that(%{poly: poly} <- model_gen_unsafe, when: poly > 0 and rem(poly, 2) != 0)

    forall {model, input} <- {model_gen, binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and
        :crc_fast.calc(model, input) === :crc_pure.calc(model, input) and
        :crc_pure.calc(model, input) === CRC.Pure.calculate(input, model)
    end
  end

  property "matching SICK for unknown models" do
    model_gen_unsafe =
      let {
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
        crc_mask = 1 ||| (msb_mask - 1) <<< 1

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

    model_gen = such_that(%{poly: poly} <- model_gen_unsafe, when: poly > 0 and rem(poly, 2) != 0)

    forall {model, input} <- {model_gen, binary()} do
      :crc_slow.calc(model, input) === :crc_fast.calc(model, input) and
        :crc_fast.calc(model, input) === :crc_pure.calc(model, input) and
        :crc_pure.calc(model, input) === CRC.Pure.calculate(input, model)
    end
  end

  if System.get_env("PYCRC_BIN") do
    property "verifies against pycrc" do
      models = Map.keys(:crc_nif.crc_list())
      # Remove unsupported pycrc models
      models =
        models --
          [
            :crc_16_sick
          ]

      infos = for model <- models, into: %{}, do: {model, :crc_fast.info(:crc_fast.init(model))}

      f = fn x, bits ->
        "0x" <> Base.encode16(<<x::unsigned-big-integer-unit(1)-size(bits)>>, case: :lower)
      end

      forall {model, input, split_at} in {oneof(models), binary(), non_neg_integer()} do
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
            <<"0x", rest::binary>> -> rest
            _ -> results
          end

        crc_le = :erlang.binary_to_integer(results, 16)
        fast_challenge = :crc_fast.calc(model, input)
        pure_challenge = :crc_pure.calc(model, input)
        slow_challenge = :crc_slow.calc(model, input)
        elixir_pure_challenge = CRC.Pure.calculate(input, model)
        elixir_challenge = CRC.calculate(input, model)

        multipart_challenges =
          for mod <- [:crc_pure, CRC.Pure, CRC],
              do: MultiPart.calculate(mod, model, input, split_at)

        if fast_challenge === crc_le do
          fast_challenge === crc_le and pure_challenge === crc_le and
            slow_challenge === crc_le and elixir_pure_challenge === crc_le and
            elixir_challenge === crc_le and Enum.all?(multipart_challenges, &(&1 === crc_le))
        else
          size =
            if rem(width, 8) != 0 do
              bits
            else
              width
            end

          crc_le_bin = <<crc_le::unsigned-little-integer-unit(1)-size(size)>>
          <<crc_be::unsigned-big-integer-unit(1)-size(^size)>> = crc_le_bin

          fast_challenge === crc_be and pure_challenge === crc_be and
            slow_challenge === crc_be and elixir_pure_challenge === crc_be and
            elixir_challenge === crc_be and Enum.all?(multipart_challenges, &(&1 === crc_be))
        end
      end
    end
  end

  if System.get_env("REVENG_BIN") do
    property "verifies against reveng" do
      models = Map.keys(:crc_nif.crc_list())
      # Remove unsupported RevEng models
      models =
        models --
          [
            :crc_8_koop,
            :crc_16_sick,
            :crc_64_jones
          ]

      infos = for model <- models, into: %{}, do: {model, :crc_fast.info(:crc_fast.init(model))}

      names =
        for {model, %{name: name}} <- infos, into: %{} do
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

      sizes = for {model, %{bits: bits}} <- infos, into: %{}, do: {model, bits}
      widths = for {model, %{width: width}} <- infos, into: %{}, do: {model, width}

      forall {model, input, split_at} in {oneof(models), binary(), non_neg_integer()} do
        command =
          "#{System.get_env("REVENG_BIN")} -c -m \"#{names[model]}\" \"#{Base.encode16(input)}\""

        results =
          command
          |> :erlang.binary_to_list()
          |> :os.cmd()
          |> :erlang.list_to_binary()
          |> String.trim()

        results =
          case results do
            <<"0x", rest::binary>> -> rest
            _ -> results
          end

        crc_le = :erlang.binary_to_integer(results, 16)
        fast_challenge = :crc_fast.calc(model, input)
        pure_challenge = :crc_pure.calc(model, input)
        slow_challenge = :crc_slow.calc(model, input)
        elixir_pure_challenge = CRC.Pure.calculate(input, model)

        multipart_challenges =
          for mod <- [:crc_pure, CRC.Pure, CRC],
              do: MultiPart.calculate(mod, model, input, split_at)

        if fast_challenge === crc_le do
          fast_challenge === crc_le and pure_challenge === crc_le and
            slow_challenge === crc_le and elixir_pure_challenge === crc_le and
            Enum.all?(multipart_challenges, &(&1 === crc_le))
        else
          size =
            if rem(widths[model], 8) != 0 do
              sizes[model]
            else
              widths[model]
            end

          crc_le_bin = <<crc_le::unsigned-little-integer-unit(1)-size(size)>>
          <<crc_be::unsigned-big-integer-unit(1)-size(^size)>> = crc_le_bin

          fast_challenge === crc_be and pure_challenge === crc_be and
            slow_challenge === crc_be and elixir_pure_challenge === crc_be and
            Enum.all?(multipart_challenges, &(&1 === crc_be))
        end
      end
    end
  end
end
