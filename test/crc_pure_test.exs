defmodule CRCPureTest do
  use ExUnit.Case
  use PropCheck
  import Bitwise

  doctest CRC.Pure

  @test_data "123456789"

  # -- Functionality -----------------------------------------------------------

  describe "calculate/2" do
    test "CRC-32 with known input" do
      assert CRC.Pure.calculate(@test_data, :crc_32) == 0xCBF43926
    end

    test "CRC-16 with known input" do
      assert CRC.Pure.calculate(@test_data, :crc_16) == 0xBB3D
    end

    test "CRC-8 with known input" do
      assert CRC.Pure.calculate(@test_data, :crc_8) == 0xF4
    end

    test "CRC-64/XZ with known input" do
      assert CRC.Pure.calculate(@test_data, :crc_64_xz) == 0x995DC9BBDF1939FA
    end

    test "SICK CRC with known input" do
      assert CRC.Pure.calculate(@test_data, :crc_16_sick) == 0x56A6
    end

    test "with large input" do
      large_input = :binary.copy(@test_data, 1024 * 40 + 1)
      assert CRC.Pure.calculate(large_input, :crc_32) == :crc_fast.calc(:crc_32, large_input)
    end

    test "with empty binary" do
      assert CRC.Pure.calculate(<<>>, :crc_32) == :crc_fast.calc(:crc_32, <<>>)
    end

    test "with iolist" do
      iolist = ["1234", "56789"]
      assert CRC.Pure.calculate(iolist, :crc_32) == CRC.Pure.calculate(@test_data, :crc_32)
    end
  end

  describe "init/1 and multi-part calculation" do
    test "streaming produces same result as one-shot" do
      one_shot = CRC.Pure.calculate(@test_data, :crc_32)

      streamed =
        CRC.Pure.init(:crc_32)
        |> CRC.Pure.update("1234")
        |> CRC.Pure.update("56789")
        |> CRC.Pure.final()

      assert streamed == one_shot
    end

    test "byte-at-a-time produces same result" do
      one_shot = CRC.Pure.calculate(@test_data, :crc_16)

      resource = CRC.Pure.init(:crc_16)

      resource =
        @test_data
        |> :binary.bin_to_list()
        |> Enum.reduce(resource, fn byte, acc -> CRC.Pure.update(acc, <<byte>>) end)

      assert CRC.Pure.final(resource) == one_shot
    end

    test "init with custom model map" do
      model = %{
        width: 16,
        poly: 0x8005,
        init: 0x0000,
        refin: true,
        refout: true,
        xorout: 0x0000
      }

      assert CRC.Pure.calc(model, @test_data) == :crc_fast.calc(model, @test_data)
    end

    test "init with extend" do
      extended = %{extend: :crc_16_ccitt_false, init: 0x1D0F}
      assert CRC.Pure.calc(extended, @test_data) == :crc_fast.calc(extended, @test_data)
    end
  end

  describe "info/1" do
    test "returns model parameters from atom" do
      info = CRC.Pure.info(:crc_32)
      assert info.width == 32
      assert info.poly == 0x04C11DB7
      assert info.init == 0xFFFFFFFF
      assert info.refin == true
      assert info.refout == true
      assert info.xorout == 0xFFFFFFFF
    end

    test "returns model parameters from resource" do
      resource = CRC.Pure.init(:crc_16)
      info = CRC.Pure.info(resource)
      assert info.width == 16
    end

    test "returns sick flag" do
      info = CRC.Pure.info(:crc_16_sick)
      assert info.sick == true
    end
  end

  describe "residue/1" do
    test "CRC-32 residue" do
      assert CRC.Pure.residue(:crc_32) == 0xDEBB20E3
    end

    test "matches check value for all models" do
      for key <- CRC.Pure.list() do
        info = CRC.Pure.info(key)

        assert CRC.Pure.calc(key, @test_data) == info.check,
               "check mismatch for #{key}"
      end
    end
  end

  describe "list/0" do
    test "returns a non-empty list of atoms" do
      models = CRC.Pure.list()
      assert is_list(models)
      assert length(models) > 0
      assert Enum.all?(models, &is_atom/1)
    end

    test "all models are initializable" do
      for key <- CRC.Pure.list() do
        resource = CRC.Pure.init(key)
        assert is_map(resource), "failed to init #{key}"
      end
    end
  end

  # -- Bad parameters ----------------------------------------------------------

  describe "init/1 with bad parameters" do
    test "raises on unknown atom" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(:not_a_real_model)
      end
    end

    test "raises on unknown extend base" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{extend: :not_a_real_model, init: 0x00})
      end
    end

    test "raises on non-integer width" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: "16",
          poly: 0x8005,
          init: 0x0000,
          refin: true,
          refout: true,
          xorout: 0x0000
        })
      end
    end

    test "raises on negative width" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: -1,
          poly: 0x8005,
          init: 0x0000,
          refin: true,
          refout: true,
          xorout: 0x0000
        })
      end
    end

    test "raises on non-boolean refin" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: 16,
          poly: 0x8005,
          init: 0x0000,
          refin: 1,
          refout: true,
          xorout: 0x0000
        })
      end
    end

    test "raises on non-boolean refout" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: 16,
          poly: 0x8005,
          init: 0x0000,
          refin: true,
          refout: "yes",
          xorout: 0x0000
        })
      end
    end

    test "raises on missing required keys" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{width: 16, poly: 0x8005})
      end
    end

    test "raises on poly exceeding width" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: 8,
          poly: 0xFFFF,
          init: 0x00,
          refin: false,
          refout: false,
          xorout: 0x00
        })
      end
    end

    test "raises on non-map, non-atom input" do
      assert_raise ArgumentError, fn ->
        CRC.Pure.init(42)
      end
    end
  end

  # -- Property tests ----------------------------------------------------------

  describe "properties" do
    property "matches crc_fast for all known models" do
      models = Map.keys(:crc_nif.crc_list())

      forall {model, input} in {oneof(models), binary()} do
        CRC.Pure.calc(model, input) === :crc_fast.calc(model, input)
      end
    end

    property "matches crc_fast for random custom models" do
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

      model_gen =
        such_that(%{poly: poly} <- model_gen_unsafe, when: poly > 0 and rem(poly, 2) != 0)

      forall {model, input} <- {model_gen, binary()} do
        CRC.Pure.calc(model, input) === :crc_fast.calc(model, input)
      end
    end

    property "matches crc_fast for random SICK models" do
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

      model_gen =
        such_that(%{poly: poly} <- model_gen_unsafe, when: poly > 0 and rem(poly, 2) != 0)

      forall {model, input} <- {model_gen, binary()} do
        CRC.Pure.calc(model, input) === :crc_fast.calc(model, input)
      end
    end

    property "streaming matches one-shot for all models" do
      models = Map.keys(:crc_nif.crc_list())

      forall {model, part1, part2} in {oneof(models), binary(), binary()} do
        one_shot = CRC.Pure.calc(model, <<part1::binary, part2::binary>>)

        streamed =
          model
          |> CRC.Pure.init()
          |> CRC.Pure.update(part1)
          |> CRC.Pure.update(part2)
          |> CRC.Pure.final()

        one_shot === streamed
      end
    end
  end

  # -- RevEng verification ----------------------------------------------------

  if System.get_env("REVENG_BIN") do
    describe "reveng verification" do
      property "CRC.Pure matches reveng for all known models" do
        models = Map.keys(:crc_nif.crc_list())

        # Remove unsupported RevEng models
        models =
          models --
            [
              :crc_8_koop,
              :crc_16_sick,
              :crc_64_jones
            ]

        infos =
          for model <- models, into: %{}, do: {model, :crc_fast.info(:crc_fast.init(model))}

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

        forall {model, input} in {oneof(models), binary()} do
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
          pure_result = CRC.Pure.calc(model, input)

          if pure_result === crc_le do
            true
          else
            size =
              if rem(widths[model], 8) != 0 do
                sizes[model]
              else
                widths[model]
              end

            crc_le_bin = <<crc_le::unsigned-little-integer-unit(1)-size(size)>>
            <<crc_be::unsigned-big-integer-unit(1)-size(size)>> = crc_le_bin
            pure_result === crc_be
          end
        end
      end
    end
  end
end
