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

      assert CRC.Pure.calculate(@test_data, model) == :crc_fast.calc(model, @test_data)
    end

    test "extend accepts a resource, a params map or a nested extend" do
      custom = %{width: 16, poly: 0x1021, init: 0xFFFF, refin: false, refout: false, xorout: 0}

      for {pure_base, nif_base} <- [
            {CRC.Pure.init(:crc_32), CRC.init(:crc_32)},
            {CRC.Pure.init(custom), CRC.init(custom)},
            {custom, custom},
            {%{extend: :crc_32, xorout: 0}, %{extend: :crc_32, xorout: 0}}
          ] do
        assert CRC.Pure.calculate(@test_data, %{extend: pure_base, init: 0x1D0F}) ==
                 CRC.calculate(@test_data, %{extend: nif_base, init: 0x1D0F})
      end
    end

    test "custom models are not cached" do
      # warm the caches for built-in models
      for {key, _name} <- CRC.Pure.list(), do: CRC.Pure.init(key)
      before = pure_cache_keys()

      for poly <- 1..200 do
        model = %{width: 32, poly: poly * 2 + 1, init: 0, refin: false, refout: false, xorout: 0}
        CRC.Pure.calculate(@test_data, model)
        CRC.Pure.info(model)
        CRC.Pure.residue(model)
      end

      assert pure_cache_keys() -- before == []
    end

    test "clear_cache/0 removes every cached model" do
      CRC.Pure.init(:crc_32)
      CRC.Pure.init(:crc_16)
      assert pure_cache_keys() != []

      assert CRC.Pure.clear_cache() == :ok
      assert pure_cache_keys() == []

      # models are rebuilt on next use
      assert CRC.Pure.calculate(@test_data, :crc_32) == 0xCBF43926
    end

    test "clear_cache/1 removes only that model, by name or alias" do
      CRC.Pure.clear_cache()
      CRC.Pure.init(:crc_32)
      CRC.Pure.init(:crc_16)

      # :pkzip is an alias of :crc_32
      assert CRC.Pure.clear_cache(:pkzip) == :ok

      assert pure_cache_keys() |> Enum.all?(&(:crc_32 not in Tuple.to_list(&1)))
      assert {:crc_pure, :model, :crc_16} in pure_cache_keys()
      assert CRC.Pure.clear_cache(:crc_32) == :ok

      # an extend of the cleared model rebuilds its table
      assert CRC.Pure.calculate(@test_data, %{extend: :crc_32, xorout: 0}) ==
               CRC.calculate(@test_data, %{extend: :crc_32, xorout: 0})

      assert_raise ArgumentError, fn -> CRC.Pure.clear_cache(:not_a_real_model) end
    end

    test "clear_cache/1 keeps a table shared with another cached model" do
      CRC.Pure.clear_cache()
      # :crc_16 and :crc_16_modbus share a width, polynomial and reflection
      CRC.Pure.init(:crc_16_modbus)
      CRC.Pure.init(:crc_16)
      CRC.Pure.clear_cache(:crc_16)

      assert CRC.Pure.calculate(@test_data, :crc_16_modbus) ==
               CRC.calculate(@test_data, :crc_16_modbus)

      assert CRC.Pure.calculate(@test_data, :crc_16) == 0xBB3D
    end

    test "init with extend" do
      extended = %{extend: :crc_16_ccitt_false, init: 0x1D0F}
      assert CRC.Pure.calculate(@test_data, extended) == :crc_fast.calc(extended, @test_data)
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
      for {key, _name} <- CRC.Pure.list() do
        info = CRC.Pure.info(key)

        assert CRC.Pure.calculate(@test_data, key) == info.check,
               "check mismatch for #{key}"
      end
    end
  end

  describe "list/0" do
    test "returns a non-empty list of {key, name} tuples" do
      models = CRC.Pure.list()
      assert is_list(models)
      assert models != []
      assert Enum.all?(models, fn {key, name} -> is_atom(key) and is_binary(name) end)
    end

    test "matches CRC.list/0" do
      assert Enum.sort(CRC.Pure.list()) == Enum.sort(CRC.list())
    end

    test "filters by key or name" do
      assert CRC.Pure.list("^crc_32$") == [{:crc_32, "CRC-32"}]
      assert {:crc_16_modbus, "CRC-16/MODBUS"} in CRC.Pure.list("MODBUS")
    end

    test "all models are initializable" do
      for {key, _name} <- CRC.Pure.list() do
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
      # Hide the literal from the type checker, which would otherwise warn
      # that this intentionally-invalid map can never match a valid clause.
      width = Enum.random(["16"])

      assert_raise ArgumentError, fn ->
        CRC.Pure.init(%{
          width: width,
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
        CRC.Pure.calculate(input, model) === :crc_fast.calc(model, input)
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
        CRC.Pure.calculate(input, model) === :crc_fast.calc(model, input)
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
        CRC.Pure.calculate(input, model) === :crc_fast.calc(model, input)
      end
    end

    property "streaming matches one-shot for all models" do
      models = Map.keys(:crc_nif.crc_list())

      forall {model, part1, part2} in {oneof(models), binary(), binary()} do
        one_shot = CRC.Pure.calculate(<<part1::binary, part2::binary>>, model)

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
          pure_result = CRC.Pure.calculate(input, model)
          multipart_result = MultiPart.calculate(CRC.Pure, model, input, split_at)

          if pure_result === crc_le do
            multipart_result === crc_le
          else
            size =
              if rem(widths[model], 8) != 0 do
                sizes[model]
              else
                widths[model]
              end

            crc_le_bin = <<crc_le::unsigned-little-integer-unit(1)-size(size)>>
            <<crc_be::unsigned-big-integer-unit(1)-size(^size)>> = crc_le_bin
            pure_result === crc_be and multipart_result === crc_be
          end
        end
      end
    end
  end

  defp pure_cache_keys do
    for {key, _} <- :persistent_term.get(),
        is_tuple(key) and elem(key, 0) == :crc_pure,
        do: key
  end
end
