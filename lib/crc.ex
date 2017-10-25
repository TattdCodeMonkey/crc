defmodule CRC do
  @moduledoc """
  This module is used to calculate CRC (Cyclic Redundancy Check) values
  for binary data. It uses NIF functions written in C to interate over
  the given binary calculating the CRC checksum value.

  CRC implementations have been tested against these online calculators to
  validate their correctness to the best of our ability.

  https://www.lammertbies.nl/comm/info/crc-calculation.html
  http://www.sunshine2k.de/coding/javascript/crc/crc_js.html
  """

  @doc """
  Calculates a 8-bit CRC with polynomial x^8+x^6+x^3+x^2+1, 0x14D.
  Chosen based on Koopman, et al. (0xA6 in his notation = 0x14D >> 1):
  http://www.ece.cmu.edu/~koopman/roses/dsn04/koopman04_crc_poly_embedded.pdf

  seed defaults to 0xFF if one is not given
  """
  @spec crc_8(binary, number) :: number
  defdelegate crc_8(input, seed \\ 0xFF), to: :crc

  @doc """
  Calculates a 16-bit ANSI CRC checksum for the provided binary
  """
  @spec crc_16(binary) :: number
  defdelegate crc_16(input), to: :crc

  @doc """
  Calculates a 16-bit CCITT CRC with the given seed,
  seed defaults to 0xFFFF if one is not given.

  This CCIT method uses a 0x1021 polynomial.
  """
  @spec ccitt_16(binary, number) :: number
  defdelegate ccitt_16(input, seed \\ 0xFFFF), to: :crc

  @doc """
  Calculates a 16-bit CCITT Kermit CRC

  This CCIT method uses a 0x8408 polynomial.
  """
  @spec ccitt_16_kermit(binary, number) :: number
  defdelegate ccitt_16_kermit(input, seed \\ 0x0000), to: :crc

  @doc """
  Calculates a 16-bit CCITT XMODEM CRC

  This CCIT method uses a 0x1021 polynomial.
  """
  @spec ccitt_16_xmodem(binary) :: number
  defdelegate ccitt_16_xmodem(input), to: :crc

  @doc """
  Calculates a 16-bit CCITT 0x1D0F CRC

  This CCIT method uses a 0x1021 polynomial.
  """
  @spec ccitt_16_1D0F(binary) :: number
  defdelegate ccitt_16_1D0F(input), to: :crc

  @doc """
  Calculates a 16-bit modbus CRC
  """
  @spec crc_16_modbus(binary) :: number
  defdelegate crc_16_modbus(input), to: :crc

  @doc """
  Calculates a 16-bit Sick CRC
  """
  @spec crc_16_sick(binary) :: number
  defdelegate crc_16_sick(input), to: :crc

  @doc """
  Calculates a 16-bit DNP CRC
  """
  @spec crc_16_dnp(binary) :: number
  defdelegate crc_16_dnp(input), to: :crc

  @doc """
  Calculates a 32-bit CRC
  """
  @spec crc_32(binary) :: number
  defdelegate crc_32(input), to: :crc

  @doc """
  Calculates an XOR checksum for the given binary
  """
  @spec checksum_xor(binary) :: number
  defdelegate checksum_xor(input), to: :crc

  def crc(model, input \\ "123456789") do
    context = :crc_nif.crc_init(model)
    context = :crc_nif.crc_update(context, input)
    %{ bits: bits } = :crc_nif.crc_info(context)
    result = :crc_nif.crc_final(context)
    Base.encode16(<< result :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower)
  end

  def check() do
    check(Map.keys(:crc_nif.crc_list()))
  end

  def check(models) do
    input = "123456789"
    for model <- models, into: %{} do
      context = :crc_nif.crc_init(model)
      context = :crc_nif.crc_update(context, input)
      %{ check: check, bits: bits } = :crc_nif.crc_info(context)
      result = :crc_nif.crc_final(context)
      if result == check do
        {model, {true, Base.encode16(<< result :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower)}}
      else
        {model, {false, Base.encode16(<< check :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower), Base.encode16(<< result :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower)}}
      end
    end
  end

  def residue() do
    residue(Map.keys(:crc_nif.crc_list()))
  end

  def residue(models) do
    for model <- models, into: %{} do
      context = :crc_nif.crc_init(model)
      %{ residue: residue, bits: bits } = :crc_nif.crc_info(context)
      result = :crc_nif.crc_residue(context)
      if result == residue do
        {model, {true, Base.encode16(<< result :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower)}}
      else
        {model, {false, Base.encode16(<< residue :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower), Base.encode16(<< result :: unsigned-little-integer-unit(1)-size(bits) >>, case: :lower)}}
      end
    end
  end

  def checkbad() do
    checkbad(Map.keys(:crc_nif.crc_list()))
  end

  def checkbad(models) do
    for {key, val={false, _, _}} <- check(models), into: %{} do
      {key, val}
    end
  end

  def resbad() do
    resbad(Map.keys(:crc_nif.crc_list()))
  end

  def resbad(models) do
    for {key, val={false, _, _}} <- residue(models), into: %{} do
      {key, val}
    end
  end

  def generate(params={sick, width, poly, init, refin, refout, xorout, check, residue}) do
    context = :crc_nif.crc_init(params)
    table =
      if sick do
        :lists.duplicate(256, 0)
      else
        {true, table} = :crc_nif.debug_table(context)
        table
      end
    bits = (div(width, 8) + (if rem(width, 8) == 0, do: 0, else: 1)) * 8
    f = fn (x) ->
      "0x" <> Base.encode16(<< x :: unsigned-big-integer-unit(1)-size(bits) >>, case: :lower)
    end
    :erlang.iolist_to_binary([
      "{{{NULL, NULL}, true, 0, \"\", #{bits}}, #{sick}, #{width}, #{f.(poly)}, #{f.(init)}, #{refin}, #{refout}, #{f.(xorout)}, #{f.(check)}, #{f.(residue)}, {",
      for n <- table, into: [] do
        [f.(n), ?,]
      end,
      "}}"
    ])
  end
end
