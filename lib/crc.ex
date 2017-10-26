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

  def check_slow() do
    check_slow(Map.keys(:crc_nif.crc_list()))
  end

  def check_slow(models) do
    input = "123456789"
    for model <- models, into: %{} do
      context = :crc_nif.crc_slow_init(model)
      context = :crc_nif.crc_slow_update(context, input)
      %{ check: check, bits: bits } = :crc_nif.crc_info(context)
      result = :crc_nif.crc_slow_final(context)
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

  def checkslowbad() do
    checkslowbad(Map.keys(:crc_nif.crc_list()))
  end

  def checkslowbad(models) do
    for {key, val={false, _, _}} <- check_slow(models), into: %{} do
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

  def list_models() do
    list =
      for {key, val} <- :crc_nif.crc_list(), into: [] do
        [key | Map.keys(val)]
      end
    :lists.usort(:lists.flatten(list))
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
  def generate(binary) when is_binary(binary) do
    params = parse([
      :width,
      :poly,
      :init,
      :refin,
      :refout,
      :xorout,
      :check,
      :residue,
      :name
    ], binary, %{
      sick: false,
      width: nil,
      poly: nil,
      init: nil,
      refin: nil,
      refout: nil,
      xorout: nil,
      check: nil,
      residue: nil,
      name: nil
    })
    generate(params)
  end
  def generate(params=%{
    sick: sick,
    width: width,
    poly: poly,
    init: init,
    refin: refin,
    refout: refout,
    xorout: xorout,
    check: check,
    residue: residue,
    name: name,
    root_key: root_key
  }) do
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
    [_ | table_values] = :lists.flatten(for n <- table, into: [] do
      [?,, f.(n)]
    end)
    :erlang.iolist_to_binary([
      "/* #{root_key} */\n",
      "{{NULL, NULL}, false, 0, \"#{root_key}\", 0, \"#{root_key}\", \"#{name}\"},\n",
      "\n",
      "/* width=#{width} poly=#{f.(poly)} init=#{f.(init)} refin=#{refin} refout=#{refout} xorout=#{f.(xorout)} check=#{f.(check)} residue=#{f.(residue)} name=\"#{name}\" */\n",
      "{{{NULL, NULL}, true, 0, \"#{root_key}\", #{bits}}, #{sick}, #{width}, #{f.(poly)}, #{f.(init)}, #{refin}, #{refout}, #{f.(xorout)}, #{f.(check)}, #{f.(residue)}, {",
      table_values,
      "}},"
    ])
  end

  def gen(params) do
    IO.puts(generate(params))
  end

  @doc false
  defp parse([key=:width | keys], "width=" <> rest, acc) do
    {rest, value} = parse_until_whitespace(rest, <<>>)
    value = :erlang.binary_to_integer(value)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:poly | keys], "poly=" <> rest, acc) do
    {rest, "0x" <> value} = parse_until_whitespace(rest, <<>>)
    value = :binary.decode_unsigned(Base.decode16!(value, case: :mixed), :big)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:init | keys], "init=" <> rest, acc) do
    {rest, "0x" <> value} = parse_until_whitespace(rest, <<>>)
    value = :binary.decode_unsigned(Base.decode16!(value, case: :mixed), :big)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:refin | keys], "refin=" <> rest, acc) do
    {rest, value} = parse_until_whitespace(rest, <<>>)
    value = :erlang.binary_to_atom(value, :unicode)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:refout | keys], "refout=" <> rest, acc) do
    {rest, value} = parse_until_whitespace(rest, <<>>)
    value = :erlang.binary_to_atom(value, :unicode)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:xorout | keys], "xorout=" <> rest, acc) do
    {rest, "0x" <> value} = parse_until_whitespace(rest, <<>>)
    value = :binary.decode_unsigned(Base.decode16!(value, case: :mixed), :big)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:check | keys], "check=" <> rest, acc) do
    {rest, "0x" <> value} = parse_until_whitespace(rest, <<>>)
    value = :binary.decode_unsigned(Base.decode16!(value, case: :mixed), :big)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:residue | keys], "residue=" <> rest, acc) do
    {rest, "0x" <> value} = parse_until_whitespace(rest, <<>>)
    value = :binary.decode_unsigned(Base.decode16!(value, case: :mixed), :big)
    acc = Map.put(acc, key, value)
    parse(keys, rest, acc)
  end
  defp parse([key=:name | keys], "name=" <> rest, acc) do
    {rest, value} = parse_until_whitespace(rest, <<>>)
    name = strip_quotes(value, <<>>)
    root_key = underscore(value, <<>>)
    acc = Map.put(acc, key, name)
    acc = Map.put(acc, :root_key, root_key)
    parse(keys, rest, acc)
  end
  defp parse(keys, << _, rest :: binary() >>, acc) do
    parse(keys, rest, acc)
  end
  defp parse([], <<>>, acc) do
    acc
  end

  @doc false
  defp parse_until_whitespace(<<>>, acc) do
    {<<>>, acc}
  end
  defp parse_until_whitespace(<< ?\s, rest :: binary() >>, acc) do
    {rest, acc}
  end
  defp parse_until_whitespace(<< c, rest :: binary() >>, acc) do
    parse_until_whitespace(rest, << acc ::binary(), c >>)
  end

  @doc false
  defp underscore(<< ?", rest :: binary() >>, acc) do
    underscore(rest, acc)
  end
  defp underscore(<< c, rest :: binary() >>, acc) when c in ?A..?Z do
    underscore(rest, << acc :: binary(), (c + 32) >>)
  end
  defp underscore(<< c, rest :: binary() >>, acc) when c in ?a..?z or c in ?0..?9 do
    underscore(rest, << acc :: binary(), c >>)
  end
  defp underscore(<< c, rest :: binary() >>, acc) when c in [?-, ?/] do
    underscore(rest, << acc :: binary(), ?_ >>)
  end
  defp underscore(<<>>, acc) do
    acc
  end

  @doc false
  defp strip_quotes(<< ?", rest :: binary() >>, acc) do
    strip_quotes(rest, acc)
  end
  defp strip_quotes(<< c, rest :: binary() >>, acc) do
    strip_quotes(rest, << acc :: binary(), c >>)
  end
  defp strip_quotes(<<>>, acc) do
    acc
  end
end
