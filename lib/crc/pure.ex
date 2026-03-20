defmodule CRC.Pure do
  @moduledoc """
  Pure Elixir CRC calculation module.

  Calculates CRC (Cyclic Redundancy Check) checksums for binary data using
  a bit-by-bit algorithm implemented entirely in Elixir with no native
  dependencies. Supports all standard CRC models (3-bit through 64-bit)
  as well as custom model definitions and the SICK variant.

  ## Usage

  Calculate a CRC using a built-in model:

      iex> CRC.Pure.calculate("123456789", :crc_32)
      0xCBF43926

  Pipe-friendly — data flows left to right:

      read_data() |> CRC.Pure.calculate(:crc_16) |> do_something()

  Use the multi-part API for streaming or incremental calculation:

      iex> resource = CRC.Pure.init(:crc_16)
      iex> resource = CRC.Pure.update(resource, "1234")
      iex> resource = CRC.Pure.update(resource, "56789")
      iex> CRC.Pure.final(resource)
      0xBB3D

  Define a custom model at runtime:

      CRC.Pure.calculate("123456789", %{
        width: 16,
        poly: 0x1021,
        init: 0xFFFF,
        refin: false,
        refout: false,
        xorout: 0x0000
      })

  Extend a built-in model with overrides:

      CRC.Pure.calculate("123456789", %{extend: :crc_16_ccitt_false, init: 0x1D0F})

  ## Models

  Use `list/0` to get all available model keys. Model parameters follow the
  Rocksoft Model CRC Algorithm specification with the addition of the `sick`
  flag for the non-standard SICK sensor variant.
  """

  @behaviour :crc_algorithm

  import Bitwise

  alias CRC.Pure.Models

  @doc """
  Calculates a CRC checksum for `input` using the given `params`.

  This is the pipe-friendly version with `input` as the first argument.
  See `calc/2` for details on valid `params`.

  ## Examples

      iex> CRC.Pure.calculate("123456789", :crc_32)
      0xCBF43926

      iex> CRC.Pure.calculate("123456789", %{extend: :crc_16_ccitt_false, init: 0x1D0F})
      0xE5CC
  """
  def calculate(input, params) do
    calc(params, input)
  end

  @doc """
  Calculates a CRC checksum for `iodata` using the given `params`.

  `params` can be:

    * an atom — a built-in model key (e.g., `:crc_32`, `:crc_16_usb`)
    * a map — with keys `:width`, `:poly`, `:init`, `:refin`, `:refout`, `:xorout`
      and optionally `:check`, `:residue`, `:sick`
    * a map with `:extend` — extends a built-in model with overrides

  Returns the CRC value as a non-negative integer.

  Prefer `calculate/2` for pipeline usage — this function exists to satisfy
  the `:crc_algorithm` behaviour callback.

  ## Examples

      iex> CRC.Pure.calc(:crc_32, "123456789")
      0xCBF43926
  """
  @impl true
  def calc(params, iodata) do
    resource = init(params)
    resource = update(resource, iodata)
    final(resource)
  end

  @doc """
  Initializes a CRC calculation resource.

  `params` accepts the same types as `calc/2`: an atom for a built-in model,
  a map with full model parameters, or a map with `:extend` to override a
  built-in model.

  Returns a resource that can be passed to `update/2` and `final/1` for
  incremental CRC calculation, or to `info/1` and `residue/1` for inspection.

  ## Examples

      iex> resource = CRC.Pure.init(:crc_32)
      iex> resource = CRC.Pure.update(resource, "123456789")
      iex> CRC.Pure.final(resource)
      0xCBF43926
  """
  @impl true
  def init(key) when is_atom(key) do
    case Models.get(key) do
      nil -> :erlang.error({:badarg, [key]})
      model -> init(model)
    end
  end

  def init(%{extend: base_key} = params) when is_atom(base_key) do
    case Models.get(base_key) do
      nil ->
        :erlang.error({:badarg, [params]})

      base_model ->
        merged = Map.merge(base_model, Map.delete(params, :extend))
        init(merged)
    end
  end

  def init(
        %{
          width: width,
          poly: poly,
          init: init_val,
          refin: refin,
          refout: refout,
          xorout: xorout
        } = params
      )
      when is_boolean(refin) and is_boolean(refout) do
    validate_non_neg_integers!(params, width, poly, init_val, xorout)

    check = Map.get(params, :check, 0)
    residue = Map.get(params, :residue, 0)
    sick = Map.get(params, :sick, false)

    masks = compute_masks(width)
    validate_params!(params, masks, check, residue)

    build_resource(params, masks, check, residue, sick)
  end

  def init(bad_params) do
    :erlang.error({:badarg, [bad_params]})
  end

  defp compute_masks(width) do
    msb_mask = 1 <<< (width - 1)
    crc_mask = 1 ||| (msb_mask - 1) <<< 1
    crc_shift = if width < 8, do: 8 - width, else: 0
    {msb_mask, crc_mask, crc_shift}
  end

  defp validate_non_neg_integers!(params, width, poly, init_val, xorout) do
    unless is_integer(width) and width >= 0 and
             is_integer(poly) and poly >= 0 and
             is_integer(init_val) and init_val >= 0 and
             is_integer(xorout) and xorout >= 0 do
      :erlang.error({:badarg, [params]})
    end
  end

  defp validate_params!(params, {_msb_mask, crc_mask, _crc_shift}, check, residue) do
    %{poly: poly, init: init_val, xorout: xorout} = params

    if poly > crc_mask or init_val > crc_mask or xorout > crc_mask or
         check > crc_mask or residue > crc_mask do
      :erlang.error({:badarg, [params]})
    end
  end

  defp build_resource(params, {msb_mask, crc_mask, crc_shift}, check, residue, sick) do
    %{width: width, poly: poly, init: init_val, refin: refin, refout: refout, xorout: xorout} =
      params

    resource = %{
      __struct__: __MODULE__,
      width: width,
      poly: poly,
      init: init_val,
      refin: refin,
      refout: refout,
      xorout: xorout,
      check: check,
      residue: residue,
      sick: sick,
      msb_mask: msb_mask,
      crc_mask: crc_mask,
      crc_shift: crc_shift,
      value: 0,
      extra: 0
    }

    if sick do
      {value, extra} = sick_init(resource)
      %{resource | value: value, extra: extra}
    else
      %{resource | value: do_crc_init(init_val, 0, width, poly, msb_mask, crc_mask)}
    end
  end

  @doc """
  Updates an in-progress CRC calculation with additional data.

  Takes a `resource` returned by `init/1` or a previous call to `update/2`,
  and `iodata` to process. Returns an updated resource.

  Can be called multiple times to process data incrementally. Call `final/1`
  on the returned resource to get the final CRC value.

  ## Examples

      iex> resource = CRC.Pure.init(:crc_16)
      iex> resource = CRC.Pure.update(resource, "1234")
      iex> resource = CRC.Pure.update(resource, "56789")
      iex> CRC.Pure.final(resource)
      0xBB3D
  """
  @impl true
  def update(resource = %{__struct__: __MODULE__, value: value, sick: false}, iodata) do
    %{resource | value: do_crc_update(resource, value, IO.iodata_to_binary(iodata))}
  end

  def update(
        resource = %{__struct__: __MODULE__, value: value, extra: extra, sick: true},
        iodata
      ) do
    {new_value, new_extra} =
      do_sick_update(resource, {value, extra}, IO.iodata_to_binary(iodata))

    %{resource | value: new_value, extra: new_extra}
  end

  @doc """
  Finalizes a CRC calculation and returns the checksum value.

  Takes a `resource` from `update/2` and applies the final processing steps
  (bit flushing, output reflection, and XOR) to produce the CRC value.

  Returns the CRC checksum as a non-negative integer.

  ## Examples

      iex> resource = CRC.Pure.init(:crc_32)
      iex> resource = CRC.Pure.update(resource, "123456789")
      iex> CRC.Pure.final(resource)
      0xCBF43926
  """
  @impl true
  def final(resource = %{__struct__: __MODULE__, value: value, sick: false}) do
    do_crc_final(resource, value)
  end

  def final(resource = %{__struct__: __MODULE__, value: value, sick: true}) do
    do_sick_final(resource, value)
  end

  @doc """
  Returns the model parameters for a CRC resource or model key.

  When given a resource (from `init/1`), extracts the model parameters.
  When given an atom or map, initializes the resource first then extracts.

  Returns a map with keys: `:width`, `:poly`, `:init`, `:refin`, `:refout`,
  `:xorout`, `:check`, `:residue`, and `:sick`.

  ## Examples

      iex> info = CRC.Pure.info(:crc_32)
      iex> info.width
      32
      iex> info.poly
      0x04C11DB7
  """
  @impl true
  def info(%{
        __struct__: __MODULE__,
        width: width,
        poly: poly,
        init: init_val,
        refin: refin,
        refout: refout,
        xorout: xorout,
        check: check,
        residue: residue,
        sick: sick
      }) do
    %{
      width: width,
      poly: poly,
      init: init_val,
      refin: refin,
      refout: refout,
      xorout: xorout,
      check: check,
      residue: residue,
      sick: sick
    }
  end

  def info(params) do
    info(init(params))
  end

  @doc """
  Calculates the residue value for a CRC model.

  The residue is the CRC value that results when the check value is appended
  to the test string "123456789" and the CRC is recalculated. For well-formed
  CRC models, this is a constant that can be used to verify data integrity.

  Accepts a resource (from `init/1`), an atom model key, or a map with model
  parameters.

  ## Examples

      iex> CRC.Pure.residue(:crc_32)
      0xDEBB20E3
  """
  @impl true
  def residue(resource = %{__struct__: __MODULE__, sick: false}) do
    do_crc_residue(resource)
  end

  def residue(
        resource = %{
          __struct__: __MODULE__,
          width: width,
          refout: refout,
          xorout: xorout0,
          sick: true
        }
      ) do
    xorout =
      if refout do
        crc_reflect(xorout0, width)
      else
        xorout0
      end

    copy = %{resource | init: 0, xorout: 0, value: 0}
    {crc, _extra} = do_sick_update(copy, {0, 0}, <<xorout::size(width)>>)
    do_sick_final(copy, crc)
  end

  def residue(params) do
    residue(init(params))
  end

  @doc """
  Returns a list of all available CRC model keys.

  These keys can be passed to `calc/2`, `init/1`, `info/1`, and `residue/1`.

  ## Examples

      iex> :crc_32 in CRC.Pure.list()
      true
  """
  def list do
    Models.root_keys()
  end

  # Bit reflection

  @doc false
  def crc_reflect(reg, width)
      when is_integer(reg) and reg >= 0 and is_integer(width) and width >= 0 do
    res = reg &&& 0x01
    do_crc_reflect(res, reg, 0, width - 1)
  end

  defp do_crc_reflect(res, _reg, max, max), do: res

  defp do_crc_reflect(res0, reg0, i, max) do
    reg1 = reg0 >>> 1
    res1 = res0 <<< 1
    res2 = res1 ||| (reg1 &&& 0x01)
    do_crc_reflect(res2, reg1, i + 1, max)
  end

  # CRC init — process the init value through Width rounds

  defp do_crc_init(crc, width, width, _poly, _msb_mask, crc_mask) do
    crc &&& crc_mask
  end

  defp do_crc_init(crc0, i, width, poly, msb_mask, crc_mask) do
    bit = crc0 &&& 0x01

    crc1 =
      if bit == 0 do
        crc0
      else
        bxor(crc0, poly)
      end

    crc2 = crc1 >>> 1

    crc3 =
      if bit == 0 do
        crc2
      else
        crc2 ||| msb_mask
      end

    do_crc_init(crc3, i + 1, width, poly, msb_mask, crc_mask)
  end

  # CRC update — process input data byte by byte, bit by bit

  defp do_crc_update(_resource, crc, <<>>), do: crc

  defp do_crc_update(
         resource = %{poly: poly, refin: false, msb_mask: msb_mask, crc_mask: crc_mask},
         crc,
         <<octet, rest::binary>>
       ) do
    crc1 = do_crc_update_once(crc, octet, 0, 8, poly, msb_mask, crc_mask)
    do_crc_update(resource, crc1, rest)
  end

  defp do_crc_update(
         resource = %{poly: poly, refin: true, msb_mask: msb_mask, crc_mask: crc_mask},
         crc,
         <<octet0, rest::binary>>
       ) do
    octet = crc_reflect(octet0, 8)
    crc1 = do_crc_update_once(crc, octet, 0, 8, poly, msb_mask, crc_mask)
    do_crc_update(resource, crc1, rest)
  end

  defp do_crc_update_once(crc, _octet, max, max, _poly, _msb_mask, _crc_mask), do: crc

  defp do_crc_update_once(crc0, octet, i, max, poly, msb_mask, crc_mask) do
    bit = crc0 &&& msb_mask
    crc1 = (crc0 <<< 1 &&& crc_mask) ||| (octet >>> (7 - i) &&& 0x01)

    crc2 =
      if bit == 0 do
        crc1
      else
        bxor(crc1, poly)
      end

    do_crc_update_once(crc2, octet, i + 1, max, poly, msb_mask, crc_mask)
  end

  # CRC final — flush remaining bits and apply refout/xorout

  defp do_crc_final(
         %{
           width: width,
           poly: poly,
           refout: refout,
           xorout: xorout,
           msb_mask: msb_mask,
           crc_mask: crc_mask
         },
         crc
       ) do
    do_crc_final_loop(crc, 0, width, poly, refout, xorout, msb_mask, crc_mask)
  end

  defp do_crc_final_loop(crc0, width, width, _poly, _refout = false, xorout, _msb_mask, crc_mask) do
    bxor(crc0, xorout) &&& crc_mask
  end

  defp do_crc_final_loop(crc0, width, width, _poly, _refout = true, xorout, _msb_mask, crc_mask) do
    crc1 = crc_reflect(crc0, width)
    bxor(crc1, xorout) &&& crc_mask
  end

  defp do_crc_final_loop(crc0, i, width, poly, refout, xorout, msb_mask, crc_mask) do
    bit = crc0 &&& msb_mask
    crc1 = crc0 <<< 1 &&& crc_mask

    crc2 =
      if bit == 0 do
        crc1
      else
        bxor(crc1, poly)
      end

    do_crc_final_loop(crc2, i + 1, width, poly, refout, xorout, msb_mask, crc_mask)
  end

  # CRC residue calculation

  defp do_crc_residue(
         resource = %{
           __struct__: __MODULE__,
           width: width,
           refin: refin,
           refout: refout,
           xorout: xorout0
         }
       ) do
    copy = %{resource | init: 0, xorout: 0, value: 0}

    xorout =
      if refout do
        crc_reflect(xorout0, width)
      else
        xorout0
      end

    residue = do_crc_residue_loop(copy, xorout)

    if refin do
      crc_reflect(residue, width)
    else
      residue
    end
  end

  defp do_crc_residue_loop(
         %{
           width: width,
           poly: poly,
           init: init_val,
           xorout: xorout,
           msb_mask: msb_mask,
           crc_mask: crc_mask
         },
         message
       ) do
    do_crc_residue_step(bxor(init_val, message), 0, width, poly, xorout, msb_mask, crc_mask)
  end

  defp do_crc_residue_step(remainder, max, max, _poly, xorout, _msb_mask, crc_mask) do
    bxor(remainder, xorout) &&& crc_mask
  end

  defp do_crc_residue_step(remainder, i, max, poly, xorout, msb_mask, crc_mask) do
    if (remainder &&& msb_mask) == 0 do
      do_crc_residue_step(remainder <<< 1, i + 1, max, poly, xorout, msb_mask, crc_mask)
    else
      do_crc_residue_step(
        bxor(remainder <<< 1, poly),
        i + 1,
        max,
        poly,
        xorout,
        msb_mask,
        crc_mask
      )
    end
  end

  # SICK variant

  defp sick_init(%{width: 16, init: init_val}), do: {init_val, 0}

  defp sick_init(params), do: :erlang.error({:badarg, [params]})

  defp do_sick_update(_resource, {crc, prev_byte}, <<>>), do: {crc, prev_byte}

  defp do_sick_update(
         resource = %{
           width: 16,
           poly: poly,
           msb_mask: msb_mask,
           crc_mask: crc_mask,
           crc_shift: crc_shift
         },
         {crc0, prev_byte0},
         <<next_byte0, rest::binary>>
       ) do
    next_byte1 = 0x00FF &&& next_byte0

    crc1 =
      if (crc0 &&& msb_mask <<< crc_shift) == 0 do
        crc0 <<< 1
      else
        bxor(crc0 <<< 1, poly <<< crc_shift)
      end

    crc2 = bxor(crc1, next_byte1 ||| prev_byte0)
    prev_byte1 = next_byte1 <<< 8
    do_sick_update(resource, {crc2 &&& crc_mask, prev_byte1}, rest)
  end

  defp do_sick_final(%{width: 16}, crc) do
    low_byte = (crc &&& 0xFF00) >>> 8
    high_byte = (crc &&& 0x00FF) <<< 8
    low_byte ||| high_byte
  end

  defp do_sick_final(params, crc), do: :erlang.error({:badarg, [params, crc]})
end
