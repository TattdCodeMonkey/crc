defmodule CRC.Pure do
  @moduledoc """
  CRC calculation with no NIF.

  `CRC.Pure` has the same API as `CRC` but is implemented entirely in
  Erlang, so it needs no C compiler. It is the implementation `CRC` will use
  from v1.0 onwards, so code written against `CRC.Pure` only needs the
  module renamed to `CRC` when upgrading.

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

      iex> CRC.Pure.calculate("123456789", %{
      ...>   width: 16,
      ...>   poly: 0x1021,
      ...>   init: 0xFFFF,
      ...>   refin: false,
      ...>   refout: false,
      ...>   xorout: 0x0000
      ...> })
      0x29B1

  Extend a built-in model with overrides:

      iex> CRC.Pure.calculate("123456789", %{extend: :crc_16_ccitt_false, init: 0x1D0F})
      0xE5CC

  ## Models

  Use `list/0` to get all available models. Model parameters follow the
  Rocksoft Model CRC Algorithm specification with the addition of the `sick`
  flag for the non-standard SICK sensor variant.
  """

  @doc """
  Calculates a CRC checksum for `input` using the given `params`.

  `params` is one of:

    * an atom — a built-in model key or alias (e.g., `:crc_32`, `:pkzip`)
    * a map with `:width`, `:poly`, `:init`, `:refin`, `:refout` and
      `:xorout` keys (and optionally `:sick`)
    * a map with an `:extend` key naming a built-in model, plus the
      parameters to override

  ## Examples

      iex> CRC.Pure.calculate("123456789", :crc_32)
      0xCBF43926
  """
  @spec calculate(iodata(), :crc_algorithm.params()) :: :crc_algorithm.value()
  def calculate(input, params), do: :crc_pure.calc(params, input)

  @doc """
  Initializes a resource for a multi-part CRC calculation.

  Accepts the same `params` as `calculate/2`.
  """
  @spec init(:crc_algorithm.params()) :: :crc_algorithm.resource()
  defdelegate init(params), to: :crc_pure

  @doc """
  Continues a multi-part CRC calculation with `input`, returning the
  resource to pass to the next `update/2` or `final/1` call.
  """
  @spec update(:crc_algorithm.resource(), iodata()) :: :crc_algorithm.resource()
  defdelegate update(resource, input), to: :crc_pure

  @doc """
  Finishes a multi-part CRC calculation and returns the CRC.
  """
  @spec final(:crc_algorithm.resource()) :: :crc_algorithm.value()
  defdelegate final(resource), to: :crc_pure

  @doc """
  Returns the parameters (`width`, `poly`, `init`, `refin`, `refout`,
  `xorout`, `check`, `residue` and `sick`) of a model or of a resource
  returned by `init/1`.

  ## Examples

      iex> CRC.Pure.info(:crc_32).poly
      0x04C11DB7
  """
  @spec info(:crc_algorithm.params() | :crc_algorithm.resource()) :: :crc_algorithm.info()
  defdelegate info(params_or_resource), to: :crc_pure

  @doc """
  Returns the residue of a model or of a resource returned by `init/1`.

  ## Examples

      iex> CRC.Pure.residue(:crc_32)
      0xDEBB20E3
  """
  @spec residue(:crc_algorithm.params() | :crc_algorithm.resource()) :: :crc_algorithm.value()
  defdelegate residue(params_or_resource), to: :crc_pure

  @doc """
  Returns every built-in model as `{key, name}`.
  """
  @spec list() :: [{atom, String.t()}]
  defdelegate list(), to: :crc

  @doc """
  Returns the built-in models whose key or name matches the regular
  expression `filter`.

  ## Examples

      iex> CRC.Pure.list("^crc_32$")
      [{:crc_32, "CRC-32"}]
  """
  @spec list(binary) :: [{atom, String.t()}]
  defdelegate list(filter), to: :crc

  @doc """
  Calculates an XOR checksum for the given binary.

  ## Examples

      iex> CRC.Pure.checksum_xor(<<1, 2, 4>>)
      7
  """
  @spec checksum_xor(binary) :: number
  defdelegate checksum_xor(input), to: :crc_pure

  @doc """
  Removes every cached built-in model.

  The first use of a built-in model builds its lookup table (tens of
  microseconds) and caches it in `:persistent_term` until the VM stops, at
  most about 2.6 KB per model. The next use of a cleared model rebuilds it.

  Clearing is expensive on a busy system: erasing a `:persistent_term` value
  makes the VM scan every process for references to it. Use it rarely, e.g.
  in tests or after a one-off job that used many models, and never on a
  timer. To remove a single model use `clear_cache/1`.
  """
  @spec clear_cache() :: :ok
  defdelegate clear_cache(), to: :crc_pure

  @doc """
  Removes the cached built-in `model` (a model name or alias). See
  `clear_cache/0` for the cost of clearing.

  Raises `ArgumentError` if `model` is not a built-in model.
  """
  @spec clear_cache(atom) :: :ok
  defdelegate clear_cache(model), to: :crc_pure
end
