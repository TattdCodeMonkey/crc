defmodule CRC do
  @moduledoc """
  This module is used to calculate CRC (Cyclic Redundancy Check) values
  for binary data. It uses NIF functions written in C to iterate over
  the given binary calculating the CRC checksum value.

  `CRC.Pure` provides the same API with no NIF, and will replace the NIF in
  v1.0. See the "Upgrading to 1.0" section of the README.

  CRC implementations have been tested against these online calculators to
  validate their correctness to the best of our ability.

  https://www.lammertbies.nl/comm/info/crc-calculation.html
  http://www.sunshine2k.de/coding/javascript/crc/crc_js.html
  """

  @doc """
  Calculate a CRC checksum for the `input` based on the crc `params` given.

  `params` can be an atom for one of the compiled models. See `CRC.list/0` for
  a full list or a Map with parameters to create a model at runtime. The map
  given should have all of the following keys:

  `width` - (unsigned integer) representation for the width of the CRC in bits
  `poly` - (unsigned integer) the polynomial used for the CRC calculation
  `init` - (unsigned integer) The initial value used when starting the calculation
  `refin` - (boolean) if the input value should be reflected. This is used for changing between endian's
  `refout` - (boolean) if the outvalue should be reflected when calculation is completed
  `xorout` - (unsigned integer) Final xor value used when completing the CRC calculation

  ## Examples

      %{
        width: 16,
        poly: 0x1021,
        init: 0x00,
        refin: false,
        refout: false,
        xorout: 0x00
      }

  You can also extend one of the compiled models at runtime by creating a map
  with `extend` key set to the model you wish to extend and the keys you wish
  to override for that model.

  For example to override the initial value for the `:crc_16_ccitt_false` model
  to `0x1D0F` you would pass the following Map as params:

      `%{extend: :crc_16_ccitt_false, init: 0x1D0F}`

  The input comes first so calls can be written as pipelines:

      read_data() |> CRC.calculate(:crc_16) |> do_something()

  You can learn more about CRC calculation here:
  https://www.sunshine2k.de/articles/coding/crc/understanding_crc.html
  """
  @spec calculate(iodata(), :crc_algorithm.params()) :: :crc_algorithm.value()
  defdelegate calculate(input, params), to: :crc

  @doc """
  Initialize a resource to be used for a multi-part CRC calculation with
  `update/2` and `final/1`.

  Resource is created using the same `params` types that are used with
  `calculate/2`:

    - atom's for compiled models
    - Map with model values
    - Map to extend a compiled model.
  """
  @spec init(:crc_algorithm.params()) :: :crc_algorithm.resource()
  defdelegate init(params), to: :crc

  @doc """
  Begins or continues a multi-part CRC calculation.

  Takes a `resource` from result of `init/1` or previous `update/2`
  call, and binary `input`, returns a new `resource` to be used to continue or
  finalize the CRC calculation.
  """
  @spec update(:crc_algorithm.resource(), iodata()) :: :crc_algorithm.resource()
  defdelegate update(resource, input), to: :crc

  @doc """
  Takes a `resource` result from `update/2` and finalizes the multi-part
  CRC calculation.
  """
  @spec final(:crc_algorithm.resource()) :: :crc_algorithm.value()
  defdelegate final(resource), to: :crc

  @doc """
  Returns the parameters (`width`, `poly`, `init`, `refin`, `refout`,
  `xorout`, `check`, `residue` and `sick`) of a model or of a resource
  returned by `init/1`.
  """
  @spec info(:crc_algorithm.params() | :crc_algorithm.resource()) :: :crc_algorithm.info()
  defdelegate info(params_or_resource), to: :crc

  @doc """
  Returns the residue of a model or of a resource returned by `init/1`.
  """
  @spec residue(:crc_algorithm.params() | :crc_algorithm.resource()) :: :crc_algorithm.value()
  defdelegate residue(params_or_resource), to: :crc

  @doc """
  Returns a list of all the compiled CRC models.
  """
  @spec list() :: [{atom, String.t()}]
  defdelegate list(), to: :crc

  @doc """
  Returns a list of all compiled CRC Models that match the filter given.

  Filter is compiled into a regular expression and matched against the model name
  and description.
  """
  @spec list(binary) :: [{atom, String.t()}]
  defdelegate list(filter), to: :crc

  @doc """
  Calculates an XOR checksum for the given binary
  """
  @spec checksum_xor(binary) :: number
  defdelegate checksum_xor(input), to: :crc

  @doc "Deprecated, use `CRC.calculate/2` instead (note the argument order: input, params)."
  @deprecated "Use CRC.calculate/2 instead (note the argument order: input, params)"
  @spec crc(:crc_algorithm.params(), iodata()) :: :crc_algorithm.value()
  def crc(params, input), do: :crc_fast.calc(params, input)

  @doc "Deprecated, use `CRC.init/1` instead."
  @deprecated "Use CRC.init/1 instead"
  @spec crc_init(:crc_algorithm.params()) :: :crc_algorithm.resource()
  def crc_init(params), do: :crc_fast.init(params)

  @doc "Deprecated, use `CRC.update/2` instead."
  @deprecated "Use CRC.update/2 instead"
  @spec crc_update(:crc_algorithm.resource(), iodata()) :: :crc_algorithm.resource()
  def crc_update(resource, input), do: :crc_fast.update(resource, input)

  @doc "Deprecated, use `CRC.final/1` instead."
  @deprecated "Use CRC.final/1 instead"
  @spec crc_final(:crc_algorithm.resource()) :: :crc_algorithm.value()
  def crc_final(resource), do: :crc_fast.final(resource)

  use CRC.Legacy
end
