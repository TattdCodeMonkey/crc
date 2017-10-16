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
  Calculates an XOR checksum for the given binary
  """
  @spec checksum_xor(binary) :: number
  defdelegate checksum_xor(input), to: :crc
end
