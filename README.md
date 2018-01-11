# CRC

[![Build
Status](https://travis-ci.org/TattdCodeMonkey/crc.png?branch=master)](https://travis-ci.org/TattdCodeMonkey/crc)
[![Hex version](https://img.shields.io/hexpm/v/crc.svg "Hex
version")](https://hex.pm/packages/crc)

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to iterate over the given binary calculating the CRC checksum value. The NIFs are written to report their time slice usage and will not interfere with the schedulers.

## Installation

  1. Add crc to your list of dependencies in `mix.exs`:

```elixir
  def deps do
    [{:crc, "~> 0.8"}]
  end
```

## Supported algorithms (models)

Run `CRC.list/0` to get a full list of all pre-defined models or `CRC.list/1` with a filter to search for a pre-defined model.

## Usage

To calculate a CRC-16 X-Modem checksum for the binary `<<1,2,3,4,5,4,3,2,1>>` using the pre-defined model:

```elixir
iex> CRC.crc(:crc_16_xmodem, <<1,2,3,4,5,4,3,2,1>>)
31763
```

You can also create model's at runtime and re-use them. This can be done with a map:

```elixir
iex> model = CRC.crc_init(
  %{
    width: 16,
    poly: 0x1021,
    init: 0x00,
    refin: false,
    refout: false,
    xorout: 0x00
  }
)
#Reference<x.x.x.x>
iex> CRC.crc(model, <<1,2,3,4,5,4,3,2,1>>)
31763
```

Or extend pre-defined models:

```elixir
iex> model = CRC.crc_init(
  %{
    extend: :crc_16_xmodem,
    init: 0x00,
  }
)
#Reference<x.x.x.x>
iex> CRC.crc(model, <<1,2,3,4,5,4,3,2,1>>)
31763
```

## Tests

CRC implementations have been tested against these online calculators to validate their correctness to the best of our ability.

-  https://www.lammertbies.nl/comm/info/crc-calculation.html
-  http://www.sunshine2k.de/coding/javascript/crc/crc_js.html

There are also two property tests that can use [PyCRC](https://github.com/tpircher/pycrc) or [CRC RevEng](https://sourceforge.net/projects/reveng/) if installed and configured locally.
```bash
$ export PYCRC_BIN=~/pycrc-0.9.1/pycrc.py
$ export REVENG_BIN=~/reveng-1.5.2/reveng
$ mix test
```

PyCRC is used as a part of the TravisCI test suite.
