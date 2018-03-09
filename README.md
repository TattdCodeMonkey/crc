# CRC

[![Build
Status](https://travis-ci.org/TattdCodeMonkey/crc.png?branch=master)](https://travis-ci.org/TattdCodeMonkey/crc)
[![Hex version](https://img.shields.io/hexpm/v/crc.svg "Hex
version")](https://hex.pm/packages/crc)

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to iterate over the given binary calculating the CRC checksum value. The NIFs are written to report their time slice usage and will not interfere with the schedulers.

## Installation

### Elixir
  1. Add crc to your list of dependencies in `mix.exs`:

```elixir
  def deps do
    [{:crc, "~> 0.9.0"}]
  end
```

### Erlang
  1. add crc to your `rebar.config`
```erlang
{deps, [
  {crc, "0.9.0"}
]}.
```
  
  or `erlang.mk`

```
dep_crc = hex 0.9.0
```

## Supported algorithms (models)

Run `CRC.list/0` to get a full list of all pre-defined models or `CRC.list/1` with a filter to search for a pre-defined model.

## Usage

To calculate a CRC-16 X-Modem checksum for the binary `<<1,2,3,4,5,4,3,2,1>>` using the pre-defined model:

```elixir
iex> CRC.crc(:crc_16_xmodem, <<1,2,3,4,5,4,3,2,1>>)
31763
iex> CRC.calculate(<<1,2,3,4,5,4,3,2,1>>, :crc_16_xmodem)
31763
```

Or you can create a model at runtime, this can be done with a map:
```elixir
iex> CRC.crc(
  %{
    width: 16,
    poly: 0x1021,
    init: 0x00,
    refin: false,
    refout: false,
    xorout: 0x00
  }, 
  <<1,2,3,4,5,4,3,2,1>>
)
31763
iex> CRC.calculate(   
  <<1,2,3,4,5,4,3,2,1>>,
  %{
    width: 16,
    poly: 0x1021,
    init: 0x00,
    refin: false,
    refout: false,
    xorout: 0x00
  }
)
31763
```

Or you can extend one of the pre-defined models:

```elixir
iex> CRC.crc(
  %{
    extend: :crc_16_xmodem,
    init: 0x00,
  }, 
  <<1,2,3,4,5,4,3,2,1>>
)
31763
```

`CRC.crc_init/1` is used to create a resource and be used to do partial updates to a calculation that is then finalized later:

```elixir
iex> resource = CRC.crc_init(:crc_16_xmodem)
#Reference<x.x.x.x>
iex> resource2 = CRC.crc_update(resource, <<1, 2, 3, 4, 5>>)
#Reference<y.y.y.y>
iex> resource3 = CRC.crc_update(resource2, <<4, 3, 2, 1>>)
#Reference<z.z.z.z>
iex>CRC.crc_final(resource3)
31763
```

This could be usefule to calculate a CRC for a larger binary that you are receiving asyncronously. 

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
