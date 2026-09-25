# CRC

[![Module Version](https://img.shields.io/hexpm/v/crc.svg)](https://hex.pm/packages/crc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/crc/)
[![Total Download](https://img.shields.io/hexpm/dt/crc.svg)](https://hex.pm/packages/crc)
[![License](https://img.shields.io/hexpm/l/crc.svg)](https://github.com/TattdCodeMonkey/crc/blob/main/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/TattdCodeMonkey/crc.svg)](https://github.com/TattdCodeMonkey/crc/commits/main)

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to iterate over the given binary calculating the CRC checksum value. The NIFs are written to report their time slice usage and will not interfere with the schedulers.

## Installation

### Elixir

Add `:crc` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:crc, "~> 0.11"}
  ]
end
```

### Erlang

Add `crc` to your `rebar.config`:

```erlang
{deps, [
  {crc, "0.11.0"}
]}.
```

Or `erlang.mk`:

```erlang
dep_crc = hex 0.11.0
```

### Build requirements

crc compiles its NIFs from C source when it is built, so you need a C compiler, `make`, and the C standard library headers:

- Alpine: `apk add build-base` (or `gcc make musl-dev`)
- Debian / Ubuntu: `apt install build-essential`
- Fedora: `dnf group install "Development Tools"`
- macOS: `xcode-select --install`
- FreeBSD / OpenBSD / NetBSD / DragonFly: `gmake` and a C compiler
- Windows: Visual Studio Build Tools (`nmake` and `cl`), run from a Developer Command Prompt

## Supported algorithms (models)

Run `CRC.list/0` to get a full list of all pre-defined models or `CRC.list/1` with a filter to search for a pre-defined model.

## Usage

To calculate a CRC-16 X-Modem checksum for the binary `<<1,2,3,4,5,4,3,2,1>>` using the pre-defined model:

```elixir
iex> CRC.calculate(<<1,2,3,4,5,4,3,2,1>>, :crc_16_xmodem)
31763
```

The input comes first, so `CRC.calculate/2` works in pipelines:

```elixir
read_data() |> CRC.calculate(:crc_16_xmodem) |> do_something()
```

Or you can create a model at runtime, this can be done with a map:

```elixir
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
iex> CRC.calculate(<<1,2,3,4,5,4,3,2,1>>, %{extend: :crc_16_xmodem, init: 0x00})
31763
```

`CRC.init/1` creates a resource that can be used to do partial updates to a calculation that is then finalized later:

```elixir
iex> resource = CRC.init(:crc_16_xmodem)
iex> resource = CRC.update(resource, <<1, 2, 3, 4, 5>>)
iex> resource = CRC.update(resource, <<4, 3, 2, 1>>)
iex> CRC.final(resource)
31763
```

This could be useful to calculate a CRC for a larger binary that you are receiving asynchronously.

`CRC.info/1` and `CRC.residue/1` return the parameters and residue of a model or resource.

### Without a C compiler

`CRC.Pure` (`crc_pure` from Erlang) has the same API as `CRC` but needs no NIF. It will become the implementation behind `CRC` in v1.0:

```elixir
iex> CRC.Pure.calculate(<<1,2,3,4,5,4,3,2,1>>, :crc_16_xmodem)
31763
```

## Upgrading to 1.0

v1.0 removes the NIF, so crc will no longer need a C compiler, and removes the functions deprecated in v0.12. v0.12 already has the v1.0 API, so fixing its deprecation warnings prepares your code for v1.0.

| Deprecated (removed in v1.0) | Replacement |
|---|---|
| `CRC.crc(params, input)` | `CRC.calculate(input, params)` |
| `CRC.crc_init(params)` | `CRC.init(params)` |
| `CRC.crc_update(resource, input)` | `CRC.update(resource, input)` |
| `CRC.crc_final(resource)` | `CRC.final(resource)` |
| `CRC.crc_8(input)` | `CRC.calculate(input, %{extend: :crc_8_koop, init: 0})` |
| `CRC.crc_8(input, seed)` | `CRC.calculate(input, %{extend: :crc_8_koop, init: init})` where `init` is `bxor(seed, 0xFF)` bit-reflected |
| `CRC.crc_16(input)` | `CRC.calculate(input, :crc_16)` |
| `CRC.ccitt_16(input)` | `CRC.calculate(input, :crc_16_ccitt_false)` |
| `CRC.ccitt_16(input, seed)` | `CRC.calculate(input, %{extend: :crc_16_ccitt_false, init: seed})` |
| `CRC.ccitt_16_kermit(input)` | `CRC.calculate(input, :crc_16_kermit)` |
| `CRC.ccitt_16_kermit(input, seed)` | `CRC.calculate(input, %{extend: :crc_16_kermit, init: seed})` |
| `CRC.ccitt_16_xmodem(input)` | `CRC.calculate(input, :xmodem)` |
| `CRC.ccitt_16_1D0F(input)` | `CRC.calculate(input, %{extend: :crc_16_ccitt_false, init: 0x1D0F})` |
| `CRC.crc_16_dnp(input)` | `CRC.calculate(input, :crc_16_dnp)` |
| `CRC.crc_16_modbus(input)` | `CRC.calculate(input, :crc_16_modbus)` |
| `CRC.crc_16_sick(input)` | `CRC.calculate(input, :crc_16_sick)` |
| `CRC.crc_32(input)` | `CRC.calculate(input, :crc_32)` |

The Erlang `crc` module has the same changes, e.g. `crc:crc(Model, Input)` becomes `crc:calculate(Input, Model)`.

`CRC.checksum_xor/1` is not deprecated. The `crc_fast`, `crc_slow` and `crc_nif` modules are removed in v1.0, and the resource returned by `CRC.init/1` is no longer a NIF reference, so treat it as opaque.

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

## Copyright and License

Copyright (c) 2026 Rodney Norris

CRC is released under the MIT License. See the [LICENSE.md](./LICENSE.md) file
for further details.
