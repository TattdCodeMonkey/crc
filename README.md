# CRC

[![Build
Status](https://travis-ci.org/TattdCodeMonkey/crc.png?branch=master)](https://travis-ci.org/TattdCodeMonkey/crc)
[![Hex version](https://img.shields.io/hexpm/v/crc.svg "Hex
version")](https://hex.pm/packages/crc)

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to interate over the given binary calculating the CRC checksum value.

CRC implementations have been tested against these online calculators to validate their correctness to the best of our ability.

-  https://www.lammertbies.nl/comm/info/crc-calculation.html
-  http://www.sunshine2k.de/coding/javascript/crc/crc_js.html

## Installation

  1. Add crc to your list of dependencies in `mix.exs`:

```elixir
  def deps do
    [{:crc, "~> 0.8"}]
  end
```

## Supported algorithms

- See [crc_model_<type>.c.h](/c_src/nif) files

