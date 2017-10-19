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
    [{:crc, "~> 0.7"}]
  end
```

## Supported algorithms

- CRC-8 (Koopman)
- CRC-16 (ANSI)
- CCITT-16
- CCITT-16 X-modem
- CCITT-16 1-DOF
- CRC-16 modbus
- CRC-16 KERMIT
- CRC-16 SICK
- CRC-16 DNP
- CRC-32
- XOR Checksum

