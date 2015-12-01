# CRC

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to iterate over the given binary calculating the CRC checksum value.

## Installation

  1. Add crc to your list of dependencies in `mix.exs`:

        def deps do
          [{:crc, "~> 0.1.0"}]
        end

## TODO:
  - Add more CRC algorithms:
    - CRC-16 Modbus
    - CRC-16 Sick
    - CRC-32
    - CRC-DNP
  - update ccitt to support Kermit
