# CRC

This module is used to calculate CRC (Cyclic Redundancy Check) values for binary data. It uses NIF functions written in C to iterate over the given binary calculating the CRC checksum value.

## Installation

  1. Add crc to your list of dependencies in `mix.exs`:

        def deps do
          [{:crc, "~> 0.5.0"}]
        end

## Supported algorithms

- CRC-8 (Koopman)
- CRC-16
- CCITT-16
- CCITT-16 X modem
- CCITT-16 1DOF
- CRC-16 modbus
- XOR Checksum

## Caution

These are rudimentary implementations that use nif functions. These functions may not be fast enough for larger binaries. If nif functions take too long to execute it can cause problems with the scheduler. More testing needs to be done to determine what is the ideal max size to send, and may require modifications to the functions to calculate the crc's in chunks instead of all at once.

All of this to say, use at your own risk and be sure to do performance testing if your depending on this for any production application.

