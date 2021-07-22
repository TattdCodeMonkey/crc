# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.10.2 - 2021-07-21
- fix compiling on non x86_64 architecture (M1 mac)

## v0.10.1 - 2020-06-12
- Removed usage of `ERL_INTERFACE_INCLUDE_DIR`, it was removed in OTP 23

## v0.10.0 - 2020-01-31
### Added
- Change build path to use `MIX_APP_PATH` if available. This should improve compilation for multiple nerves targets.

## v0.9.2 - 2019-04-09
### Fixes
- Fixed GCC warning implicit declaration of strnlen on GNU/Linux

## v0.9.1 - 2018-03-09
### Fixes
- Removed files from Windows Makefile to fix compiling on Windows.

## v0.9.0 - 2018-03-09
### Added
- `CRC.calculate/2` generic calculate function that takes the input as first
  parameter. This is a copy of `CRC.crc/2` but should allow easier transition
  from the legacy model functions ie `CRC.crc_16` etc.
- Many README and Documentation tweaks / improvements.

## v0.8.3 - 2018-02-09
### Added
- `CRC` module docs
- Docs for generic `CRC.crc` functions
### Changes
- Migrated erlang model specific functions to use `crc_fast` instead of model specific NIFs
- Removed model specific NIFs
- Moved model specific functions from `CRC` to `CRC.Legacy`
- Removed redundant tests

## v0.8.2 - 2018-01-17
### Fixes
- Fixed cross-compile settings
- Removed compile-time reference to NIF

## v0.8.1 - 2018-01-11
### Fixes
- Restored Windows build support #21
### Added
- `CRC.list/0` & `CRC.list/1` functions to get full list of pre-defined models

## v0.8.0 - 2017-12-22
### Added
- Generic CRC calculation NIFs `CRC.crc/2`, `CRC.crc_init/1`, `CRC.crc_update/2`, `CRC.crc_final/1`
- PYCRC property tests to TravisCI build

## v0.7.1 - 2017-11-29
### Fixes
- Removed compiled NIF files from release #16

## v0.7.0 - 2017-10-19
### Added
- Timeslice reduction counting to NIFs, allowing NIF functions to play nice with the BEAM Scheduler
- Erlang support & rebar configs
- NIF based XOR checksum

## v0.6.0 - 2017-08-28
### Added
- CRC-16 Kermit
- Added TravisCI
