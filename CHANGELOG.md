# Changelog
## v0.10.0
### Added
- Change build path to use `MIX_APP_PATH` if available. This should improve compilation for multiple nerves targets.

## v0.9.2
### Fixes
- Fixed gcc warning implicit declaration of strnlen on linux 

## v0.9.1
### Fixes
- Removed files from Windows makefile to fix compiling on Windows.

## v0.9.0
### Added
- `CRC.calculate/2` generic calculate function that takes the input as first
  parameter. This is a copy of `CRC.crc/2` but should allow easier transition
  from the legacy model functions ie `CRC.crc_16` etc.
- Many README and Documentation tweaks / improvements.

## v0.8.3
### Added
- `CRC` module docs
- Docs for generic `CRC.crc` functions
### Changes
- Migrated erlang model specific functions to use `crc_fast` instead of model specific NIFs
- Removed model specific NIFs
- Moved model specific functions from `CRC` to `CRC.Legacy`
- Removed redundant tests

## v0.8.2
### Fixes
- Fixed cross-compile settings
- Removed compile-time reference to NIF

## v0.8.1
### Fixes
- Restored Windows build support #21
### Added
- `CRC.list/0` & `CRC.list/1` functions to get full list of pre-defined models

## v0.8.0
### Added
- Generic CRC calculation NIFs `CRC.crc/2`, `CRC.crc_init/1`, `CRC.crc_update/2`, `CRC.crc_final/1`
- PYCRC property tests to TravisCI build
- 

## v0.7.1
### Fixes
- Removed compiled NIF files from release #16

## v0.7.0
### Added
- Timeslice reduction counting to NIFs, allowing NIF functions to play nice with the BEAM Scheduler
- Erlang support & rebar configs
- NIF based XOR checksum 

## v0.6.0
### Added
- CRC-16 Kermit
- Added TravisCI
