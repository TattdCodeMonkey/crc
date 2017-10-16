%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(crc_nif).

%% NIF Functions
-export([crc_8/2]).
-export([crc_8_init/1]).
-export([crc_8_update/2]).
-export([crc_8_final/1]).
-export([crc_16/1]).
-export([crc_16_init/0]).
-export([crc_16_update/2]).
-export([crc_16_final/1]).
-export([crc_16_ccitt/2]).
-export([crc_16_ccitt_init/1]).
-export([crc_16_ccitt_update/2]).
-export([crc_16_ccitt_final/1]).
-export([crc_16_kermit/2]).
-export([crc_16_kermit_init/1]).
-export([crc_16_kermit_update/2]).
-export([crc_16_kermit_final/1]).
-export([crc_16_modbus/1]).
-export([crc_16_modbus_init/0]).
-export([crc_16_modbus_update/2]).
-export([crc_16_modbus_final/1]).

-on_load(init/0).

%%%===================================================================
%%% NIF Functions
%%%===================================================================

crc_8(_Seed, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_8_init(_Seed) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_8_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_8_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_init() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_ccitt(_Seed, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_ccitt_init(_Seed) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_ccitt_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_ccitt_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_kermit(_Seed, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_kermit_init(_Seed) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_kermit_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_kermit_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_modbus(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_modbus_init() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_modbus_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_modbus_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(crc:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
