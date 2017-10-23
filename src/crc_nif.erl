%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(crc_nif).

%% NIF Functions
-export([debug_table/1]).
-export([crc/2]).
-export([crc_info/1]).
-export([crc_init/1]).
-export([crc_list/0]).
-export([crc_update/2]).
-export([crc_final/1]).
-export([checksum_xor/1]).
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
-export([crc_16_dnp/1]).
-export([crc_16_dnp_init/0]).
-export([crc_16_dnp_update/2]).
-export([crc_16_dnp_final/1]).
-export([crc_16_kermit/2]).
-export([crc_16_kermit_init/1]).
-export([crc_16_kermit_update/2]).
-export([crc_16_kermit_final/1]).
-export([crc_16_modbus/1]).
-export([crc_16_modbus_init/0]).
-export([crc_16_modbus_update/2]).
-export([crc_16_modbus_final/1]).
-export([crc_16_sick/1]).
-export([crc_16_sick_init/0]).
-export([crc_16_sick_update/2]).
-export([crc_16_sick_final/1]).
-export([crc_32/1]).
-export([crc_32_init/0]).
-export([crc_32_update/2]).
-export([crc_32_final/1]).

-on_load(init/0).

%%%===================================================================
%%% NIF Functions
%%%===================================================================

debug_table(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc(_Params, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_info(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_init(_Params) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_list() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

checksum_xor(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

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

crc_16_dnp(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_dnp_init() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_dnp_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_dnp_final(_Context) ->
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

crc_16_sick(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_sick_init() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_sick_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_16_sick_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_32(_Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_32_init() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_32_update(_Context, _Input) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

crc_32_final(_Context) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(crc:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
