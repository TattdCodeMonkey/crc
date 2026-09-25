%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(crc).

%% Public API
-export([calculate/2]).
-export([init/1]).
-export([update/2]).
-export([final/1]).
-export([info/1]).
-export([residue/1]).
-export([list/0]).
-export([list/1]).
-export([checksum_xor/1]).
%% Deprecated API, removed in v1.0
-export([crc/2]).
-export([crc_init/1]).
-export([crc_update/2]).
-export([crc_final/1]).
-export([crc_8/1]).
-export([crc_8/2]).
-export([crc_16/1]).
-export([ccitt_16/1]).
-export([ccitt_16/2]).
-export([ccitt_16_kermit/1]).
-export([ccitt_16_kermit/2]).
-export([ccitt_16_xmodem/1]).
-export([ccitt_16_1D0F/1]).
-export([crc_16_dnp/1]).
-export([crc_16_modbus/1]).
-export([crc_16_sick/1]).
-export([crc_32/1]).
%% Internal API
-export([priv_dir/0]).

-deprecated([
	{crc, 2, "use crc:calculate/2 instead (note the argument order: Input, Model)"},
	{crc_init, 1, "use crc:init/1 instead"},
	{crc_update, 2, "use crc:update/2 instead"},
	{crc_final, 1, "use crc:final/1 instead"},
	{crc_8, 1, "use crc:calculate(Input, #{extend => crc_8_koop, init => 0}) instead"},
	{crc_8, 2, "use crc:calculate(Input, #{extend => crc_8_koop, init => Init}) instead, "
		"where Init is the bit-reflected value of (Seed bxor 16#FF)"},
	{crc_16, 1, "use crc:calculate(Input, crc_16) instead"},
	{ccitt_16, 1, "use crc:calculate(Input, crc_16_ccitt_false) instead"},
	{ccitt_16, 2, "use crc:calculate(Input, #{extend => crc_16_ccitt_false, init => Seed}) instead"},
	{ccitt_16_kermit, 1, "use crc:calculate(Input, crc_16_kermit) instead"},
	{ccitt_16_kermit, 2, "use crc:calculate(Input, #{extend => crc_16_kermit, init => Seed}) instead"},
	{ccitt_16_xmodem, 1, "use crc:calculate(Input, xmodem) instead"},
	{ccitt_16_1D0F, 1, "use crc:calculate(Input, #{extend => crc_16_ccitt_false, init => 16#1D0F}) instead"},
	{crc_16_dnp, 1, "use crc:calculate(Input, crc_16_dnp) instead"},
	{crc_16_modbus, 1, "use crc:calculate(Input, crc_16_modbus) instead"},
	{crc_16_sick, 1, "use crc:calculate(Input, crc_16_sick) instead"},
	{crc_32, 1, "use crc:calculate(Input, crc_32) instead"}
]).

%% Types
-type uint8_t() :: 16#00..16#FF.
-type uint16_t() :: 16#0000..16#FFFF.
-type uint32_t() :: 16#00000000..16#FFFFFFFF.

%%%===================================================================
%%% Public API Functions
%%%===================================================================

%% @doc Calculates the CRC of `Input' using `Model': a pre-defined model
%% name or alias (see list/0), a map of model parameters, or a map with an
%% `extend' key naming a pre-defined model plus the parameters to override.
-spec calculate(iodata(), crc_algorithm:params()) -> crc_algorithm:value().
calculate(Input, Model) ->
	crc_fast:calc(Model, Input).

%% @doc Starts a multi-part CRC calculation for `Model'.
-spec init(crc_algorithm:params()) -> crc_algorithm:resource().
init(Model) ->
	crc_fast:init(Model).

%% @doc Continues a multi-part CRC calculation with `Input', returning a
%% new resource to pass to the next update/2 or final/1 call.
-spec update(crc_algorithm:resource(), iodata()) -> crc_algorithm:resource().
update(Resource, Input) ->
	crc_fast:update(Resource, Input).

%% @doc Finishes a multi-part CRC calculation and returns the CRC.
-spec final(crc_algorithm:resource()) -> crc_algorithm:value().
final(Resource) ->
	crc_fast:final(Resource).

%% @doc Returns the parameters of a model or of a resource from init/1.
-spec info(crc_algorithm:params() | crc_algorithm:resource()) -> crc_algorithm:info().
info(ModelOrResource) ->
	Info = crc_fast:info(to_resource(ModelOrResource)),
	maps:with([width, poly, init, refin, refout, xorout, check, residue, sick], Info).

%% @doc Returns the residue of a model or of a resource from init/1.
-spec residue(crc_algorithm:params() | crc_algorithm:resource()) -> crc_algorithm:value().
residue(ModelOrResource) ->
	crc_fast:residue(to_resource(ModelOrResource)).

%% @doc Returns every pre-defined model as `{Key, Name}'.
-spec list() -> [{atom(), binary()}].
list() ->
	crc_models:list().

%% @doc Returns the pre-defined models whose key or name matches the
%% regular expression `Filter'.
-spec list(iodata()) -> [{atom(), binary()}].
list(Filter) ->
	case re:compile(Filter) of
		{ok, Regex} ->
			[Model || Model = {Key, Name} <- list(),
				re:run(atom_to_binary(Key, utf8), Regex, [{capture, none}]) =:= match
				orelse re:run(Name, Regex, [{capture, none}]) =:= match];
		{error, _} ->
			erlang:error({badarg, [Filter]})
	end.

%% @doc Calculates an 8-bit XOR checksum of `Input'.
-spec checksum_xor(binary()) -> uint8_t().
checksum_xor(Input) ->
	crc_nif:checksum_xor(Input).

%%%===================================================================
%%% Deprecated API Functions
%%%===================================================================

-spec crc(crc_algorithm:model(), iodata()) -> crc_algorithm:value().
crc(Model, Input) ->
	crc_fast:calc(Model, Input).

-spec crc_init(crc_algorithm:model()) -> crc_algorithm:resource().
crc_init(Model) ->
	crc_fast:init(Model).

-spec crc_update(crc_algorithm:resource(), iodata()) -> crc_algorithm:resource().
crc_update(Resource, Input) ->
	crc_fast:update(Resource, Input).

-spec crc_final(crc_algorithm:resource()) -> crc_algorithm:value().
crc_final(Resource) ->
	crc_fast:final(Resource).

-spec crc_8(binary()) -> uint8_t().
crc_8(Input) ->
	crc_8(Input, 16#FF).

-spec crc_8(binary(), uint8_t()) -> uint8_t().
crc_8(Input, Seed) ->
	crc_nif:crc_8(Seed, Input).

-spec crc_16(binary()) -> uint16_t().
crc_16(Input) ->
	crc_fast:calc(crc_16, Input).

-spec ccitt_16(binary()) -> uint16_t().
ccitt_16(Input) ->
	crc_fast:calc(crc_16_ccitt_false, Input).

-spec ccitt_16(binary(), uint16_t()) -> uint16_t().
ccitt_16(Input, Seed) ->
	crc_fast:calc(#{extend => crc_16_ccitt_false, init => Seed}, Input).

-spec ccitt_16_kermit(binary()) -> uint16_t().
ccitt_16_kermit(Input) ->
	crc_fast:calc(crc_16_kermit, Input).

-spec ccitt_16_kermit(binary(), uint16_t()) -> uint16_t().
ccitt_16_kermit(Input, Seed) ->
	crc_fast:calc(#{extend => crc_16_kermit, init => Seed}, Input).

-spec ccitt_16_xmodem(binary()) -> uint16_t().
ccitt_16_xmodem(Input) ->
	crc_fast:calc(xmodem, Input).

-spec ccitt_16_1D0F(binary()) -> uint16_t().
ccitt_16_1D0F(Input) ->
	crc_fast:calc(#{extend => crc_16_ccitt_false, init => 16#1D0F}, Input).

-spec crc_16_dnp(binary()) -> uint16_t().
crc_16_dnp(Input) ->
	crc_fast:calc(crc_16_dnp, Input).

-spec crc_16_modbus(binary()) -> uint16_t().
crc_16_modbus(Input) ->
	crc_fast:calc(crc_16_modbus, Input).

-spec crc_16_sick(binary()) -> uint16_t().
crc_16_sick(Input) ->
	crc_fast:calc(crc_16_sick, Input).

-spec crc_32(binary()) -> uint32_t().
crc_32(Input) ->
	crc_fast:calc(crc_32, Input).

%%%===================================================================
%%% Internal API Functions
%%%===================================================================

-spec priv_dir() -> file:filename_all().
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
to_resource(Resource) when is_reference(Resource) ->
	Resource;
to_resource(Model) ->
	crc_fast:init(Model).
