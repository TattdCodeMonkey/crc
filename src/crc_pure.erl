%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(crc_pure).
-behaviour(crc_algorithm).

%% crc_algorithm callbacks
-export([calc/2]).
-export([init/1]).
-export([update/2]).
-export([final/1]).
-export([info/1]).
-export([residue/1]).
%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% CRC API
-export([crc_reflect/2]).
-export([crc_init/1]).
-export([crc_update/3]).
-export([crc_final/2]).
-export([crc_residue/1]).
%% Checksum API
-export([checksum_xor/1]).
%% Cache API
-export([clear_cache/0]).
-export([clear_cache/1]).
%% SICK API
-export([sick_init/1]).
-export([sick_update/3]).
-export([sick_final/2]).

%%====================================================================
%% crc_algorithm callbacks
%%====================================================================

calc(Params, Iodata) ->
	Resource0 = init(Params),
	Resource1 = update(Resource0, Iodata),
	final(Resource1).

init(Params = {Width, Poly, Init, Refin, Refout, Xorout, Check0, Residue0, Sick0})
		when (is_integer(Width) andalso Width >= 0)
		andalso (is_integer(Poly) andalso Poly >= 0)
		andalso (is_integer(Init) andalso Init >= 0)
		andalso is_boolean(Refin) andalso is_boolean(Refout)
		andalso (is_integer(Xorout) andalso Xorout >= 0) ->
	Check =
		case Check0 of
			_ when is_integer(Check0) andalso Check0 >= 0 -> Check0;
			nil -> 0;
			_ -> erlang:error({badarg, [Params]})
		end,
	Residue =
		case Residue0 of
			_ when is_integer(Residue0) andalso Residue0 >= 0 -> Residue0;
			nil -> 0;
			_ -> erlang:error({badarg, [Params]})
		end,
	Sick =
		case Sick0 of
			_ when is_boolean(Sick0) -> Sick0;
			nil -> false;
			_ -> erlang:error({badarg, [Params]})
		end,
	MSBMask = 1 bsl (Width - 1),
	CRCMask = 1 bor ((MSBMask - 1) bsl 1),
	CRCShift = case Width < 8 of true -> (8 - Width); false -> 0 end,
	case Poly > CRCMask orelse Init > CRCMask orelse Xorout > CRCMask orelse Check > CRCMask orelse Residue > CRCMask of
		true ->
			erlang:error({badarg, [Params]});
		false ->
			Resource = '__struct__'(#{
				width => Width,
				poly => Poly,
				init => Init,
				refin => Refin,
				refout => Refout,
				xorout => Xorout,
				check => Check,
				residue => Residue,
				sick => Sick,
				msb_mask => MSBMask,
				crc_mask => CRCMask,
				crc_shift => CRCShift
			}),
			case Sick of
				false ->
					Tabled = Resource#{ table := crc_table(Resource) },
					Tabled#{ value := crc_init(Tabled) };
				true ->
					{Value, Extra} = sick_init(Resource),
					Resource#{ value := Value, extra := Extra }
			end
	end;
init(Key) when is_atom(Key) ->
	%% Built-in models are initialised once and cached; a resource is an
	%% immutable map so the cached copy can be shared by every caller.
	%% Only built-in models are cached (in persistent_term, which suits a
	%% small fixed set of keys): there are a bounded number of them, while
	%% custom models can be created without limit at runtime.
	%%
	%% Aliases share their root model's entry. The resource keeps its
	%% lookup table, and table_key/1 maps the table's parameters to the
	%% model holding it so custom models with the same width, polynomial and
	%% reflection (such as those made with `extend') can reuse it.
	case crc_models:get(Key) of
		undefined ->
			erlang:error({badarg, [Key]});
		Model = #{ key := Root } ->
			CacheKey = model_key(Root),
			case persistent_term:get(CacheKey, undefined) of
				undefined ->
					Resource = init(Model),
					persistent_term:put(CacheKey, Resource),
					TableKey = table_key(Resource),
					case Resource of
						%% SICK models do not use a table
						#{ sick := true } ->
							ok;
						%% Only index the first model with these parameters:
						%% replacing an existing persistent_term value is
						%% expensive (see clear_cache/0).
						_ ->
							case persistent_term:get(TableKey, undefined) of
								undefined -> persistent_term:put(TableKey, Root);
								_ -> ok
							end
					end,
					Resource;
				Cached ->
					Cached
			end
	end;
init(Params = #{ extend := Base }) ->
	init(maps:merge(extend_base(Base, Params), maps:remove(extend, Params)));
init(Params = #{
	width := Width,
	poly := Poly,
	init := Init,
	refin := Refin,
	refout := Refout,
	xorout := Xorout
})
		when (is_integer(Width) andalso Width >= 0)
		andalso (is_integer(Poly) andalso Poly >= 0)
		andalso (is_integer(Init) andalso Init >= 0)
		andalso is_boolean(Refin) andalso is_boolean(Refout)
		andalso (is_integer(Xorout) andalso Xorout >= 0) ->
	Check = maps:get(check, Params, nil),
	Residue = maps:get(residue, Params, nil),
	Sick = maps:get(sick, Params, nil),
	init({Width, Poly, Init, Refin, Refout, Xorout, Check, Residue, Sick});
init(BadParams) ->
	erlang:error({badarg, [BadParams]}).

update(Resource=#{ '__struct__' := ?MODULE, value := Value, sick := false }, Iodata) ->
	Resource#{ value := crc_update(Resource, Value, erlang:iolist_to_binary(Iodata)) };
update(Resource=#{ '__struct__' := ?MODULE, value := Value, extra := Extra, sick := true }, Iodata) ->
	{NewValue, NewExtra} = sick_update(Resource, {Value, Extra}, erlang:iolist_to_binary(Iodata)),
	Resource#{ value := NewValue, extra := NewExtra }.

final(Resource=#{ '__struct__' := ?MODULE, value := Value, sick := false }) ->
	crc_final(Resource, Value);
final(Resource=#{ '__struct__' := ?MODULE, value := Value, sick := true }) ->
	sick_final(Resource, Value).

info(#{
	'__struct__' := ?MODULE,
	width := Width,
	poly := Poly,
	init := Init,
	refin := Refin,
	refout := Refout,
	xorout := Xorout,
	check := Check,
	residue := Residue,
	sick := Sick
}) ->
	#{
		width => Width,
		poly => Poly,
		init => Init,
		refin => Refin,
		refout => Refout,
		xorout => Xorout,
		check => Check,
		residue => Residue,
		sick => Sick
	};
info(Params) ->
	info(init(Params)).

residue(Resource=#{ '__struct__' := ?MODULE, sick := false }) ->
	crc_residue(Resource);
residue(Resource=#{ '__struct__' := ?MODULE, width := Width, refout := Refout, xorout := Xorout0, sick := true }) ->
	Xorout =
		case Refout of
			true -> crc_reflect(Xorout0, Width);
			false -> Xorout0
		end,
	Copy = Resource#{ init := 0, xorout := 0, value := 0 },
	{CRC, _Extra} = sick_update(Copy, {0, 0}, << Xorout:Width >>),
	sick_final(Copy, CRC);
residue(Params) ->
	residue(init(Params)).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		width => nil,
		poly => nil,
		init => nil,
		refin => nil,
		refout => nil,
		xorout => nil,
		check => nil,
		residue => nil,
		sick => nil,
		msb_mask => nil,
		crc_mask => nil,
		crc_shift => nil,
		table => nil,
		value => nil,
		extra => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% CRC API functions
%%%===================================================================

crc_reflect(Reg, Width)
		when (is_integer(Reg) andalso Reg >= 0)
		andalso (is_integer(Width) andalso Width >= 0) ->
	Res = Reg band 16#01,
	do_crc_reflect(Res, Reg, 0, Width - 1).

%% The register is kept in the form the table-driven loop works on:
%%
%%   * reflected (refin = true): the bit-reversed CRC, `Width' bits wide.
%%   * normal (refin = false): the CRC left-aligned to at least 8 bits,
%%     so widths below 8 can share the byte-at-a-time table.
%%
%% Widths of 60 to 64 bits are held as a `{High, Low}' pair of 32-bit
%% halves so the loop never allocates bignums.
%%
%% Tables are only cached for built-in models (see init/1). A custom model
%% that shares a built-in model's width, polynomial and reflection, such as
%% one made with `extend', reuses that table; any other custom model builds
%% its own table (about 50 microseconds) each time it is initialised.
crc_table(Params = #{ width := Width, poly := Poly, refin := Refin }) ->
	case persistent_term:get(table_key(Params), undefined) of
		undefined ->
			build_table(Width, Poly, Refin);
		Root ->
			%% The model may have been removed by clear_cache/0,1 since the
			%% index was read.
			case persistent_term:get(model_key(Root), undefined) of
				#{ table := Table } -> Table;
				undefined -> build_table(Width, Poly, Refin)
			end
	end.

crc_init(#{ width := Width, init := Init, refin := Refin, table := {Mode, _} }) ->
	CRC =
		case Refin of
			true -> crc_reflect(Init, Width);
			false -> Init bsl normal_shift(Width)
		end,
	to_register(Mode, CRC).

crc_update(#{}, CRC, <<>>) ->
	CRC;
crc_update(#{ table := {reflected, Table} }, CRC, Rest) ->
	do_reflected_update(Rest, Table, CRC);
crc_update(#{ width := Width, table := {normal, Table} }, CRC, Rest) ->
	Bits = max(Width, 8),
	do_normal_update(Rest, Table, CRC, Bits - 8, (1 bsl (Bits - 8)) - 1);
crc_update(#{ table := {reflected_split, Table} }, CRC, Rest) ->
	do_reflected_split_update(Rest, Table, CRC);
crc_update(#{ width := Width, table := {normal_split, Table} }, CRC, Rest) ->
	HighBits = Width - 32,
	do_normal_split_update(Rest, Table, CRC, HighBits - 8, (1 bsl (HighBits - 8)) - 1).

crc_final(#{
	width := Width,
	refin := Refin,
	refout := Refout,
	xorout := Xorout,
	crc_mask := CRCMask,
	table := {Mode, _}
}, Register) ->
	CRC0 = from_register(Mode, Register),
	CRC1 =
		case Refin of
			true -> CRC0;
			false -> CRC0 bsr normal_shift(Width)
		end,
	CRC2 =
		case Refin =:= Refout of
			true -> CRC1;
			false -> crc_reflect(CRC1, Width)
		end,
	(CRC2 bxor Xorout) band CRCMask.

crc_residue(Resource = #{
	'__struct__' := ?MODULE,
	width := Width,
	refin := Refin,
	refout := Refout,
	xorout := Xorout0
}) ->
	Copy = Resource#{ init := 0, xorout := 0, value := 0 },
	Xorout =
		case Refout of
			true -> crc_reflect(Xorout0, Width);
			false -> Xorout0
		end,
	Residue = do_crc_residue(Copy, Xorout),
	case Refin of
		true -> crc_reflect(Residue, Width);
		false -> Residue
	end.

%%%===================================================================
%%% Checksum API functions
%%%===================================================================

checksum_xor(Input) when is_binary(Input) ->
	do_checksum_xor(Input, 0);
checksum_xor(Input) ->
	erlang:error({badarg, [Input]}).

%%%===================================================================
%%% Cache API functions
%%%===================================================================

%% Built-in models are cached in persistent_term the first time they are
%% used, and stay cached until the VM stops. These functions remove them;
%% the next use of a model rebuilds it.
%%
%% Erasing a persistent_term value makes the VM scan every process for
%% references to it, so clearing is expensive on a busy system. Use it
%% rarely, e.g. in tests or after a one-off job that used many models, and
%% never on a timer.

clear_cache() ->
	_ = [persistent_term:erase(Key) || {Key, _} <- persistent_term:get(), is_cache_key(Key)],
	ok.

clear_cache(Key) when is_atom(Key) ->
	case crc_models:get(Key) of
		undefined ->
			erlang:error({badarg, [Key]});
		#{ key := Root } ->
			case persistent_term:get(model_key(Root), undefined) of
				undefined ->
					ok;
				Resource ->
					TableKey = table_key(Resource),
					_ = case persistent_term:get(TableKey, undefined) of
						Root -> persistent_term:erase(TableKey);
						_ -> false
					end,
					_ = persistent_term:erase(model_key(Root)),
					ok
			end
	end;
clear_cache(Key) ->
	erlang:error({badarg, [Key]}).

%%%===================================================================
%%% SICK API functions
%%%===================================================================

sick_init(#{
	width := 16,
	init := Init
}) ->
	{Init, 0};
sick_init(Params) ->
	erlang:error({badarg, [Params]}).

sick_update(#{ width := 16 }, {CRC, PrevByte}, <<>>) ->
	{CRC, PrevByte};
sick_update(#{
	width := 16,
	poly := Poly,
	msb_mask := MSBMask,
	crc_mask := CRCMask,
	crc_shift := CRCShift
}, {CRC, PrevByte}, Rest) ->
	do_sick_update(CRC, Rest, PrevByte, Poly, MSBMask, CRCMask, CRCShift);
sick_update(Params, CRC, Rest) ->
	erlang:error({badarg, [Params, CRC, Rest]}).

sick_final(#{ width := 16 }, CRC) ->
	LowByte = (CRC band 16#ff00) bsr 8,
	HighByte = (CRC band 16#00ff) bsl 8,
	(LowByte bor HighByte);
sick_final(Params, CRC) ->
	erlang:error({badarg, [Params, CRC]}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
do_crc_reflect(Res, _Reg, Max, Max) ->
	Res;
do_crc_reflect(Res0, Reg0, I, Max) ->
	Reg1 = Reg0 bsr 1,
	Res1 = Res0 bsl 1,
	Res2 = Res1 bor (Reg1 band 16#01),
	do_crc_reflect(Res2, Reg1, I + 1, Max).

%% @private
is_cache_key({?MODULE, model, _Root}) -> true;
is_cache_key({?MODULE, table, _Width, _Poly, _Refin}) -> true;
is_cache_key(_) -> false.

%% @private
model_key(Root) ->
	{?MODULE, model, Root}.

%% @private
table_key(#{ width := Width, poly := Poly, refin := Refin }) ->
	{?MODULE, table, Width, Poly, Refin}.

%% @private
%% Parameters of the model named by `extend': a built-in model name or
%% alias, a map of parameters (which may itself use `extend'), or a
%% resource returned by init/1.
extend_base(Key, Params) when is_atom(Key) ->
	case crc_models:get(Key) of
		undefined -> erlang:error({badarg, [Params]});
		_Model -> info(init(Key))
	end;
extend_base(Resource = #{ '__struct__' := ?MODULE }, _Params) ->
	info(Resource);
extend_base(Base, _Params) when is_map(Base) ->
	info(init(Base));
extend_base(_Base, Params) ->
	erlang:error({badarg, [Params]}).

%% @private
%% Bits the register of a normal (non-reflected) CRC is shifted left by so
%% that it is at least one byte wide.
normal_shift(Width) when Width < 8 -> 8 - Width;
normal_shift(_Width) -> 0.

%% @private
%% Registers of 60 bits or more would be bignums; split them in two.
split_width(Width) -> Width >= 60 andalso Width =< 64.

%% @private
to_register(Mode, CRC) when Mode =:= reflected_split orelse Mode =:= normal_split ->
	{CRC bsr 32, CRC band 16#FFFFFFFF};
to_register(_Mode, CRC) ->
	CRC.

%% @private
from_register(_Mode, {High, Low}) ->
	(High bsl 32) bor Low;
from_register(_Mode, CRC) ->
	CRC.

%% @private
build_table(Width, Poly, true) ->
	RPoly = crc_reflect(Poly, Width),
	Entry = fun(Index) -> reflected_entry(Index, RPoly, 8) end,
	Mode = case split_width(Width) of true -> reflected_split; false -> reflected end,
	{Mode, make_table(Mode, Entry)};
build_table(Width, Poly, false) ->
	Bits = max(Width, 8),
	Shift = normal_shift(Width),
	TopBit = 1 bsl (Bits - 1),
	Mask = (1 bsl Bits) - 1,
	Entry = fun(Index) -> normal_entry(Index bsl (Bits - 8), Poly bsl Shift, TopBit, Mask, 8) end,
	Mode = case split_width(Width) of true -> normal_split; false -> normal end,
	{Mode, make_table(Mode, Entry)}.

%% @private
make_table(Mode, Entry) ->
	list_to_tuple([to_register(Mode, Entry(Index)) || Index <- lists:seq(0, 255)]).

%% @private
reflected_entry(CRC, _RPoly, 0) ->
	CRC;
reflected_entry(CRC, RPoly, N) when CRC band 1 =:= 1 ->
	reflected_entry((CRC bsr 1) bxor RPoly, RPoly, N - 1);
reflected_entry(CRC, RPoly, N) ->
	reflected_entry(CRC bsr 1, RPoly, N - 1).

%% @private
normal_entry(CRC, _Poly, _TopBit, _Mask, 0) ->
	CRC;
normal_entry(CRC, Poly, TopBit, Mask, N) when CRC band TopBit =/= 0 ->
	normal_entry(((CRC bsl 1) band Mask) bxor Poly, Poly, TopBit, Mask, N - 1);
normal_entry(CRC, Poly, TopBit, Mask, N) ->
	normal_entry((CRC bsl 1) band Mask, Poly, TopBit, Mask, N - 1).

%% @private
do_reflected_update(<< Octet, Rest/binary >>, Table, CRC) ->
	do_reflected_update(Rest, Table, (CRC bsr 8) bxor element(((CRC bxor Octet) band 16#FF) + 1, Table));
do_reflected_update(<<>>, _Table, CRC) ->
	CRC.

%% @private
do_normal_update(<< Octet, Rest/binary >>, Table, CRC, Shift, LowMask) ->
	Index = ((CRC bsr Shift) bxor Octet) band 16#FF,
	do_normal_update(Rest, Table, ((CRC band LowMask) bsl 8) bxor element(Index + 1, Table), Shift, LowMask);
do_normal_update(<<>>, _Table, CRC, _Shift, _LowMask) ->
	CRC.

%% @private
do_reflected_split_update(<< Octet, Rest/binary >>, Table, {High, Low}) ->
	{TableHigh, TableLow} = element(((Low bxor Octet) band 16#FF) + 1, Table),
	do_reflected_split_update(Rest, Table, {
		(High bsr 8) bxor TableHigh,
		((Low bsr 8) bor ((High band 16#FF) bsl 24)) bxor TableLow
	});
do_reflected_split_update(<<>>, _Table, CRC) ->
	CRC.

%% @private
do_normal_split_update(<< Octet, Rest/binary >>, Table, {High, Low}, Shift, LowMask) ->
	{TableHigh, TableLow} = element((((High bsr Shift) bxor Octet) band 16#FF) + 1, Table),
	do_normal_split_update(Rest, Table, {
		(((High band LowMask) bsl 8) bor (Low bsr 24)) bxor TableHigh,
		((Low band 16#FFFFFF) bsl 8) bxor TableLow
	}, Shift, LowMask);
do_normal_split_update(<<>>, _Table, CRC, _Shift, _LowMask) ->
	CRC.

%% @private
do_crc_residue(#{
	width := Width,
	poly := Poly,
	init := Init,
	xorout := Xorout,
	msb_mask := MSBMask,
	crc_mask := CRCMask
}, Message) ->
	do_crc_residue(Init bxor Message, 0, Width, Poly, Xorout, MSBMask, CRCMask).

%% @private
do_crc_residue(Rem, Max, Max, _Poly, Xorout, _MSBMask, CRCMask) ->
	(Rem bxor Xorout) band CRCMask;
do_crc_residue(Rem, I, Max, Poly, Xorout, MSBMask, CRCMask) ->
	case Rem band MSBMask of
		0 ->
			do_crc_residue(Rem bsl 1, I + 1, Max, Poly, Xorout, MSBMask, CRCMask);
		_ ->
			do_crc_residue((Rem bsl 1) bxor Poly, I + 1, Max, Poly, Xorout, MSBMask, CRCMask)
	end.

%% @private
do_checksum_xor(<< Octet, Rest/binary >>, Sum) ->
	do_checksum_xor(Rest, Sum bxor Octet);
do_checksum_xor(<<>>, Sum) ->
	Sum.

%% @private
do_sick_update(CRC0, << NextByte0, Rest/binary >>, PrevByte0, Poly, MSBMask, CRCMask, CRCShift) ->
	NextByte1 = 16#00ff band NextByte0,
	CRC1 =
		case (CRC0 band (MSBMask bsl CRCShift)) of
			0 ->
				CRC0 bsl 1;
			_ ->
				(CRC0 bsl 1) bxor (Poly bsl CRCShift)
		end,
	CRC2 = CRC1 bxor (NextByte1 bor PrevByte0),
	PrevByte1 = NextByte1 bsl 8,
	do_sick_update(CRC2, Rest, PrevByte1, Poly, MSBMask, CRCMask, CRCShift);
do_sick_update(CRC, <<>>, PrevByte, _Poly, _MSBMask, CRCMask, _CRCShift) ->
	{CRC band CRCMask, PrevByte}.
