%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(crc).

%% Public API
-export([priv_dir/0]).

%%%===================================================================
%%% Public API
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
