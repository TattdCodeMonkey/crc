%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%
%% Catalogue of pre-defined CRC models.
%%
%% Generated from the model tables of the crc NIF; values follow the
%% Rocksoft Model CRC Algorithm parameters, plus the non-standard `sick`
%% flag for the SICK sensor variant.
-module(crc_models).
-compile({no_auto_import, [get/1]}).

%% API
-export([get/1]).
-export([list/0]).
-export([aliases/0]).

-type key() :: atom().
-type model() :: #{
	key := key(),
	name := binary(),
	width := pos_integer(),
	poly := non_neg_integer(),
	init := non_neg_integer(),
	refin := boolean(),
	refout := boolean(),
	xorout := non_neg_integer(),
	check := non_neg_integer(),
	residue := non_neg_integer(),
	sick := boolean()
}.

-export_type([key/0]).
-export_type([model/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Returns the model for `Key' (a model name or an alias), or
%% `undefined' if there is no such model. Aliases return the model they
%% alias, so `key' is always the root model name.
-spec get(atom()) -> model() | undefined.
get(crc_10) ->
	#{
		key => crc_10,
		name => <<"CRC-10">>,
		width => 10,
		poly => 16#233,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#199,
		residue => 16#0,
		sick => false
	};
get(crc_10_cdma2000) ->
	#{
		key => crc_10_cdma2000,
		name => <<"CRC-10/CDMA2000">>,
		width => 10,
		poly => 16#3D9,
		init => 16#3FF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#233,
		residue => 16#0,
		sick => false
	};
get(crc_10_gsm) ->
	#{
		key => crc_10_gsm,
		name => <<"CRC-10/GSM">>,
		width => 10,
		poly => 16#175,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#3FF,
		check => 16#12A,
		residue => 16#C6,
		sick => false
	};
get(crc_11) ->
	#{
		key => crc_11,
		name => <<"CRC-11">>,
		width => 11,
		poly => 16#385,
		init => 16#1A,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#5A3,
		residue => 16#0,
		sick => false
	};
get(crc_11_umts) ->
	#{
		key => crc_11_umts,
		name => <<"CRC-11/UMTS">>,
		width => 11,
		poly => 16#307,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#61,
		residue => 16#0,
		sick => false
	};
get(crc_12_cdma2000) ->
	#{
		key => crc_12_cdma2000,
		name => <<"CRC-12/CDMA2000">>,
		width => 12,
		poly => 16#F13,
		init => 16#FFF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#D4D,
		residue => 16#0,
		sick => false
	};
get(crc_12_dect) ->
	#{
		key => crc_12_dect,
		name => <<"CRC-12/DECT">>,
		width => 12,
		poly => 16#80F,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#F5B,
		residue => 16#0,
		sick => false
	};
get(crc_12_gsm) ->
	#{
		key => crc_12_gsm,
		name => <<"CRC-12/GSM">>,
		width => 12,
		poly => 16#D31,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FFF,
		check => 16#B34,
		residue => 16#178,
		sick => false
	};
get(crc_12_umts) ->
	#{
		key => crc_12_umts,
		name => <<"CRC-12/UMTS">>,
		width => 12,
		poly => 16#80F,
		init => 16#0,
		refin => false,
		refout => true,
		xorout => 16#0,
		check => 16#DAF,
		residue => 16#0,
		sick => false
	};
get(crc_13_bbc) ->
	#{
		key => crc_13_bbc,
		name => <<"CRC-13/BBC">>,
		width => 13,
		poly => 16#1CF5,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#4FA,
		residue => 16#0,
		sick => false
	};
get(crc_14_darc) ->
	#{
		key => crc_14_darc,
		name => <<"CRC-14/DARC">>,
		width => 14,
		poly => 16#805,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#82D,
		residue => 16#0,
		sick => false
	};
get(crc_14_gsm) ->
	#{
		key => crc_14_gsm,
		name => <<"CRC-14/GSM">>,
		width => 14,
		poly => 16#202D,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#3FFF,
		check => 16#30AE,
		residue => 16#31E,
		sick => false
	};
get(crc_15) ->
	#{
		key => crc_15,
		name => <<"CRC-15">>,
		width => 15,
		poly => 16#4599,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#59E,
		residue => 16#0,
		sick => false
	};
get(crc_15_mpt1327) ->
	#{
		key => crc_15_mpt1327,
		name => <<"CRC-15/MPT1327">>,
		width => 15,
		poly => 16#6815,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#1,
		check => 16#2566,
		residue => 16#6815,
		sick => false
	};
get(crc_16) ->
	#{
		key => crc_16,
		name => <<"CRC-16">>,
		width => 16,
		poly => 16#8005,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#BB3D,
		residue => 16#0,
		sick => false
	};
get(crc_16_a) ->
	#{
		key => crc_16_a,
		name => <<"CRC-16/A">>,
		width => 16,
		poly => 16#1021,
		init => 16#C6C6,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#BF05,
		residue => 16#0,
		sick => false
	};
get(crc_16_aug_ccitt) ->
	#{
		key => crc_16_aug_ccitt,
		name => <<"CRC-16/AUG-CCITT">>,
		width => 16,
		poly => 16#1021,
		init => 16#1D0F,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#E5CC,
		residue => 16#0,
		sick => false
	};
get(crc_16_buypass) ->
	#{
		key => crc_16_buypass,
		name => <<"CRC-16/BUYPASS">>,
		width => 16,
		poly => 16#8005,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#FEE8,
		residue => 16#0,
		sick => false
	};
get(crc_16_ccitt) ->
	#{
		key => crc_16_ccitt,
		name => <<"CRC-16/CCITT">>,
		width => 16,
		poly => 16#1021,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#2189,
		residue => 16#0,
		sick => false
	};
get(crc_16_ccitt_false) ->
	#{
		key => crc_16_ccitt_false,
		name => <<"CRC-16/CCITT-FALSE">>,
		width => 16,
		poly => 16#1021,
		init => 16#FFFF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#29B1,
		residue => 16#0,
		sick => false
	};
get(crc_16_cdma2000) ->
	#{
		key => crc_16_cdma2000,
		name => <<"CRC-16/CDMA2000">>,
		width => 16,
		poly => 16#C867,
		init => 16#FFFF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#4C06,
		residue => 16#0,
		sick => false
	};
get(crc_16_cms) ->
	#{
		key => crc_16_cms,
		name => <<"CRC-16/CMS">>,
		width => 16,
		poly => 16#8005,
		init => 16#FFFF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#AEE7,
		residue => 16#0,
		sick => false
	};
get(crc_16_dds_110) ->
	#{
		key => crc_16_dds_110,
		name => <<"CRC-16/DDS-110">>,
		width => 16,
		poly => 16#8005,
		init => 16#800D,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#9ECF,
		residue => 16#0,
		sick => false
	};
get(crc_16_dect_r) ->
	#{
		key => crc_16_dect_r,
		name => <<"CRC-16/DECT-R">>,
		width => 16,
		poly => 16#589,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#1,
		check => 16#7E,
		residue => 16#589,
		sick => false
	};
get(crc_16_dect_x) ->
	#{
		key => crc_16_dect_x,
		name => <<"CRC-16/DECT-X">>,
		width => 16,
		poly => 16#589,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#7F,
		residue => 16#0,
		sick => false
	};
get(crc_16_dnp) ->
	#{
		key => crc_16_dnp,
		name => <<"CRC-16/DNP">>,
		width => 16,
		poly => 16#3D65,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#FFFF,
		check => 16#EA82,
		residue => 16#66C5,
		sick => false
	};
get(crc_16_en_13757) ->
	#{
		key => crc_16_en_13757,
		name => <<"CRC-16/EN-13757">>,
		width => 16,
		poly => 16#3D65,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FFFF,
		check => 16#C2B7,
		residue => 16#A366,
		sick => false
	};
get(crc_16_genibus) ->
	#{
		key => crc_16_genibus,
		name => <<"CRC-16/GENIBUS">>,
		width => 16,
		poly => 16#1021,
		init => 16#FFFF,
		refin => false,
		refout => false,
		xorout => 16#FFFF,
		check => 16#D64E,
		residue => 16#1D0F,
		sick => false
	};
get(crc_16_gsm) ->
	#{
		key => crc_16_gsm,
		name => <<"CRC-16/GSM">>,
		width => 16,
		poly => 16#1021,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FFFF,
		check => 16#CE3C,
		residue => 16#1D0F,
		sick => false
	};
get(crc_16_lj1200) ->
	#{
		key => crc_16_lj1200,
		name => <<"CRC-16/LJ1200">>,
		width => 16,
		poly => 16#6F63,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#BDF4,
		residue => 16#0,
		sick => false
	};
get(crc_16_mcrf4xx) ->
	#{
		key => crc_16_mcrf4xx,
		name => <<"CRC-16/MCRF4XX">>,
		width => 16,
		poly => 16#1021,
		init => 16#FFFF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#6F91,
		residue => 16#0,
		sick => false
	};
get(crc_16_modbus) ->
	#{
		key => crc_16_modbus,
		name => <<"CRC-16/MODBUS">>,
		width => 16,
		poly => 16#8005,
		init => 16#FFFF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#4B37,
		residue => 16#0,
		sick => false
	};
get(crc_16_opensafety_a) ->
	#{
		key => crc_16_opensafety_a,
		name => <<"CRC-16/OPENSAFETY-A">>,
		width => 16,
		poly => 16#5935,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#5D38,
		residue => 16#0,
		sick => false
	};
get(crc_16_opensafety_b) ->
	#{
		key => crc_16_opensafety_b,
		name => <<"CRC-16/OPENSAFETY-B">>,
		width => 16,
		poly => 16#755B,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#20FE,
		residue => 16#0,
		sick => false
	};
get(crc_16_profibus) ->
	#{
		key => crc_16_profibus,
		name => <<"CRC-16/PROFIBUS">>,
		width => 16,
		poly => 16#1DCF,
		init => 16#FFFF,
		refin => false,
		refout => false,
		xorout => 16#FFFF,
		check => 16#A819,
		residue => 16#E394,
		sick => false
	};
get(crc_16_riello) ->
	#{
		key => crc_16_riello,
		name => <<"CRC-16/RIELLO">>,
		width => 16,
		poly => 16#1021,
		init => 16#B2AA,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#63D0,
		residue => 16#0,
		sick => false
	};
get(crc_16_sick) ->
	#{
		key => crc_16_sick,
		name => <<"CRC-16/SICK">>,
		width => 16,
		poly => 16#8005,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#56A6,
		residue => 16#0,
		sick => true
	};
get(crc_16_t10_dif) ->
	#{
		key => crc_16_t10_dif,
		name => <<"CRC-16/T10-DIF">>,
		width => 16,
		poly => 16#8BB7,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#D0DB,
		residue => 16#0,
		sick => false
	};
get(crc_16_teledisk) ->
	#{
		key => crc_16_teledisk,
		name => <<"CRC-16/TELEDISK">>,
		width => 16,
		poly => 16#A097,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#FB3,
		residue => 16#0,
		sick => false
	};
get(crc_16_tms37157) ->
	#{
		key => crc_16_tms37157,
		name => <<"CRC-16/TMS37157">>,
		width => 16,
		poly => 16#1021,
		init => 16#89EC,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#26B1,
		residue => 16#0,
		sick => false
	};
get(crc_16_usb) ->
	#{
		key => crc_16_usb,
		name => <<"CRC-16/USB">>,
		width => 16,
		poly => 16#8005,
		init => 16#FFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFF,
		check => 16#B4C8,
		residue => 16#B001,
		sick => false
	};
get(crc_16_x_25) ->
	#{
		key => crc_16_x_25,
		name => <<"CRC-16/X-25">>,
		width => 16,
		poly => 16#1021,
		init => 16#FFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFF,
		check => 16#906E,
		residue => 16#F0B8,
		sick => false
	};
get(crc_16_xmodem) ->
	#{
		key => crc_16_xmodem,
		name => <<"CRC-16/XMODEM">>,
		width => 16,
		poly => 16#1021,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#31C3,
		residue => 16#0,
		sick => false
	};
get(crc_17_can_fd) ->
	#{
		key => crc_17_can_fd,
		name => <<"CRC-17/CAN-FD">>,
		width => 17,
		poly => 16#1685B,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#4F03,
		residue => 16#0,
		sick => false
	};
get(crc_21_can_fd) ->
	#{
		key => crc_21_can_fd,
		name => <<"CRC-21/CAN-FD">>,
		width => 21,
		poly => 16#102899,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#ED841,
		residue => 16#0,
		sick => false
	};
get(crc_24) ->
	#{
		key => crc_24,
		name => <<"CRC-24">>,
		width => 24,
		poly => 16#864CFB,
		init => 16#B704CE,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#21CF02,
		residue => 16#0,
		sick => false
	};
get(crc_24_ble) ->
	#{
		key => crc_24_ble,
		name => <<"CRC-24/BLE">>,
		width => 24,
		poly => 16#65B,
		init => 16#555555,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#C25A56,
		residue => 16#0,
		sick => false
	};
get(crc_24_flexray_a) ->
	#{
		key => crc_24_flexray_a,
		name => <<"CRC-24/FLEXRAY-A">>,
		width => 24,
		poly => 16#5D6DCB,
		init => 16#FEDCBA,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#7979BD,
		residue => 16#0,
		sick => false
	};
get(crc_24_flexray_b) ->
	#{
		key => crc_24_flexray_b,
		name => <<"CRC-24/FLEXRAY-B">>,
		width => 24,
		poly => 16#5D6DCB,
		init => 16#ABCDEF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#1F23B8,
		residue => 16#0,
		sick => false
	};
get(crc_24_interlaken) ->
	#{
		key => crc_24_interlaken,
		name => <<"CRC-24/INTERLAKEN">>,
		width => 24,
		poly => 16#328B63,
		init => 16#FFFFFF,
		refin => false,
		refout => false,
		xorout => 16#FFFFFF,
		check => 16#B4F3E6,
		residue => 16#144E63,
		sick => false
	};
get(crc_24_lte_a) ->
	#{
		key => crc_24_lte_a,
		name => <<"CRC-24/LTE-A">>,
		width => 24,
		poly => 16#864CFB,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#CDE703,
		residue => 16#0,
		sick => false
	};
get(crc_24_lte_b) ->
	#{
		key => crc_24_lte_b,
		name => <<"CRC-24/LTE-B">>,
		width => 24,
		poly => 16#800063,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#23EF52,
		residue => 16#0,
		sick => false
	};
get(crc_30_cdma) ->
	#{
		key => crc_30_cdma,
		name => <<"CRC-30/CDMA">>,
		width => 30,
		poly => 16#2030B9C7,
		init => 16#3FFFFFFF,
		refin => false,
		refout => false,
		xorout => 16#3FFFFFFF,
		check => 16#4C34ABF,
		residue => 16#34EFA55A,
		sick => false
	};
get(crc_31_philips) ->
	#{
		key => crc_31_philips,
		name => <<"CRC-31/PHILIPS">>,
		width => 31,
		poly => 16#4C11DB7,
		init => 16#7FFFFFFF,
		refin => false,
		refout => false,
		xorout => 16#7FFFFFFF,
		check => 16#CE9E46C,
		residue => 16#4EAF26F1,
		sick => false
	};
get(crc_32) ->
	#{
		key => crc_32,
		name => <<"CRC-32">>,
		width => 32,
		poly => 16#4C11DB7,
		init => 16#FFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFF,
		check => 16#CBF43926,
		residue => 16#DEBB20E3,
		sick => false
	};
get(crc_32_autosar) ->
	#{
		key => crc_32_autosar,
		name => <<"CRC-32/AUTOSAR">>,
		width => 32,
		poly => 16#F4ACFB13,
		init => 16#FFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFF,
		check => 16#1697D06A,
		residue => 16#904CDDBF,
		sick => false
	};
get(crc_32_bzip2) ->
	#{
		key => crc_32_bzip2,
		name => <<"CRC-32/BZIP2">>,
		width => 32,
		poly => 16#4C11DB7,
		init => 16#FFFFFFFF,
		refin => false,
		refout => false,
		xorout => 16#FFFFFFFF,
		check => 16#FC891918,
		residue => 16#C704DD7B,
		sick => false
	};
get(crc_32_jamcrc) ->
	#{
		key => crc_32_jamcrc,
		name => <<"CRC-32/JAMCRC">>,
		width => 32,
		poly => 16#4C11DB7,
		init => 16#FFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#340BC6D9,
		residue => 16#0,
		sick => false
	};
get(crc_32_mpeg_2) ->
	#{
		key => crc_32_mpeg_2,
		name => <<"CRC-32/MPEG-2">>,
		width => 32,
		poly => 16#4C11DB7,
		init => 16#FFFFFFFF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#376E6E7,
		residue => 16#0,
		sick => false
	};
get(crc_32_posix) ->
	#{
		key => crc_32_posix,
		name => <<"CRC-32/POSIX">>,
		width => 32,
		poly => 16#4C11DB7,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FFFFFFFF,
		check => 16#765E7680,
		residue => 16#C704DD7B,
		sick => false
	};
get(crc_32_xfer) ->
	#{
		key => crc_32_xfer,
		name => <<"CRC-32/XFER">>,
		width => 32,
		poly => 16#AF,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#BD0BE338,
		residue => 16#0,
		sick => false
	};
get(crc_32c) ->
	#{
		key => crc_32c,
		name => <<"CRC-32C">>,
		width => 32,
		poly => 16#1EDC6F41,
		init => 16#FFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFF,
		check => 16#E3069283,
		residue => 16#B798B438,
		sick => false
	};
get(crc_32d) ->
	#{
		key => crc_32d,
		name => <<"CRC-32D">>,
		width => 32,
		poly => 16#A833982B,
		init => 16#FFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFF,
		check => 16#87315576,
		residue => 16#45270551,
		sick => false
	};
get(crc_32q) ->
	#{
		key => crc_32q,
		name => <<"CRC-32Q">>,
		width => 32,
		poly => 16#814141AB,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#3010BF7F,
		residue => 16#0,
		sick => false
	};
get(crc_3_gsm) ->
	#{
		key => crc_3_gsm,
		name => <<"CRC-3/GSM">>,
		width => 3,
		poly => 16#3,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#7,
		check => 16#4,
		residue => 16#2,
		sick => false
	};
get(crc_3_rohc) ->
	#{
		key => crc_3_rohc,
		name => <<"CRC-3/ROHC">>,
		width => 3,
		poly => 16#3,
		init => 16#7,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#6,
		residue => 16#0,
		sick => false
	};
get(crc_40_gsm) ->
	#{
		key => crc_40_gsm,
		name => <<"CRC-40/GSM">>,
		width => 40,
		poly => 16#4820009,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FFFFFFFFFF,
		check => 16#D4164FC646,
		residue => 16#C4FF8071FF,
		sick => false
	};
get(crc_4_interlaken) ->
	#{
		key => crc_4_interlaken,
		name => <<"CRC-4/INTERLAKEN">>,
		width => 4,
		poly => 16#3,
		init => 16#F,
		refin => false,
		refout => false,
		xorout => 16#F,
		check => 16#B,
		residue => 16#2,
		sick => false
	};
get(crc_4_itu) ->
	#{
		key => crc_4_itu,
		name => <<"CRC-4/ITU">>,
		width => 4,
		poly => 16#3,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#7,
		residue => 16#0,
		sick => false
	};
get(crc_5_epc) ->
	#{
		key => crc_5_epc,
		name => <<"CRC-5/EPC">>,
		width => 5,
		poly => 16#9,
		init => 16#9,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#0,
		residue => 16#0,
		sick => false
	};
get(crc_5_itu) ->
	#{
		key => crc_5_itu,
		name => <<"CRC-5/ITU">>,
		width => 5,
		poly => 16#15,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#7,
		residue => 16#0,
		sick => false
	};
get(crc_5_usb) ->
	#{
		key => crc_5_usb,
		name => <<"CRC-5/USB">>,
		width => 5,
		poly => 16#5,
		init => 16#1F,
		refin => true,
		refout => true,
		xorout => 16#1F,
		check => 16#19,
		residue => 16#6,
		sick => false
	};
get(crc_64) ->
	#{
		key => crc_64,
		name => <<"CRC-64">>,
		width => 64,
		poly => 16#42F0E1EBA9EA3693,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#6C40DF5F0B497347,
		residue => 16#0,
		sick => false
	};
get(crc_64_go_iso) ->
	#{
		key => crc_64_go_iso,
		name => <<"CRC-64/GO-ISO">>,
		width => 64,
		poly => 16#1B,
		init => 16#FFFFFFFFFFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFFFFFFFFFF,
		check => 16#B90956C775A41001,
		residue => 16#5300000000000000,
		sick => false
	};
get(crc_64_jones) ->
	#{
		key => crc_64_jones,
		name => <<"CRC-64/JONES">>,
		width => 64,
		poly => 16#AD93D23594C935A9,
		init => 16#FFFFFFFFFFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#CAA717168609F281,
		residue => 16#0,
		sick => false
	};
get(crc_64_we) ->
	#{
		key => crc_64_we,
		name => <<"CRC-64/WE">>,
		width => 64,
		poly => 16#42F0E1EBA9EA3693,
		init => 16#FFFFFFFFFFFFFFFF,
		refin => false,
		refout => false,
		xorout => 16#FFFFFFFFFFFFFFFF,
		check => 16#62EC59E3F1A4F00A,
		residue => 16#FCACBEBD5931A992,
		sick => false
	};
get(crc_64_xz) ->
	#{
		key => crc_64_xz,
		name => <<"CRC-64/XZ">>,
		width => 64,
		poly => 16#42F0E1EBA9EA3693,
		init => 16#FFFFFFFFFFFFFFFF,
		refin => true,
		refout => true,
		xorout => 16#FFFFFFFFFFFFFFFF,
		check => 16#995DC9BBDF1939FA,
		residue => 16#49958C9ABD7D353F,
		sick => false
	};
get(crc_6_cdma2000_a) ->
	#{
		key => crc_6_cdma2000_a,
		name => <<"CRC-6/CDMA2000-A">>,
		width => 6,
		poly => 16#27,
		init => 16#3F,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#D,
		residue => 16#0,
		sick => false
	};
get(crc_6_cdma2000_b) ->
	#{
		key => crc_6_cdma2000_b,
		name => <<"CRC-6/CDMA2000-B">>,
		width => 6,
		poly => 16#7,
		init => 16#3F,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#3B,
		residue => 16#0,
		sick => false
	};
get(crc_6_darc) ->
	#{
		key => crc_6_darc,
		name => <<"CRC-6/DARC">>,
		width => 6,
		poly => 16#19,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#26,
		residue => 16#0,
		sick => false
	};
get(crc_6_gsm) ->
	#{
		key => crc_6_gsm,
		name => <<"CRC-6/GSM">>,
		width => 6,
		poly => 16#2F,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#3F,
		check => 16#13,
		residue => 16#3A,
		sick => false
	};
get(crc_6_itu) ->
	#{
		key => crc_6_itu,
		name => <<"CRC-6/ITU">>,
		width => 6,
		poly => 16#3,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#6,
		residue => 16#0,
		sick => false
	};
get(crc_7) ->
	#{
		key => crc_7,
		name => <<"CRC-7">>,
		width => 7,
		poly => 16#9,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#75,
		residue => 16#0,
		sick => false
	};
get(crc_7_rohc) ->
	#{
		key => crc_7_rohc,
		name => <<"CRC-7/ROHC">>,
		width => 7,
		poly => 16#4F,
		init => 16#7F,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#53,
		residue => 16#0,
		sick => false
	};
get(crc_7_umts) ->
	#{
		key => crc_7_umts,
		name => <<"CRC-7/UMTS">>,
		width => 7,
		poly => 16#45,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#61,
		residue => 16#0,
		sick => false
	};
get(crc_8) ->
	#{
		key => crc_8,
		name => <<"CRC-8">>,
		width => 8,
		poly => 16#7,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#F4,
		residue => 16#0,
		sick => false
	};
get(crc_8_autosar) ->
	#{
		key => crc_8_autosar,
		name => <<"CRC-8/AUTOSAR">>,
		width => 8,
		poly => 16#2F,
		init => 16#FF,
		refin => false,
		refout => false,
		xorout => 16#FF,
		check => 16#DF,
		residue => 16#42,
		sick => false
	};
get(crc_8_bluetooth) ->
	#{
		key => crc_8_bluetooth,
		name => <<"CRC-8/BLUETOOTH">>,
		width => 8,
		poly => 16#A7,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#26,
		residue => 16#0,
		sick => false
	};
get(crc_8_cdma2000) ->
	#{
		key => crc_8_cdma2000,
		name => <<"CRC-8/CDMA2000">>,
		width => 8,
		poly => 16#9B,
		init => 16#FF,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#DA,
		residue => 16#0,
		sick => false
	};
get(crc_8_darc) ->
	#{
		key => crc_8_darc,
		name => <<"CRC-8/DARC">>,
		width => 8,
		poly => 16#39,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#15,
		residue => 16#0,
		sick => false
	};
get(crc_8_dvb_s2) ->
	#{
		key => crc_8_dvb_s2,
		name => <<"CRC-8/DVB-S2">>,
		width => 8,
		poly => 16#D5,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#BC,
		residue => 16#0,
		sick => false
	};
get(crc_8_ebu) ->
	#{
		key => crc_8_ebu,
		name => <<"CRC-8/EBU">>,
		width => 8,
		poly => 16#1D,
		init => 16#FF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#97,
		residue => 16#0,
		sick => false
	};
get(crc_8_gsm_a) ->
	#{
		key => crc_8_gsm_a,
		name => <<"CRC-8/GSM-A">>,
		width => 8,
		poly => 16#1D,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#37,
		residue => 16#0,
		sick => false
	};
get(crc_8_gsm_b) ->
	#{
		key => crc_8_gsm_b,
		name => <<"CRC-8/GSM-B">>,
		width => 8,
		poly => 16#49,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#FF,
		check => 16#94,
		residue => 16#53,
		sick => false
	};
get(crc_8_i_code) ->
	#{
		key => crc_8_i_code,
		name => <<"CRC-8/I-CODE">>,
		width => 8,
		poly => 16#1D,
		init => 16#FD,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#7E,
		residue => 16#0,
		sick => false
	};
get(crc_8_itu) ->
	#{
		key => crc_8_itu,
		name => <<"CRC-8/ITU">>,
		width => 8,
		poly => 16#7,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#55,
		check => 16#A1,
		residue => 16#AC,
		sick => false
	};
get(crc_8_koop) ->
	#{
		key => crc_8_koop,
		name => <<"CRC-8/KOOP">>,
		width => 8,
		poly => 16#4D,
		init => 16#FF,
		refin => true,
		refout => true,
		xorout => 16#FF,
		check => 16#D8,
		residue => 16#15,
		sick => false
	};
get(crc_8_lte) ->
	#{
		key => crc_8_lte,
		name => <<"CRC-8/LTE">>,
		width => 8,
		poly => 16#9B,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#EA,
		residue => 16#0,
		sick => false
	};
get(crc_8_maxim) ->
	#{
		key => crc_8_maxim,
		name => <<"CRC-8/MAXIM">>,
		width => 8,
		poly => 16#31,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#A1,
		residue => 16#0,
		sick => false
	};
get(crc_8_opensafety) ->
	#{
		key => crc_8_opensafety,
		name => <<"CRC-8/OPENSAFETY">>,
		width => 8,
		poly => 16#2F,
		init => 16#0,
		refin => false,
		refout => false,
		xorout => 16#0,
		check => 16#3E,
		residue => 16#0,
		sick => false
	};
get(crc_8_rohc) ->
	#{
		key => crc_8_rohc,
		name => <<"CRC-8/ROHC">>,
		width => 8,
		poly => 16#7,
		init => 16#FF,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#D0,
		residue => 16#0,
		sick => false
	};
get(crc_8_sae_j1850) ->
	#{
		key => crc_8_sae_j1850,
		name => <<"CRC-8/SAE-J1850">>,
		width => 8,
		poly => 16#1D,
		init => 16#FF,
		refin => false,
		refout => false,
		xorout => 16#FF,
		check => 16#4B,
		residue => 16#C4,
		sick => false
	};
get(crc_8_wcdma) ->
	#{
		key => crc_8_wcdma,
		name => <<"CRC-8/WCDMA">>,
		width => 8,
		poly => 16#9B,
		init => 16#0,
		refin => true,
		refout => true,
		xorout => 16#0,
		check => 16#25,
		residue => 16#0,
		sick => false
	};
get(x_crc_12) ->
	get(crc_12_dect);
get(crc_12_3gpp) ->
	get(crc_12_umts);
get(arc) ->
	get(crc_16);
get(crc_16_arc) ->
	get(crc_16);
get(crc_16_lha) ->
	get(crc_16);
get(crc_ibm) ->
	get(crc_16);
get(crc_a) ->
	get(crc_16_a);
get(crc_16_spi_fujitsu) ->
	get(crc_16_aug_ccitt);
get(crc_16_umts) ->
	get(crc_16_buypass);
get(crc_16_verifone) ->
	get(crc_16_buypass);
get(crc_16_ccitt_true) ->
	get(crc_16_ccitt);
get(crc_16_kermit) ->
	get(crc_16_ccitt);
get(crc_ccitt) ->
	get(crc_16_ccitt);
get(kermit) ->
	get(crc_16_ccitt);
get(crc_16_darc) ->
	get(crc_16_genibus);
get(crc_16_epc) ->
	get(crc_16_genibus);
get(crc_16_i_code) ->
	get(crc_16_genibus);
get(modbus) ->
	get(crc_16_modbus);
get(crc_16_iec_61158_2) ->
	get(crc_16_profibus);
get(sick) ->
	get(crc_16_sick);
get(crc_16_b) ->
	get(crc_16_x_25);
get(crc_16_ibm_sdlc) ->
	get(crc_16_x_25);
get(crc_16_iso_hdlc) ->
	get(crc_16_x_25);
get(crc_b) ->
	get(crc_16_x_25);
get(x_25) ->
	get(crc_16_x_25);
get(crc_16_acorn) ->
	get(crc_16_xmodem);
get(crc_16_lte) ->
	get(crc_16_xmodem);
get(xmodem) ->
	get(crc_16_xmodem);
get(zmodem) ->
	get(crc_16_xmodem);
get(crc_24_openpgp) ->
	get(crc_24);
get(crc_32_adccp) ->
	get(crc_32);
get(pkzip) ->
	get(crc_32);
get(b_crc_32) ->
	get(crc_32_bzip2);
get(crc_32_aal5) ->
	get(crc_32_bzip2);
get(crc_32_dect_b) ->
	get(crc_32_bzip2);
get(jamcrc) ->
	get(crc_32_jamcrc);
get(cksum) ->
	get(crc_32_posix);
get(xfer) ->
	get(crc_32_xfer);
get(crc_32_castagnoli) ->
	get(crc_32c);
get(crc_32_interlaken) ->
	get(crc_32c);
get(crc_32_iscsi) ->
	get(crc_32c);
get(crc_5) ->
	get(crc_5_usb);
get(crc_64_ecma_182) ->
	get(crc_64);
get(crc_64_go_ecma) ->
	get(crc_64_xz);
get(dow_crc) ->
	get(crc_8_maxim);
get(_) ->
	undefined.

%% @doc Returns every root model as `{Key, Name}'.
-spec list() -> [{key(), binary()}].
list() ->
	[
		{crc_10, <<"CRC-10">>},
		{crc_10_cdma2000, <<"CRC-10/CDMA2000">>},
		{crc_10_gsm, <<"CRC-10/GSM">>},
		{crc_11, <<"CRC-11">>},
		{crc_11_umts, <<"CRC-11/UMTS">>},
		{crc_12_cdma2000, <<"CRC-12/CDMA2000">>},
		{crc_12_dect, <<"CRC-12/DECT">>},
		{crc_12_gsm, <<"CRC-12/GSM">>},
		{crc_12_umts, <<"CRC-12/UMTS">>},
		{crc_13_bbc, <<"CRC-13/BBC">>},
		{crc_14_darc, <<"CRC-14/DARC">>},
		{crc_14_gsm, <<"CRC-14/GSM">>},
		{crc_15, <<"CRC-15">>},
		{crc_15_mpt1327, <<"CRC-15/MPT1327">>},
		{crc_16, <<"CRC-16">>},
		{crc_16_a, <<"CRC-16/A">>},
		{crc_16_aug_ccitt, <<"CRC-16/AUG-CCITT">>},
		{crc_16_buypass, <<"CRC-16/BUYPASS">>},
		{crc_16_ccitt, <<"CRC-16/CCITT">>},
		{crc_16_ccitt_false, <<"CRC-16/CCITT-FALSE">>},
		{crc_16_cdma2000, <<"CRC-16/CDMA2000">>},
		{crc_16_cms, <<"CRC-16/CMS">>},
		{crc_16_dds_110, <<"CRC-16/DDS-110">>},
		{crc_16_dect_r, <<"CRC-16/DECT-R">>},
		{crc_16_dect_x, <<"CRC-16/DECT-X">>},
		{crc_16_dnp, <<"CRC-16/DNP">>},
		{crc_16_en_13757, <<"CRC-16/EN-13757">>},
		{crc_16_genibus, <<"CRC-16/GENIBUS">>},
		{crc_16_gsm, <<"CRC-16/GSM">>},
		{crc_16_lj1200, <<"CRC-16/LJ1200">>},
		{crc_16_mcrf4xx, <<"CRC-16/MCRF4XX">>},
		{crc_16_modbus, <<"CRC-16/MODBUS">>},
		{crc_16_opensafety_a, <<"CRC-16/OPENSAFETY-A">>},
		{crc_16_opensafety_b, <<"CRC-16/OPENSAFETY-B">>},
		{crc_16_profibus, <<"CRC-16/PROFIBUS">>},
		{crc_16_riello, <<"CRC-16/RIELLO">>},
		{crc_16_sick, <<"CRC-16/SICK">>},
		{crc_16_t10_dif, <<"CRC-16/T10-DIF">>},
		{crc_16_teledisk, <<"CRC-16/TELEDISK">>},
		{crc_16_tms37157, <<"CRC-16/TMS37157">>},
		{crc_16_usb, <<"CRC-16/USB">>},
		{crc_16_x_25, <<"CRC-16/X-25">>},
		{crc_16_xmodem, <<"CRC-16/XMODEM">>},
		{crc_17_can_fd, <<"CRC-17/CAN-FD">>},
		{crc_21_can_fd, <<"CRC-21/CAN-FD">>},
		{crc_24, <<"CRC-24">>},
		{crc_24_ble, <<"CRC-24/BLE">>},
		{crc_24_flexray_a, <<"CRC-24/FLEXRAY-A">>},
		{crc_24_flexray_b, <<"CRC-24/FLEXRAY-B">>},
		{crc_24_interlaken, <<"CRC-24/INTERLAKEN">>},
		{crc_24_lte_a, <<"CRC-24/LTE-A">>},
		{crc_24_lte_b, <<"CRC-24/LTE-B">>},
		{crc_30_cdma, <<"CRC-30/CDMA">>},
		{crc_31_philips, <<"CRC-31/PHILIPS">>},
		{crc_32, <<"CRC-32">>},
		{crc_32_autosar, <<"CRC-32/AUTOSAR">>},
		{crc_32_bzip2, <<"CRC-32/BZIP2">>},
		{crc_32_jamcrc, <<"CRC-32/JAMCRC">>},
		{crc_32_mpeg_2, <<"CRC-32/MPEG-2">>},
		{crc_32_posix, <<"CRC-32/POSIX">>},
		{crc_32_xfer, <<"CRC-32/XFER">>},
		{crc_32c, <<"CRC-32C">>},
		{crc_32d, <<"CRC-32D">>},
		{crc_32q, <<"CRC-32Q">>},
		{crc_3_gsm, <<"CRC-3/GSM">>},
		{crc_3_rohc, <<"CRC-3/ROHC">>},
		{crc_40_gsm, <<"CRC-40/GSM">>},
		{crc_4_interlaken, <<"CRC-4/INTERLAKEN">>},
		{crc_4_itu, <<"CRC-4/ITU">>},
		{crc_5_epc, <<"CRC-5/EPC">>},
		{crc_5_itu, <<"CRC-5/ITU">>},
		{crc_5_usb, <<"CRC-5/USB">>},
		{crc_64, <<"CRC-64">>},
		{crc_64_go_iso, <<"CRC-64/GO-ISO">>},
		{crc_64_jones, <<"CRC-64/JONES">>},
		{crc_64_we, <<"CRC-64/WE">>},
		{crc_64_xz, <<"CRC-64/XZ">>},
		{crc_6_cdma2000_a, <<"CRC-6/CDMA2000-A">>},
		{crc_6_cdma2000_b, <<"CRC-6/CDMA2000-B">>},
		{crc_6_darc, <<"CRC-6/DARC">>},
		{crc_6_gsm, <<"CRC-6/GSM">>},
		{crc_6_itu, <<"CRC-6/ITU">>},
		{crc_7, <<"CRC-7">>},
		{crc_7_rohc, <<"CRC-7/ROHC">>},
		{crc_7_umts, <<"CRC-7/UMTS">>},
		{crc_8, <<"CRC-8">>},
		{crc_8_autosar, <<"CRC-8/AUTOSAR">>},
		{crc_8_bluetooth, <<"CRC-8/BLUETOOTH">>},
		{crc_8_cdma2000, <<"CRC-8/CDMA2000">>},
		{crc_8_darc, <<"CRC-8/DARC">>},
		{crc_8_dvb_s2, <<"CRC-8/DVB-S2">>},
		{crc_8_ebu, <<"CRC-8/EBU">>},
		{crc_8_gsm_a, <<"CRC-8/GSM-A">>},
		{crc_8_gsm_b, <<"CRC-8/GSM-B">>},
		{crc_8_i_code, <<"CRC-8/I-CODE">>},
		{crc_8_itu, <<"CRC-8/ITU">>},
		{crc_8_koop, <<"CRC-8/KOOP">>},
		{crc_8_lte, <<"CRC-8/LTE">>},
		{crc_8_maxim, <<"CRC-8/MAXIM">>},
		{crc_8_opensafety, <<"CRC-8/OPENSAFETY">>},
		{crc_8_rohc, <<"CRC-8/ROHC">>},
		{crc_8_sae_j1850, <<"CRC-8/SAE-J1850">>},
		{crc_8_wcdma, <<"CRC-8/WCDMA">>}
	].

%% @doc Returns every alias as `{Alias, Name, RootKey}'.
-spec aliases() -> [{key(), binary(), key()}].
aliases() ->
	[
		{x_crc_12, <<"X-CRC-12">>, crc_12_dect},
		{crc_12_3gpp, <<"CRC-12/3GPP">>, crc_12_umts},
		{arc, <<"ARC">>, crc_16},
		{crc_16_arc, <<"CRC-16/ARC">>, crc_16},
		{crc_16_lha, <<"CRC-16/LHA">>, crc_16},
		{crc_ibm, <<"CRC-IBM">>, crc_16},
		{crc_a, <<"CRC-A">>, crc_16_a},
		{crc_16_spi_fujitsu, <<"CRC-16/SPI-FUJITSU">>, crc_16_aug_ccitt},
		{crc_16_umts, <<"CRC-16/UMTS">>, crc_16_buypass},
		{crc_16_verifone, <<"CRC-16/VERIFONE">>, crc_16_buypass},
		{crc_16_ccitt_true, <<"CRC-16/CCITT-TRUE">>, crc_16_ccitt},
		{crc_16_kermit, <<"CRC-16/KERMIT">>, crc_16_ccitt},
		{crc_ccitt, <<"CRC-CCITT">>, crc_16_ccitt},
		{kermit, <<"KERMIT">>, crc_16_ccitt},
		{crc_16_darc, <<"CRC-16/DARC">>, crc_16_genibus},
		{crc_16_epc, <<"CRC-16/EPC">>, crc_16_genibus},
		{crc_16_i_code, <<"CRC-16/I-CODE">>, crc_16_genibus},
		{modbus, <<"MODBUS">>, crc_16_modbus},
		{crc_16_iec_61158_2, <<"CRC-16/IEC-61158-2">>, crc_16_profibus},
		{sick, <<"SICK">>, crc_16_sick},
		{crc_16_b, <<"CRC-16/B">>, crc_16_x_25},
		{crc_16_ibm_sdlc, <<"CRC-16/IBM-SDLC">>, crc_16_x_25},
		{crc_16_iso_hdlc, <<"CRC-16/ISO-HDLC">>, crc_16_x_25},
		{crc_b, <<"CRC-B">>, crc_16_x_25},
		{x_25, <<"X-25">>, crc_16_x_25},
		{crc_16_acorn, <<"CRC-16/ACORN">>, crc_16_xmodem},
		{crc_16_lte, <<"CRC-16/LTE">>, crc_16_xmodem},
		{xmodem, <<"XMODEM">>, crc_16_xmodem},
		{zmodem, <<"ZMODEM">>, crc_16_xmodem},
		{crc_24_openpgp, <<"CRC-24/OPENPGP">>, crc_24},
		{crc_32_adccp, <<"CRC-32/ADCCP">>, crc_32},
		{pkzip, <<"PKZIP">>, crc_32},
		{b_crc_32, <<"B-CRC-32">>, crc_32_bzip2},
		{crc_32_aal5, <<"CRC-32/AAL5">>, crc_32_bzip2},
		{crc_32_dect_b, <<"CRC-32/DECT-B">>, crc_32_bzip2},
		{jamcrc, <<"JAMCRC">>, crc_32_jamcrc},
		{cksum, <<"CKSUM">>, crc_32_posix},
		{xfer, <<"XFER">>, crc_32_xfer},
		{crc_32_castagnoli, <<"CRC-32/CASTAGNOLI">>, crc_32c},
		{crc_32_interlaken, <<"CRC-32/INTERLAKEN">>, crc_32c},
		{crc_32_iscsi, <<"CRC-32/ISCSI">>, crc_32c},
		{crc_5, <<"CRC-5">>, crc_5_usb},
		{crc_64_ecma_182, <<"CRC-64/ECMA-182">>, crc_64},
		{crc_64_go_ecma, <<"CRC-64/GO-ECMA">>, crc_64_xz},
		{dow_crc, <<"DOW-CRC">>, crc_8_maxim}
	].
