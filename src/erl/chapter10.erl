-module(chapter10).
-export([is_vertical_spread/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_vertical_spread(Position) -> %page 202
	chapter8:long_count(Position) =:= chapter8:short_count(Position) andalso
		chapter8:expiration_count(Position) =:= 1 andalso
			chapter8:strike_count(Position) =:= 2 andalso
				((chapter8:call_count(Position) =:= 0) xor (chapter8:put_count(Position) =:= 0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_vertical_spread_test() ->
	?assertEqual(true, is_vertical_spread(?CALL_BULL_SPREAD)),
	?assertEqual(true, is_vertical_spread(?PUT_BEAR_SPREAD)),
	?assertEqual(false, is_vertical_spread(?LONG_BUTTERFLY)),
	?assertEqual(false, is_vertical_spread(?SHORT_STRANGLE)),
	?assertEqual(false, is_vertical_spread(?LONG_STRADDLE)),
	?assertEqual(false, is_vertical_spread(?PUT_BACKSPREAD)),
	?assertEqual(false, is_vertical_spread(?CALL_RATIO_VERTICAL_SPREAD)).
