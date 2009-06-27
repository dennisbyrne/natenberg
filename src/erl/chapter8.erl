% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chapter8).
-export([is_backspread/1, is_ratio_vertical_spread/1, is_straddle/1, is_strangle/1, is_butterfly/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_backspread(Position) -> % page 138
	LongCount = position:long_count(Position),
	ShortCount = position:short_count(Position),	
	position:expiration_count(Position) =:= 1 andalso 
		LongCount > ShortCount andalso
			LongCount + ShortCount > 1.

is_ratio_vertical_spread(Position) -> % page 139
	LongCount = position:long_count(Position),
	ShortCount = position:short_count(Position),
	position:expiration_count(Position) =:= 1 andalso
		LongCount < ShortCount andalso
			LongCount + ShortCount > 1.

is_straddle(Position) -> % page 140
	position:strike_count(Position) =:= 1 andalso
		position:call_count(Position) > 0 andalso
			position:put_count(Position) > 0 andalso
				(is_backspread(Position) xor is_ratio_vertical_spread(Position)).

is_strangle(Position) -> % page 143
	position:strike_count(Position) > 1 andalso
		position:call_count(Position) > 0 andalso
			position:put_count(Position) > 0 andalso
				(is_backspread(Position) xor is_ratio_vertical_spread(Position)).

is_butterfly(Position) ->
	Strikes = position:strikes(Position),
	((position:call_count(Position) =:= 0) xor (position:put_count(Position) =:= 0)) andalso
		position:long_count(Position) =:= position:short_count(Position) andalso
  			3 =:= length(Strikes) andalso
				(lists:nth(2, Strikes) - hd(Strikes) =:= lists:last(Strikes) - lists:nth(2, Strikes)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_strangle_test() ->
	?assertEqual(false, is_strangle(?LONG_UNDERLYING)),
	?assertEqual(false, is_strangle(?LONG_BUTTERFLY)),
	?assertEqual(false, is_strangle(?SHORT_BUTTERFLY)),	
	?assertEqual(true, is_strangle(?SHORT_STRANGLE)),
	?assertEqual(true, is_strangle(?LONG_STRANGLE)),
	?assertEqual(false, is_strangle(?LONG_STRADDLE)),
	?assertEqual(false, is_strangle(?SHORT_STRADDLE)),
	?assertEqual(false, is_strangle(?PUT_BACKSPREAD)),
	?assertEqual(false, is_strangle(?CALL_BACKSPREAD)),
	?assertEqual(false, is_strangle(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(false, is_strangle(?PUT_RATIO_VERTICAL_SPREAD)).

is_straddle_test() ->
	?assertEqual(false, is_straddle(?LONG_BUTTERFLY)),
	?assertEqual(false, is_straddle(?SHORT_BUTTERFLY)),	
	?assertEqual(false, is_straddle(?SHORT_STRANGLE)),
	?assertEqual(false, is_straddle(?LONG_STRANGLE)),	
	?assertEqual(true, is_straddle(?LONG_STRADDLE)),
	?assertEqual(true, is_straddle(?SHORT_STRADDLE)),
	?assertEqual(false, is_straddle(?PUT_BACKSPREAD)),
	?assertEqual(false, is_straddle(?CALL_BACKSPREAD)),
	?assertEqual(false, is_straddle(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(false, is_straddle(?PUT_RATIO_VERTICAL_SPREAD)).

is_backspread_test() ->
	?assertEqual(false, is_backspread(?LONG_BUTTERFLY)),
	?assertEqual(false, is_backspread(?SHORT_BUTTERFLY)),	
	?assertEqual(false, is_backspread(?SHORT_STRANGLE)),
	?assertEqual(true, is_backspread(?LONG_STRANGLE)),	
	?assertEqual(true, is_backspread(?LONG_STRADDLE)),
	?assertEqual(false, is_backspread(?SHORT_STRADDLE)),	
	?assertEqual(true, is_backspread(?PUT_BACKSPREAD)),
	?assertEqual(true, is_backspread(?CALL_BACKSPREAD)),
	?assertEqual(false, is_backspread(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(false, is_backspread(?PUT_RATIO_VERTICAL_SPREAD)).

is_ratio_vertical_spread_test() ->
	?assertEqual(false, is_ratio_vertical_spread(?LONG_BUTTERFLY)),
	?assertEqual(false, is_ratio_vertical_spread(?SHORT_BUTTERFLY)),	
	?assertEqual(true, is_ratio_vertical_spread(?SHORT_STRANGLE)),
	?assertEqual(false, is_ratio_vertical_spread(?LONG_STRANGLE)),	
	?assertEqual(false, is_ratio_vertical_spread(?LONG_STRADDLE)),
	?assertEqual(true, is_ratio_vertical_spread(?SHORT_STRADDLE)),
	?assertEqual(false, is_ratio_vertical_spread(?PUT_BACKSPREAD)),
	?assertEqual(false, is_ratio_vertical_spread(?CALL_BACKSPREAD)),	
	?assertEqual(true, is_ratio_vertical_spread(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(true, is_ratio_vertical_spread(?PUT_RATIO_VERTICAL_SPREAD)).

is_butterfly_test() ->
	?assertEqual(true, is_butterfly(?LONG_BUTTERFLY)),
	?assertEqual(true, is_butterfly(?SHORT_BUTTERFLY)),
	?assertEqual(false, is_butterfly(?SHORT_STRANGLE)),
	?assertEqual(false, is_butterfly(?LONG_STRANGLE)),	
	?assertEqual(false, is_butterfly(?LONG_STRADDLE)),
	?assertEqual(false, is_butterfly(?SHORT_STRADDLE)),
	?assertEqual(false, is_butterfly(?PUT_BACKSPREAD)),
	?assertEqual(false, is_butterfly(?CALL_BACKSPREAD)),	
	?assertEqual(false, is_butterfly(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(false, is_butterfly(?PUT_RATIO_VERTICAL_SPREAD)).
