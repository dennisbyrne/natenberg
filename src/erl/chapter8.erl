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
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("position.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_backspread(Position) -> % page 138
	expiration_count(Position) =:= 1 andalso 
		long_count(Position) > short_count(Position).

is_ratio_vertical_spread(Position) -> % page 139
	expiration_count(Position) =:= 1 andalso 
		long_count(Position) < short_count(Position).

is_straddle(Position) -> % page 140
	strike_count(Position) =:= 1 andalso
		call_count(Position) > 0 andalso
			put_count(Position) > 0 andalso
				(is_backspread(Position) xor is_ratio_vertical_spread(Position)).

is_strangle(Position) -> % page 143
	strike_count(Position) > 1 andalso
		call_count(Position) > 0 andalso
			put_count(Position) > 0 andalso
				(is_backspread(Position) xor is_ratio_vertical_spread(Position)).

is_butterfly(Position) ->
	Strikes = strikes(Position),
	((call_count(Position) =:= 0) xor (put_count(Position) =:= 0)) andalso
		long_count(Position) =:= short_count(Position) andalso
  			3 =:= length(Strikes) andalso
				(lists:nth(2, Strikes) - hd(Strikes) =:= lists:last(Strikes) - lists:nth(2, Strikes)).

is_ratioed(Position) ->
	call_count(Position) =/= put_count(Position).

call_count(#position{long = Long, short = Short}) ->
	#side{calls = LongCalls} = Long,
	#side{calls = ShortCalls} = Short,
	length(LongCalls ++ ShortCalls).
	
put_count(#position{long = Long, short = Short}) ->
	#side{puts = LongPuts} = Long,
	#side{puts = ShortPuts} = Short,
	length(LongPuts ++ ShortPuts).

short_count(#position{short = Short}) ->
	#side{calls = ShortCalls, puts = ShortPuts} = Short,
	length(ShortCalls ++ ShortPuts).

long_count(#position{long = Long}) ->
	#side{calls = LongCalls, puts = LongPuts} = Long,
	length(LongCalls ++ LongPuts).

expiration_count(Position) ->
	Expirations = [ Option#option.expiration || Option <- options(Position) ],
	length(lists:usort(Expirations)).

strikes(Position) ->
	lists:usort([ Option#option.strike || Option <- options(Position) ]).

strike_count(Position) ->
	length(strikes(Position)).

options(#position{long = Long, short = Short}) ->
	Long#side.calls ++ Long#side.puts ++ Short#side.calls ++ Short#side.puts.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_strangle_test() ->
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

expirations_test() ->
	?assertEqual(0, expiration_count(#position{})),
	?assertEqual(1, expiration_count(?CALL_BACKSPREAD)).

call_count_test() ->
	?assertEqual(1, call_count(?LONG_CALL)),
	?assertEqual(1, call_count(?SHORT_CALL)),
	?assertEqual(0, call_count(?SHORT_PUT)).

put_count_test() ->
	?assertEqual(0, put_count(?LONG_CALL)),
	?assertEqual(1, put_count(?LONG_PUT)),
	?assertEqual(1, put_count(?SHORT_PUT)).

is_ratioed_test() ->
	?assertEqual(true, is_ratioed(?LONG_CALL)),
	?assertEqual(false, is_ratioed(?SHORT_STRANGLE)),
	?assertEqual(true, is_ratioed(?CALL_RATIO_VERTICAL_SPREAD)),
	?assertEqual(true, is_ratioed(?LONG_BUTTERFLY)),
	?assertEqual(true, is_ratioed(?SHORT_BUTTERFLY)).



