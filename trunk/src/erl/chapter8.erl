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
	long_count(Position) > short_count(Position).

is_ratio_vertical_spread(Position) ->
	% ... with all options expiring @ the same time ?
	long_count(Position) < short_count(Position).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_backspread_test() ->
	?assertEqual(true, is_backspread(?PUT_BACKSPREAD)).

is_ratio_vertical_spread_test() ->
	?assertEqual(true, is_ratio_vertical_spread(?CALL_RATIO_VERTICAL_SPREAD)).

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
	?assertEqual(true, is_ratioed(?CALL_RATIO_VERTICAL_SPREAD)).



