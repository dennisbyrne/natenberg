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

-module(position).
-export([merge/3, long_count/1, short_count/1, call_count/1, put_count/1, 
		 expiration_count/1, strike_count/1, strikes/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(To, From, Description) ->
	#position{description = Description,
			  long = merge(To#position.long, From#position.long), 
			  short = merge(To#position.short, From#position.short)}.

merge(To, From) ->
	#side{calls = To#side.calls ++ From#side.calls,
		  puts = To#side.puts ++ From#side.puts,
		  underlyings = To#side.underlyings ++ From#side.underlyings}.

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

is_ratioed(Position) ->
	position:call_count(Position) =/= position:put_count(Position).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
