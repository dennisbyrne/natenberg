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

-module(chapter5).
-export([delta/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("position.hrl").

-define(FUTURES_DELTA, [62, 46, 53, 56, 74, 45, 35, 50, 93]).
-define(FUTURES_PXS, [102.26, 99.07, 100.39, 100.76, 103.59, 99.26, 98.28, 99.98, 103.78]).
-define(FUTURES_DELTA_BY_PX, dict:from_list(lists:zip([101.35] ++ ?FUTURES_PXS, [57] ++ ?FUTURES_DELTA))).

-define(EQY_DELTA, [52, 66, 64, 52, 28, 38, 73, 78, 55]).
-define(EQY_PXS, [ 100 * X || X <- [49.625, 52.125, 51.75, 50.0, 47.0, 48.125, 52.0, 52.25, 50.125]]).
-define(EQY_DELTA_BY_PX, dict:from_list(lists:zip(?EQY_PXS, ?EQY_DELTA))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust([], Position) ->
	Position;
adjust([Px|T], Position) ->
	adjust(T, adjust(Px, Position));
adjust(Px, Position) ->
	Delta = delta(Px, Position),
	adjust(Delta div 100, Px, Position).

adjust(0, _, Position) ->
	Position;
adjust(Quantity, Px, Position = #position{short = Short}) when Quantity > 0 ->
	Underlyings = Short#side.underlyings,
	NewUnderlyings = Underlyings ++ lists:duplicate(Quantity, #underlying{px = Px}),
	Position#position{short = Short#side{underlyings = NewUnderlyings}};
adjust(Quantity, Px, Position = #position{long = Long}) when Quantity < 0 ->
	Underlyings = Long#side.underlyings,
	NewUnderlyings = Underlyings ++ lists:duplicate(-Quantity, #underlying{px = Px}),
	Position#position{long = Long#side{underlyings = NewUnderlyings}}.

delta(Px, #position{long = Long, short = Short, deltas = Deltas}) ->
	#side{underlyings = LongUnderlyings, calls = LongCalls, puts = LongPuts} = Long,
	#side{underlyings = ShortUnderlyings, calls = ShortCalls, puts = ShortPuts} = Short,
	Delta = dict:fetch(Px, Deltas),
	OptionDelta = Delta * (length(LongCalls ++ ShortPuts) - length(ShortCalls ++ LongPuts)),
	UnderlyingDelta = 100 * (length(LongUnderlyings) - length(ShortUnderlyings)),
	OptionDelta + UnderlyingDelta.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_buy_test() ->
	Underlying = #underlying{px = 5.0},
	Expected = #position{long = #side{underlyings = [Underlying]}},
	?assertEqual(Expected, adjust(-1, 5.0, #position{})).

adjust_sell_test() ->
	Long = #side{calls = [#option{}]},
	Underlying = #underlying{px = 5.0},
	Short = #side{underlyings = [Underlying]},
	Position = #position{long = Long, short = Short},
	Expected = Position#position{short = Short#side{underlyings = [Underlying, Underlying]}},
	?assertEqual(Expected, adjust(1, 5.0, Position)).

delta_page_82_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Position = #position{long = Long, deltas = ?FUTURES_DELTA_BY_PX},
	?assertEqual(5700, delta(101.35, Position)),
	?assertEqual(6200, delta(102.26, Position)),
  	?assertEqual(4600, delta(99.07, Position)),
	?assertEqual(5300, delta(100.39, Position)).

delta_page_83_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Short = #side{underlyings = lists:duplicate(57, #underlying{})},
	Position = #position{long = Long, short = Short, deltas = ?FUTURES_DELTA_BY_PX},
	?assertEqual(0, delta(101.35, Position)).

delta_negative_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Short = #side{underlyings = lists:duplicate(58, #underlying{})},
	Position = #position{long = Long, short = Short, deltas = ?FUTURES_DELTA_BY_PX},
	?assertEqual(-100, delta(101.35, Position)).

delta_options_and_underlyings_test() ->
	Calls = lists:duplicate(100, #option{}),
	ShortUnderlyings = lists:duplicate(57, #underlying{px = 101.35}) ++
					   lists:duplicate(5, #underlying{px = 102.26}),
	LongUnderlyings = lists:duplicate(16, #underlying{px = 99.07}),
	Long = #side{calls = Calls, underlyings = LongUnderlyings},
	Short = #side{underlyings = ShortUnderlyings},
	Position = #position{long = Long, 
						 short = Short, 
						 deltas = ?FUTURES_DELTA_BY_PX},
	?assertEqual(700, delta(100.39, Position)).

delta_no_options_test() ->
	Underlying = #underlying{px = 101.35},
	Side = #side{underlyings = [Underlying]},
	Position = #position{long = Side, short = Side, 
						 deltas = ?FUTURES_DELTA_BY_PX},
	?assertEqual(0, delta(101.35, Position)).

delta_put_test() ->
	Position = #position{long = #side{ puts = [#option{}]}, 
						 deltas = ?FUTURES_DELTA_BY_PX },
	?assertEqual(-53, delta(100.39, Position)).

adjust_page_83_test() ->
	Calls = lists:duplicate(100, #option{}),
	Long = #side{calls = Calls},
	Position = #position{long = Long, deltas = ?FUTURES_DELTA_BY_PX},
	ExpectedUnderlyings = lists:duplicate(57, #underlying{px = 101.35}) ++
						  lists:duplicate(5, #underlying{px = 102.26}),
	ExpectedShort = #side{underlyings = ExpectedUnderlyings},
	ExpectedPosition = Position#position{short = ExpectedShort},
	?assertEqual(ExpectedPosition, adjust([101.35, 102.26], Position)).

adjust_page_85_test() ->
	Calls = lists:duplicate(100, #option{px = 3.25, strike = 100.0}),
	Underlyings = lists:duplicate(57, #underlying{px = 101.35}),
	NeutralPosition = #position{long = #side{calls = Calls},
								short = #side{underlyings = Underlyings},
								deltas = ?FUTURES_DELTA_BY_PX},
	Position = adjust(?FUTURES_PXS, NeutralPosition),
	ShortUnderlyings = (Position#position.short)#side.underlyings,
	LongUnderlyings = (Position#position.long)#side.underlyings,
	PreClosePosition = adjust(-36, 102.54, Position),
	PreClosePnl = chapter1:pnl(102.54, PreClosePosition),
	?assertEqual(Calls, (Position#position.long)#side.calls),
	?assertEqual(length(Underlyings) + 36, length(ShortUnderlyings) - length(LongUnderlyings)),
	?assert((-138.83 + 205.27) - PreClosePnl < 0.001).

adjust_page_91_test() ->
	Calls = lists:duplicate(100, #option{px = 3.0, strike = 50.0}),
	Stocks = lists:duplicate(46, #underlying{px = 4850.0}),
	NeutralPosition = #position{short = #side{calls = Calls},
								long = #side{underlyings = Stocks},
								deltas = dict:store(4850.0, 46.0, ?EQY_DELTA_BY_PX)},
	Position = adjust(?EQY_PXS, NeutralPosition),
	ShortUnderlyings = (Position#position.short)#side.underlyings,
	LongUnderlyings = (Position#position.long)#side.underlyings,
	?assertEqual(Calls, (Position#position.short)#side.calls),
	?assertEqual(length(Stocks) + 9, length(LongUnderlyings) - length(ShortUnderlyings)),
	?assertEqual(0.0, delta(4850.0, NeutralPosition)).
