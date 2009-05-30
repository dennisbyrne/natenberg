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
-include_lib("eunit/include/eunit.hrl").
-include_lib("position.hrl").

-define(DELTA, [62, 46, 53, 56, 74, 45, 35, 50, 93]).
-define(PXS, [102.26, 99.07, 100.39, 100.76, 103.59, 99.26, 98.28, 99.98, 103.78]).
-define(DELTA_BY_PRICE, dict:from_list(lists:zip([101.35] ++ ?PXS, [57] ++ ?DELTA))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust(Px, Position) when is_float(Px) ->
	Delta = delta(Px, Position),
	adjust(Delta div 100, Px, Position);
adjust([], Position) ->
	Position;
adjust([Px|T], Position) ->
	adjust(T, adjust(Px, Position)).

adjust(Quantity, Px, #position{long = Long, short = Short}) when Quantity > 0 ->
	#side{underlyings = Underlyings} = Short,
	NewUnderlyings = Underlyings ++ lists:duplicate(Quantity, #underlying{px = Px}),
	#position{long = Long, short = Short#side{underlyings = NewUnderlyings}};
adjust(Quantity, _, Position) when Quantity == 0 ->
	Position;
adjust(Quantity, Px, #position{long = Long, short = Short}) when Quantity < 0 ->
	#side{underlyings = Underlyings} = Long,
	NewUnderlyings = Underlyings ++ lists:duplicate(-Quantity, #underlying{px = Px}),
	#position{long = Long#side{underlyings = NewUnderlyings}, short = Short}.

delta(Px, #position{long = Long, short = Short}) ->
	#side{underlyings = LongUnderlyings, calls = LongCalls, puts = _} = Long,
	#side{underlyings = ShortUnderlyings, calls = _, puts = _} = Short,
	Delta = dict:fetch(Px, ?DELTA_BY_PRICE),
	OptionDeltas = lists:duplicate(length(LongCalls), Delta),
	OptionDelta = lists:foldl(fun common:sum/2, 0, OptionDeltas),
	UnderlyingDelta = 100 * (length(LongUnderlyings) - length(ShortUnderlyings)),
	OptionDelta + UnderlyingDelta.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_buy_test() ->
	Underlying = #underlying{px = 5.0},
	Expected = #position{long = #side{underlyings = [Underlying]}},
	?assertMatch(Expected, adjust(-1, 5.0, #position{})).

adjust_sell_test() ->
	Call = #option{},
	Underlying = #underlying{px = 5.0},
	Short = #side{underlyings = [Underlying], calls = [Call]},
	Expected = #position{short = #side{underlyings = [Underlying, Underlying], calls = [Call]}},
	?assertMatch(Expected, adjust(1, 5.0, #position{short = Short})).

delta_page_82_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Position = #position{long = Long},
	?assertMatch(5700, delta(101.35, Position)),
	?assertMatch(6200, delta(102.26, Position)),
  	?assertMatch(4600, delta(99.07, Position)),
	?assertMatch(5300, delta(100.39, Position)).

delta_page_83_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Short = #side{underlyings = lists:duplicate(57, #underlying{})},
	Position = #position{long = Long, short = Short},
	?assertMatch(0, delta(101.35, Position)).

delta_negative_test() ->
	Long = #side{calls = lists:duplicate(100, #option{})},
	Short = #side{underlyings = lists:duplicate(58, #underlying{})},
	Position = #position{long = Long, short = Short},
	?assertMatch(-100, delta(101.35, Position)).

delta_options_and_underlyings_test() ->
	Calls = lists:duplicate(100, #option{}),
	ShortUnderlyings = [ #underlying{px = 101.35} || _ <- lists:seq(1, 57) ] ++
					   [ #underlying{px = 102.26} || _ <- lists:seq(1, 5) ],
	LongUnderlyings = [ #underlying{px = 99.07} || _ <- lists:seq(1, 16) ],
	Long = #side{calls = Calls, underlyings = LongUnderlyings},
	Short = #side{underlyings = ShortUnderlyings},
	Position = #position{long = Long, short = Short},
	?assertMatch(700, delta(100.39, Position)).

delta_no_options_test() ->
	Underlying = #underlying{px = 101.35},
	Long = #side{underlyings = [Underlying]},
	Short = #side{underlyings = [Underlying]},
	Position = #position{long = Long, short = Short},
	?assertMatch(0, delta(101.35, Position)).

adjust_page_83_test() ->
	Calls = lists:duplicate(100, #option{}),
	Long = #side{calls = Calls},
	ExpectedUnderlyings = [ #underlying{px = 101.35} || _ <- lists:seq(1, 57) ] ++
						  [ #underlying{px = 102.26} || _ <- lists:seq(1, 5) ],
	ExpectedShort = #side{underlyings = ExpectedUnderlyings},
	ExpectedPosition = #position{long = Long, short = ExpectedShort},
	?assertMatch(ExpectedPosition, adjust([101.35, 102.26], #position{long = Long})).

adjust_page_85_test() ->
	Calls = lists:duplicate(100, #option{px = 3.25, strike = 100.0}),
	Underlyings = lists:duplicate(57, #underlying{px = 101.35}),
	NeutralPosition = #position{long = #side{calls = Calls},
								short = #side{underlyings = Underlyings}},
	Position = adjust(?PXS, NeutralPosition),
	#position{long = Long, short = Short} = Position,
	#side{underlyings = ShortUnderlyings} = Short,
	#side{underlyings = LongUnderlyings} = Long,
	?assertMatch(Calls, Long#side.calls),
	Net = length(Underlyings) + 36,
	?assertMatch(Net, length(ShortUnderlyings) - length(LongUnderlyings)),
	PreClosePosition = adjust(-36, 102.54, Position),
	OriginalHedgeAndAdjustmentsPnl = -138.83 + 205.27,
	PreClosePnl = chapter1:pnl(102.54, PreClosePosition),
	?assertMatch(true, PreClosePnl - OriginalHedgeAndAdjustmentsPnl < 0.001).
