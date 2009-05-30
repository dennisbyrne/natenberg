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

-define(DELTA, [57, 62, 46, 53, 56, 74, 45, 35, 50, 93]).
-define(PXS, [101.35, 102.26, 99.07, 100.39, 100.76, 103.59, 99.26, 98.28, 99.98, 103.78]).
-define(DELTA_BY_PRICE, dict:from_list(lists:zip(?PXS, ?DELTA))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust(Px, Position) when is_float(Px) ->
	Delta = delta(Px, Position),
	sell_underlying(Delta div 100, Px, Position);
adjust([], Position) ->
	Position;
adjust([Px|T], Position) ->
	adjust(T, adjust(Px, Position)).

delta(Px, #position{long = Long, short = Short}) ->
	#side{underlyings = _, calls = LongCalls, puts = _} = Long,
	#side{underlyings = ShortUnderlyings, calls = _, puts = _} = Short,
	OptionDeltas = [ dict:fetch(Px, ?DELTA_BY_PRICE) || _ <- LongCalls ],
	OptionDelta = lists:foldl(fun common:sum/2, 0, OptionDeltas),
	UnderlyingDelta = 100 * length(ShortUnderlyings),
	OptionDelta - UnderlyingDelta.

sell_underlying(Quantity, Px, #position{long = Long, short = Short}) ->
	#side{underlyings = Underlyings} = Short,
	NewUnderlyings = Underlyings ++ [ #underlying{px = Px} || _ <- lists:seq(1, Quantity) ],
	#position{long = Long, short = Short#side{underlyings = NewUnderlyings}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sell_underlying_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	Underlying = #underlying{px = 5.0},
	Short = #side{underlyings = [Underlying], calls = [Call]},
	Expected = #position{short = #side{underlyings = [Underlying, Underlying], calls = [Call]}},
	?assertMatch(Expected, sell_underlying(1, 5.0, #position{short = Short})).

delta_page_82_test() ->
	Call = #option{px = 3.88, strike = 100.0},
	Long = #side{calls = [ Call || _ <- lists:seq(1, 100) ]},
	Position = #position{long = Long},
	?assertMatch(5700, delta(101.35, Position)).

delta_page_83_test() ->
	Call = #option{px = 3.88, strike = 100.0},
	Underlying = #underlying{px = 101.35},
	Long = #side{calls = [ Call || _ <- lists:seq(1, 100) ]},
	Short = #side{underlyings = [ Underlying || _ <- lists:seq(1, 57) ]},
	Position = #position{long = Long, short = Short},
	?assertMatch(0, delta(101.35, Position)).

adjust_page_83_test() ->
	Call = #option{px = 3.88, strike = 100.0},
	Calls = [ Call || _ <- lists:seq(1, 100) ],
	Long = #side{calls = Calls},
	ExpectedUnderlyings = [ #underlying{px = 101.35} || _ <- lists:seq(1, 57) ] ++
						  [ #underlying{px = 102.26} || _ <- lists:seq(1, 5) ],
	ExpectedShort = #side{underlyings = ExpectedUnderlyings},
	ExpectedPosition = #position{long = Long, short = ExpectedShort},
	?assertMatch(ExpectedPosition, adjust([101.35, 102.26], #position{long = Long})).

