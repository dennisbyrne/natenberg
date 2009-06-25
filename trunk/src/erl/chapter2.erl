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

-module(chapter2).
-export([draw/1, risk/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Positions) when is_list(Positions) ->
	Descs = [ P#position.description || P <- Positions ],
	Description = description(Positions, Descs),
	Points = lists:map(fun pxByPnl/1, Positions),
	Stretched = stretch(Points, Positions),
	view:draw(Stretched, Description);
draw(Position) ->
	draw([Position]).

pxByPnl(Position) ->
	Pxs = pxs(Position),
	Stretched = stretch(Pxs, Position),
	Pnls = [ chapter1:pnl(Px, Position) || Px <- Stretched ],
	lists:zip(Stretched, Pnls).

pxs(#position{long = Long, short = Short}) ->
	LongPxs = pxs(Long),
	ShortPxs = pxs(Short),
	lists:usort(LongPxs ++ ShortPxs);
pxs(#side{underlyings = Underlyings, calls = Calls, puts = Puts}) ->
	[Underlying#underlying.px || Underlying <- Underlyings] ++ 
	[Option#option.strike || Option <- Calls ++ Puts].

stretch(Points, Positions) when is_list(Positions) ->
	PointsByPositions = lists:zip(Points, Positions),
	{Pxs, _} = lists:unzip(lists:flatten(Points)),
	MinPx = lists:min(lists:flatten(Pxs)),
	MaxPx = lists:max(lists:flatten(Pxs)),
	[ stretch(MinPx, MaxPx, Pts, Position) || {Pts, Position} <- PointsByPositions ];
stretch(Pxs, Position) ->
	{DownsideRisk, UpsideRisk} = risk(Position),
	Low = hd(Pxs),
	LowPx = Low - break_even(Low, DownsideRisk, Position),
	High = lists:last(Pxs),
	HighPx = High + break_even(High, UpsideRisk, Position),
	[common:floor(LowPx)] ++ Pxs ++ [common:ceiling(HighPx)].

stretch(MinPx, MaxPx, Points, Position) ->
	{LeftPx, _} = hd(Points),
	{RightPx, _} = lists:last(Points),
	Left = common:min(LeftPx, MinPx),
	Right = common:max(RightPx, MaxPx),
	LeftPoint = {Left, chapter1:pnl(Left, Position)},
	RightPoint = {Right, chapter1:pnl(Right, Position)},
	lists:usort([LeftPoint] ++ Points ++ [RightPoint]).

break_even(Px, Risk, Position) ->
	Pnl = chapter1:pnl(Px, Position),
	% stretch the x axis by 2 if pnl does not approach 0
	if Risk * Pnl < 0 -> Pnl / -Risk;
	   true -> 2
	end.

risk(#position{long = Long, short = Short}) ->
	#side{underlyings = LongUnderlyings, calls = LongCalls, puts = LongPuts} = Long,
	#side{underlyings = ShortUnderlyings, calls = ShortCalls, puts = ShortPuts} = Short,
	{length(LongPuts) - length(ShortPuts) + length(ShortUnderlyings) - length(LongUnderlyings), 
	 length(LongCalls) - length(ShortCalls) - length(ShortUnderlyings) + length(LongUnderlyings)}.

description([_], [Description]) ->
	Description;
description(Positions, Descriptions) ->
	Keys = [fun chapter8:is_strangle/1,
			fun chapter8:is_straddle/1,
		    fun chapter8:is_butterfly/1,
			fun chapter8:is_ratio_vertical_spread/1,
			fun chapter8:is_backspread/1,
			fun chapter10:is_vertical_spread/1,
			fun(_) -> true end],
	Values = ["Strangles",
			  "Straddles",
			  "Butterflies",
			  "Ratio Vertical Spreads",
			  "Backspreads",
			  "Vertical Spreads",
			  common:join(Descriptions, ", ")],
	Dict = dict:from_list(lists:zip(Keys, Values)),
	description(Positions, Keys, Dict).

description(Positions, [Predicate|T], Dict) ->
	All = lists:all(Predicate, Positions),
	if All -> dict:fetch(Predicate, Dict);
	   true -> description(Positions, T, Dict)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

description_test() ->
	?assertEqual("Vertical Spreads", description([?CALL_BULL_SPREAD, ?PUT_BEAR_SPREAD], ["a", "b"])),
	?assertEqual("a, b", description([?CALL_BULL_SPREAD, ?LONG_CALL], ["a", "b"])),
	?assertEqual("a", description([?LONG_CALL], ["a"])).

risk_underlying_test() ->
	Underlying = #underlying{px = 99.0},
	?assertMatch({-1, 1}, risk(#position{ long = #side{underlyings = [Underlying]} })),
	?assertMatch({1, -1}, risk(#position{ short = #side{underlyings = [Underlying]} })),
	?assertMatch({2, -2}, risk(#position{ short = #side{underlyings = [Underlying, Underlying]} })).

risk_calls_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	?assertMatch({0, 1}, risk(#position{ long = #side{calls = [Call]} })),
	?assertMatch({0, -1}, risk(#position{ short = #side{calls = [Call]} })),
	?assertMatch({0, -2}, risk(#position{ short = #side{calls = [Call, Call]} })).

risk_puts_test() ->
	Put = #option{px = 1.0, strike = 100.0},
	?assertMatch({1, 0}, risk(#position{ long = #side{puts = [Put]} })),
	?assertMatch({-1, 0}, risk(#position{ short = #side{puts = [Put]} })),
	?assertMatch({-2, 0}, risk(#position{ short = #side{puts = [Put, Put]} })).

risk_spread_test() ->
	LongCall = #option{px = 9.35, strike = 90.0},
	ShortCall = #option{px = 2.7, strike = 100.0},
	Long = #side{calls = [LongCall]},
	Short = #side{calls = [ShortCall]},
	Position = #position{long = Long, short = Short},	
	?assertMatch({0, 0}, risk(Position)).

-define(TO_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

risk_reversal_test() ->
	Combined = chapter11:reversal(?SHORT_UNDERLYING, ?TO_OPTION),
	Synthetic = chapter11:synthetic_short(?SHORT_UNDERLYING, ?TO_OPTION),
	{0, 0} = risk(Combined),
	{1, -1} = risk(?SHORT_UNDERLYING),
	{-1, 1} = risk(Synthetic).	

stretch_one_short_one_put_test() ->
	Put = #option{px = 2.00, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([98, 100.0, 102], stretch(pxs(Position), Position)).

stretch_one_short_one_put_list_test() ->
	Put = #option{px = 2.00, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([[{98, 0.0}, {100.0, 2.0}, {102, 2.0}]], stretch([pxByPnl(Position)], [Position])).

stretch_one_short_one_put_force_stretch_test() ->
	Put = #option{px = 2.01, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([97, 100.0, 102], stretch(pxs(Position), Position)).

stretch_long_straddle_test() ->
	Call = #option{px = 0.70, strike = 100.0},
	Put = #option{px = 1.70, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	Position = #position{long = Long},	
	?assertMatch([97, 100.0, 103], stretch(pxs(Position), Position)).

stretch_long_straddle_list_test() ->
	Call = #option{px = 0.70, strike = 100.0},
	Put = #option{px = 1.70, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	Position = #position{long = Long},	
	?assertMatch([[{97, _}, {100.0, -2.4}, {103, _}]], stretch([pxByPnl(Position)], [Position])).

stretch_short_straddle_test() ->
	Call = #option{px = 0.7, strike = 100.0},
	Put = #option{px = 1.7, strike = 100.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	?assertMatch([97, 100.0, 103], stretch(pxs(Position), Position)).

stretch_short_straddle_different_strike_test() ->
	Call = #option{px = 1.15, strike = 105.0},
	Put = #option{px = 1.55, strike = 95.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	?assertMatch([92, 95.0, 105.0, 108], stretch(pxs(Position), Position)).

stretch_one_underlying_test() ->
	Underlying = #underlying{px = 99.0},
	Long = #side{underlyings = [Underlying]},
	Position = #position{ long = Long },
	?assertMatch([97, 99.0, 101], stretch(pxs(Position), Position)).
