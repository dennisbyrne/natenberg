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
-record(underlying, {px}).
-record(option, {px, strike}).
-record(side, { underlyings=[], calls=[], puts=[] }).
-record(position, { long=#side{}, short=#side{} }).
-export([pages/0, page15/0, page16/0, page17/0, page18/0, page19/0, page20/0, page21/0, page22/0, page23/0, page24/0, page26/0, page29/0]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pages() ->
	timer:start(),
	Functions = [page15, page16, page17, page18, page19, page20, page21, page22, page23, page24, page26, page29],
	pages(1, 1000, Functions).

pages(Seq, _, []) ->
	Seq;
pages(Seq, Millis, [H|T]) ->
	timer:apply_after(Seq * Millis, chapter2, H, []),
	pages(Seq + 1, Millis, T).

page15() ->
	Underlying = #underlying{px = 99.0},
	Long = #side{underlyings = [Underlying]},
	draw(#position{ long = Long }).

page16() ->
	Call = #option{px = 2.7, strike = 100.0},
	Long = #side{calls = [Call]},
	draw(#position{long = Long}).

page17() ->
	Call = #option{px = 1.15, strike = 105.0},
	Short = #side{calls = [Call]},
	draw(#position{short = Short}).

page18() ->
	Put = #option{px = 1.55, strike = 95.0},
	Long = #side{puts = [Put]},
	draw(#position{long = Long}).	

page19() ->
	Put = #option{px = 1.55, strike = 95.0},
	Short = #side{puts = [Put]},
	draw(#position{short = Short}).

page20() ->
	Call = #option{px = 2.7, strike = 100.0},
	Put = #option{px = 3.7, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	draw(#position{long = Long}).	

page21() ->
	Call = #option{px = 2.7, strike = 100.0},
	Put = #option{px = 3.7, strike = 100.0},
	Short = #side{calls = [Call], puts = [Put]},
	draw(#position{short = Short}).

page22() ->
	Call = #option{px = 1.15, strike = 105.0},
	Put = #option{px = 1.55, strike = 95.0},
	Short = #side{calls = [Call], puts = [Put]},
	draw(#position{short = Short}).

page23() ->
	LongCall = #option{px = 9.35, strike = 90.0},
	ShortCall = #option{px = 2.7, strike = 100.0},
	Long = #side{calls = [LongCall]},
	Short = #side{calls = [ShortCall]},
	draw(#position{long = Long, short = Short}).

page24() ->
	LongPut = #option{px = 7.1, strike = 105.0},
	ShortPut = #option{px = 3.7, strike = 100.0},
	Long = #side{puts = [LongPut]},
	Short = #side{puts = [ShortPut]},
	draw(#position{long = Long, short = Short}).

page26() ->
	LongCall = #option{px = 5.5, strike = 95.0},
	ShortCall = #option{px = 1.15, strike = 105.0},
	Long = #side{calls = [LongCall]},
	Short = #side{calls = [ShortCall, ShortCall, ShortCall]},
	draw(#position{long = Long, short = Short}).

page29() ->
	LongCall = #option{px = 2.7, strike = 100.0},
	ShortCall = #option{px = 9.35, strike = 90.0},
	LongPut = #option{px = 3.7, strike = 100.0},
	ShortPut = #option{px = 1.55, strike = 95.0},
	Long = #side{calls = [LongCall, LongCall], puts = [LongPut, LongPut]},
	Short = #side{calls = [ShortCall], puts = [ShortPut, ShortPut, ShortPut, ShortPut]},
	draw(#position{long = Long, short = Short}).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Position) ->
	view:draw(pxByPnl(Position)).

pxByPnl(Position) ->
	Pxs = pxs(Position),
	Pnls = [ pnl(Px, Position) || Px <- Pxs ],
	lists:zip(Pxs, Pnls).

pxs(Position = #position{long = Long, short = Short}) ->
	LongPxs = pxs(Long),
	ShortPxs = pxs(Short),
	Pxs = lists:usort(LongPxs ++ ShortPxs),
	{DownsideRisk, UpsideRisk} = risk(Position),
	Low = hd(Pxs),
	LowPnl = pnl(Low, Position),
	LowPx = Low - break_even(LowPnl, DownsideRisk),
	High = lists:last(Pxs),
	HighPnl = pnl(High, Position),
	HighPx = High + break_even(HighPnl, UpsideRisk),
	[common:floor(LowPx)] ++ Pxs ++ [common:ceiling(HighPx)];
pxs(#side{underlyings = Underlyings, calls = Calls, puts = Puts}) ->
	[Underlying#underlying.px || Underlying <- Underlyings] ++ 
	[Option#option.strike || Option <- Calls ++ Puts].

break_even(Pnl, Risk) when (Risk > 0 andalso Pnl < 0) or (Risk < 0 andalso Pnl > 0) ->
	Pnl / -Risk;
break_even(_, _) ->
	2. % stretch the x axis by 2 if pnl does not approach 0

pnl(Px, #position{long = Long, short = Short}) ->
	Pnl = [ Px - U#underlying.px || U <- Long#side.underlyings ] ++
	      [-Px + U#underlying.px || U <- Short#side.underlyings ] ++
	      [-C#option.px || C <- Long#side.calls,  Px =< C#option.strike ] ++
	      [ C#option.px || C <- Short#side.calls, Px =< C#option.strike ] ++
	      [-P#option.px || P <- Long#side.puts,   Px >= P#option.strike ] ++
	      [ P#option.px || P <- Short#side.puts,  Px >= P#option.strike ] ++
	      [ Px - C#option.strike - C#option.px || C <- Long#side.calls,  Px > C#option.strike ] ++
	      [-Px + C#option.strike + C#option.px || C <- Short#side.calls, Px > C#option.strike ] ++
	      [-P#option.strike + Px + P#option.px || P <- Short#side.puts,  Px < P#option.strike ] ++
	      [ P#option.strike - Px - P#option.px || P <- Long#side.puts,   Px < P#option.strike ],
	lists:foldl(fun common:sum/2, 0.0, Pnl).

risk(#position{long = Long, short = Short}) ->
	#side{underlyings = LongUnderlyings, calls = LongCalls, puts = LongPuts} = Long,
	#side{underlyings = ShortUnderlyings, calls = ShortCalls, puts = ShortPuts} = Short,
	{length(LongPuts) - length(ShortPuts) + length(ShortUnderlyings) - length(LongUnderlyings), 
	 length(LongCalls) - length(ShortCalls) - length(ShortUnderlyings) + length(LongUnderlyings)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	
pnls_empty_position_test() ->
	Position = #position{},
	?assertMatch(0.0, pnl(89.0, Position)).

pnls_short_straddle_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	Put = #option{px = 1.0, strike = 100.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	?assertMatch(-1.0, pnl(97.0, Position)),
	?assertMatch(-1.0, pnl(103.0, Position)),	
	?assertMatch(0.0, pnl(98.0, Position)),
	?assertMatch(0.0, pnl(102.0, Position)),	
	?assertMatch(2.0, pnl(100.0, Position)).

pnls_long_straddle_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	Put = #option{px = 1.0, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	Position = #position{long = Long},
	?assertMatch(1.0, pnl(97.0, Position)),
	?assertMatch(1.0, pnl(103.0, Position)),
	?assertMatch(0.0, pnl(98.0, Position)),
	?assertMatch(0.0, pnl(102.0, Position)),
	?assertMatch(-2.0, pnl(100.0, Position)).

pnls_one_short_put_test() ->
	Put = #option{px = 1.0, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{short = Short},
	?assertMatch(-1.0, pnl(98.0, Position)),
	?assertMatch(0.0, pnl(99.0, Position)),
	?assertMatch(1.0, pnl(100.0, Position)),
	?assertMatch(1.0, pnl(101.0, Position)).

pnls_one_short_call_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	Short = #side{calls = [Call]},
	Position = #position{ short = Short },
	?assertMatch(1.0, pnl(99.0, Position)),
	?assertMatch(1.0, pnl(100.0, Position)),
	?assertMatch(0.0, pnl(101.0, Position)),
	?assertMatch(-1.0, pnl(102.0, Position)).

pnls_one_long_call_test() ->
	Call = #option{px = 1.0, strike = 100.0},
	Long = #side{calls = [Call]},
	Position = #position{ long = Long },
	?assertMatch(-1.0, pnl(99.0, Position)),
	?assertMatch(-1.0, pnl(100.0, Position)),
	?assertMatch(0.0, pnl(101.0, Position)),
	?assertMatch(1.0, pnl(102.0, Position)).

pnls_one_long_put_test() ->
	Put = #option{px = 1.0, strike = 100.0},
	Long = #side{puts = [Put]},
	Position = #position{ long = Long },
	?assertMatch(-1.0, pnl(101.0, Position)),
	?assertMatch(-1.0, pnl(100.0, Position)),
	?assertMatch(0.0, pnl(99.0, Position)),
	?assertMatch(1.0, pnl(98.0, Position)).

pxs_one_short_one_put_test() ->
	Put = #option{px = 2.00, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([98, 100.0, 102], pxs(Position)).

pxs_one_short_one_put_force_stretch_test() ->
	Put = #option{px = 2.01, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([97, 100.0, 102], pxs(Position)).

pxs_long_straddle_test() ->
	Call = #option{px = 0.70, strike = 100.0},
	Put = #option{px = 1.70, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	Position = #position{long = Long},	
	?assertMatch([97, 100.0, 103], pxs(Position)).

pxs_short_straddle_test() ->
	Call = #option{px = 0.7, strike = 100.0},
	Put = #option{px = 1.7, strike = 100.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	?assertMatch([97, 100.0, 103], pxs(Position)).

pxs_short_straddle_different_strike_test() ->
	Call = #option{px = 1.15, strike = 105.0},
	Put = #option{px = 1.55, strike = 95.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	?assertMatch([92, 95.0, 105.0, 108], pxs(Position)).

pxs_one_underlying_test() ->
	Underlying = #underlying{px = 99.0},
	Long = #side{underlyings = [Underlying]},
	Position = #position{ long = Long },
	?assertMatch([97, 99.0, 101], pxs(Position)).
