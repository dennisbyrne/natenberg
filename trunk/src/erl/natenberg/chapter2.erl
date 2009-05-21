-module(chapter2).
-record(underlying, {px}).
-record(option, {px, strike}).
-record(side, { underlyings=[], calls=[], puts=[] }).
-record(position, { long=#side{}, short=#side{} }).
-export([long_call/0, short_put/0, long_straddle/0, short_straddle/0, underlying/0]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

long_call() ->
	Call = #option{px = 1.0, strike = 100.0},
	Long = #side{calls = [Call]},
	Position = #position{long = Long},
	draw(Position).

short_put() ->
	Put = #option{px = 1.0, strike = 100.0},
	Short = #side{puts = [Put]},
	Position = #position{short = Short},
	draw(Position).

long_straddle() ->
	Call = #option{px = 1.0, strike = 100.0},
	Put = #option{px = 1.0, strike = 100.0},
	Long = #side{calls = [Call], puts = [Put]},
	Position = #position{long = Long},
	draw(Position).

short_straddle() ->
	Call = #option{px = 1.0, strike = 100.0},
	Put = #option{px = 1.0, strike = 101.0},
	Short = #side{calls = [Call], puts = [Put]},
	Position = #position{short = Short},
	draw(Position).

underlying() ->
	Underlying = #underlying{px = 99.5},
	Long = #side{underlyings = [Underlying]},
	Position = #position{ long = Long },
	draw(Position).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Position) ->
	view:draw(pxByPnl(Position)).

pxByPnl(Position) ->
	Pxs = pxs(Position),
	Pnls = [ pnl(Px, Position) || Px <- Pxs ],
	lists:zip(Pxs, Pnls).

pxs(#position{long = Long, short = Short}) ->
	LongPxs = pxs(Long),
	ShortPxs = pxs(Short),
	Pxs = lists:usort(LongPxs ++ ShortPxs),
	Low = lists:nth(1, Pxs),
	High = lists:last(Pxs),
	[common:floor(Low - 2)] ++ Pxs ++ [common:ceiling(High + 2)];
pxs(#side{underlyings = Underlyings, calls = Calls, puts = Puts}) ->
	[Underlying#underlying.px || Underlying <- Underlyings] ++ 
	[Option#option.strike || Option <- Calls ++ Puts].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	Put = #option{px = 3.24, strike = 101.4},
	Short = #side{puts = [Put]},
	Position = #position{ short = Short },
	?assertMatch([99, 101.4, 104], pxs(Position)).

pxs_one_underlying_test() ->
	Underlying = #underlying{px = 99.0},
	Long = #side{underlyings = [Underlying]},
	Position = #position{ long = Long },
	?assertMatch([97, 99.0, 101], pxs(Position)).
