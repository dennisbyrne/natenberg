-module(chapter1).
-export([pnl/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("position.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

