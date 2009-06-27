-module(chapter11).
-export([conversion/2, reversal/2, synthetic_long/2, synthetic_short/2, 
		 three_way_long/2, three_way_short/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(Position, ToOption) ->
	Synthetic = synthetic_long(Position, ToOption),
	position:merge(Synthetic, Position, "Converted " ++ Position#position.description).

synthetic_long(Position, ToOption) ->
	{Puts, Calls} = put_call_parity(Position#position.long, ToOption),
	#position{description = "Synthetic", long = #side{puts = Puts}, short = #side{calls = Calls}}.

reversal(Position, ToOption) ->
	Synthetic = synthetic_short(Position, ToOption),
	position:merge(Synthetic, Position, "Reversed " ++ Position#position.description).

synthetic_short(Position, ToOption) ->
	{Puts, Calls} = put_call_parity(Position#position.short, ToOption),
	#position{description = "Synthetic", long = #side{calls = Calls}, short = #side{puts = Puts}}.

three_way_long(Position, ToOption) ->
	Synthetic = synthetic_long(Position, ToOption),
	[U] = (Position#position.long)#side.underlyings,
	{CallParity, _} = dict:fetch(U, ToOption),
	In = #option{px = CallParity + 10, strike = U#underlying.px - 10},
	position:merge(Synthetic, #position{long = #side{calls = [In]}}, "Three Way").

three_way_short(Position, ToOption) ->
	Synthetic = synthetic_short(Position, ToOption),
	[U] = (Position#position.short)#side.underlyings,
	In = #option{px = 0.01, strike = U#underlying.px + 10},
	position:merge(Synthetic, #position{long = #side{puts = [In]}}, "Three Way").

put_call_parity(Side, ToOption) ->
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Side#side.underlyings],
	{[ #option{px = Px, strike = Strike} || {{_, Px}, Strike} <- Pxs],
	 [ #option{px = Px, strike = Strike} || {{Px, _}, Strike} <- Pxs]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TO_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

three_way_short_test() ->
	ThreeWay = #position{description = "Three Way",
						 long = #side{calls = [#option{px = 3.0, strike = 99.0}],
									  puts = [#option{px = 0.01, strike = 109.0}]},
						 short = #side{puts = [#option{px = 5.0, strike = 99.0}]}},
	?assertEqual(ThreeWay, three_way_short(?SHORT_UNDERLYING, ?TO_OPTION)).

three_way_long_test() ->
	ThreeWay = #position{description = "Three Way",
						 long = #side{calls = [#option{px = 13.0, strike = 89.0}],
									  puts = [#option{px = 5.0, strike = 99.0}]},
						 short = #side{calls = [#option{px = 3.0, strike = 99.0}]}},
	?assertEqual(ThreeWay, three_way_long(?LONG_UNDERLYING, ?TO_OPTION)).

combined_reversal_test() ->
	Combined = reversal(?SHORT_UNDERLYING, ?TO_OPTION),
	Pnl = chapter1:pnl(10.0, Combined),
	Pnl = chapter1:pnl(34.2, Combined).

combined_conversion_test() ->
	Combined = conversion(?LONG_UNDERLYING, ?TO_OPTION),
	Pnl = chapter1:pnl(10.0, Combined),
	Pnl = chapter1:pnl(34.2, Combined).

reversal_test() ->
	Combined = #position{description = "Reversed Short Underlying on Pg 15", 
						 long = #side{calls = [#option{px = 3.0, strike = 99.0}]},
						 short = #side{underlyings = [?UNDERLYING],
									   puts = [#option{px = 5.0, strike = 99.0}]}},
	?assertEqual(Combined, reversal(?SHORT_UNDERLYING, ?TO_OPTION)).

conversion_test() ->
	Combined = #position{description = "Converted Long Underlying on Pg 15", 
						 long = #side{underlyings = [?UNDERLYING],
									  puts = [#option{px = 5.0, strike = 99.0}]},
						 short = #side{calls = [#option{px = 3.0, strike = 99.0}]}},
	?assertEqual(Combined, conversion(?LONG_UNDERLYING, ?TO_OPTION)).
