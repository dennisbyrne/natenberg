-module(chapter11).
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(Position, ToOption) ->
	LongUnderlyings = (Position#position.long)#side.underlyings,
	{ShortCalls, LongPuts} = synthesize(LongUnderlyings, ToOption),
	#position{description = "Converted " ++ Position#position.description,
			  long = #side{underlyings = LongUnderlyings, puts = LongPuts},
			  short = #side{calls = ShortCalls}}.

reversal(Position, ToOption) ->
	ShortUnderlyings = (Position#position.short)#side.underlyings,
	{LongCalls, ShortPuts} = synthesize(ShortUnderlyings, ToOption),
	#position{description = "Reversed " ++ Position#position.description,
			  long = #side{calls = LongCalls},
			  short = #side{underlyings = ShortUnderlyings, puts = ShortPuts}}.	

synthesize(Underlyings, ToOption) ->
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Underlyings],
	{[ #option{px = Px, strike = Strike} || {{Px, _}, Strike} <- Pxs],
	 [ #option{px = Px, strike = Strike} || {{_, Px}, Strike} <- Pxs]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TO_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

combined_reversal_test() ->
	Combined = reversal(?SHORT_UNDERLYING, ?TO_OPTION),
	Pnl = chapter1:pnl(10.0, Combined),
	Pnl = chapter1:pnl(34.2, Combined).

combined_conversion_test() ->
	Combined = conversion(?LONG_UNDERLYING, ?TO_OPTION),
	Pnl = chapter1:pnl(10.0, Combined),
	Pnl = chapter1:pnl(34.2, Combined).

reversal_short_test() ->
	Combined = #position{description = "Reversed Short Underlying on Pg 15", 
						 long = #side{calls = [#option{px = 3.0, strike = 99.0}]},
						 short = #side{underlyings = [?UNDERLYING],
									   puts = [#option{px = 5.0, strike = 99.0}]}},
	?assertEqual(Combined, reversal(?SHORT_UNDERLYING, ?TO_OPTION)).

conversion_long_test() ->
	Combined = #position{description = "Converted Long Underlying on Pg 15", 
						 long = #side{underlyings = [?UNDERLYING],
									  puts = [#option{px = 5.0, strike = 99.0}]},
						 short = #side{calls = [#option{px = 3.0, strike = 99.0}]}},
	?assertEqual(Combined, conversion(?LONG_UNDERLYING, ?TO_OPTION)).
