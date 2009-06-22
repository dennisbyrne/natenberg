-module(chapter11).
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(Position, ToOption) ->
	LongUnderlyings = (Position#position.long)#side.underlyings,
	ShortUnderlyings = (Position#position.short)#side.underlyings,
	{ShortCalls, LongPuts} = convert(LongUnderlyings, ToOption),
	{LongCalls, ShortPuts} = convert(ShortUnderlyings, ToOption),
	#position{description = "Conversion of " ++ Position#position.description,
			  long = #side{underlyings = LongUnderlyings,
						   calls = LongCalls,
						   puts = LongPuts},
			  short = #side{underlyings = ShortUnderlyings,
						    calls = ShortCalls,
						    puts = ShortPuts}}.

convert(Underlyings, ToOption) ->
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Underlyings],
	{[ #option{px = Px, strike = Strike} || {{Px, _}, Strike} <- Pxs],
	 [ #option{px = Px, strike = Strike} || {{_, Px}, Strike} <- Pxs]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(T0_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

conversion_short_test() ->
	Combined = #position{description = "Conversion of Short Underlying on Pg 15", 
						 long = #side{calls = [#option{px = 3.0, strike = 99.0}]},
						 short = #side{underlyings = [?UNDERLYING],
									   puts = [#option{px = 5.0, strike = 99.0}]}},
	?assertEqual(Combined, conversion(?SHORT_UNDERLYING, ?T0_OPTION)).

conversion_long_test() ->
	Combined = #position{description = "Conversion of Long Underlying on Pg 15", 
						 long = #side{underlyings = [?UNDERLYING],
									  puts = [#option{px = 5.0, strike = 99.0}]},
						 short = #side{calls = [#option{px = 3.0, strike = 99.0}]}},
	?assertEqual(Combined, conversion(?LONG_UNDERLYING, ?T0_OPTION)).
