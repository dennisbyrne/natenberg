-module(chapter11).
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(#side{underlyings = Underlyings}, ToOption) ->
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Underlyings],
	CallPxs = [ {CallPx, UnderlyingPx} || {{CallPx, _}, UnderlyingPx} <- Pxs],
	PutPxs = [ {PutPx, UnderlyingPx} || {{_, PutPx}, UnderlyingPx} <- Pxs],
	#position{long = #side{underlyings = [ #underlying{px = Px} || {_, Px} <- Pxs],
						   puts = [#option{px = Px, strike = Strike} || {Px, Strike} <- PutPxs ]},
			  short = #side{calls = [#option{px = Px, strike = Strike} || {Px, Strike} <- CallPxs ]}};
conversion(Position, ToOption) ->
	LongPosition = conversion(Position#position.long, ToOption),
	ShortPosition = invert(conversion(Position#position.short, ToOption)),
	merge(LongPosition, ShortPosition, "Conversion of " ++ Position#position.description).

invert(Position) ->
	#position{description = Position#position.description,
			  long = Position#position.short,
			  short = Position#position.long}.

merge(To, From, Description) ->
	#position{description = Description,
			  long = merge(To#position.long, From#position.long),
			  short = merge(To#position.short, From#position.short)}.

merge(To, From) ->
	#side{calls = To#side.calls ++ From#side.calls,
		  puts = To#side.puts ++ From#side.puts,
		  underlyings = To#side.underlyings ++ From#side.underlyings}.

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
