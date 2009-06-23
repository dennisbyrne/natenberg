-module(chapter11).
-export([conversion/2, synthetic_long/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(Position, ToOption) ->
	Synthetic = synthetic_long(Position, ToOption),
	merge(Synthetic, Position, "Converted " ++ Position#position.description).

synthetic_long(Position, ToOption) ->
	{Puts, Calls} = put_call_parity(Position#position.long, ToOption),
	#position{description = "Synthetic", long = #side{puts = Puts}, short = #side{calls = Calls}}.

reversal(Position, ToOption) ->
	Synthetic = synthetic_short(Position, ToOption),
	merge(Synthetic, Position, "Reversed " ++ Position#position.description).

synthetic_short(Position, ToOption) ->
	{Puts, Calls} = put_call_parity(Position#position.short, ToOption),
	#position{long = #side{calls = Calls}, short = #side{puts = Puts}}.

%three_way_long(Position, ToOption) ->
%	Synthetic = synthetic_long(Position, ToOption),
%	InTheMoney = #option{},
%	merge(Synthetic, #position{}).

put_call_parity(Side, ToOption) ->
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Side#side.underlyings],
	{[ #option{px = Px, strike = Strike} || {{_, Px}, Strike} <- Pxs],
	 [ #option{px = Px, strike = Strike} || {{Px, _}, Strike} <- Pxs]}.

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

-define(TO_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

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
