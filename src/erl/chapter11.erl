-module(chapter11).
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion(Position, ToOption) ->
	#position{description = Desc, long = Long, short = _} = Position,
	Pxs = [ {dict:fetch(U, ToOption), U#underlying.px} || U <- Long#side.underlyings],
	CallPxs = [ {CallPx, UnderlyingPx} || {{CallPx, _}, UnderlyingPx} <- Pxs],
	PutPxs = [ {PutPx, UnderlyingPx} || {{_, PutPx}, UnderlyingPx} <- Pxs],
	#position{description = "Conversion of " ++ Desc,
			  long = #side{underlyings = [ #underlying{px = Px} || {_, Px} <- Pxs],
						   puts = [#option{px = Px, strike = Strike} || {Px, Strike} <- PutPxs ]},
			  short = #side{calls = [#option{px = Px, strike = Strike} || {Px, Strike} <- CallPxs ]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(T0_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

conversion_long_test() ->
	Synthetic = #position{description = "Conversion of Long Underlying on Pg 15", 
						  long = #side{underlyings = [?UNDERLYING],
									   puts = [#option{px = 5.0, strike = 99.0}]},
						  short = #side{calls = [#option{px = 3.0, strike = 99.0}]}},
	?assertEqual(Synthetic, conversion(?LONG_UNDERLYING, ?T0_OPTION)).
