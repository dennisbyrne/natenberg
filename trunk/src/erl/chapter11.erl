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
	#position{description = "Synthetic of " ++ Desc,
			  long = #side{calls = [#option{px = Px, strike = Strike} || {Px, Strike} <- CallPxs ]},
			  short = #side{puts = [#option{px = Px, strike = Strike} || {Px, Strike} <- PutPxs ]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversion_test() ->
	ToOption = dict:from_list([{?UNDERLYING, {3.0, 5.0}}]),
	Synthetic = #position{description = "Synthetic of Long Underlying on Pg 15", 
						  long = #side{calls = [#option{px = 3.0, strike = 99.0}]},
						  short = #side{puts = [#option{px = 5.0, strike = 99.0}]}},
	?assertEqual(Synthetic, conversion(?LONG_UNDERLYING, ToOption)).
