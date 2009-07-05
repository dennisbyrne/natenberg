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

-module(rest).
-export([pages/0, page15/0, page16/0, page17/0, page18/0, page19/0, page20/0, 
		 page21/0, page22/0, page23/0, page24/0, page26/0, page29/0, page138/0,
		 page139/0, page140/0, page143/0, page146/0, page147/0, page158a/0, 
		 page158b/0, page158c/0, page158d/0, page159a/0, page159b/0, page159c/0,
		 page159d/0, page159e/0, page159f/0, many_long_calls/0, many_short_calls/0,
		 page215/0, page218/0, page219/0, page229a/0, page229b/0, page230a/0,
		 page230b/0, page231/0]).
-include_lib("struct.hrl").

pages() ->
	timer:start(),
	Functions = [page15, page16, page17, page18, page19, page20, 
				 page21, page22, page23, page24, page26, page29, 
				 page138, page139, page140, page143, page146, 
				 page147, page158a, page158b, page158c, page158d,
				 page159a, page159b, page159c, page159d, page159e,
				 page159f, many_long_calls, many_short_calls, page215,
				 page218, page219, page229a, page229b, page230a, 
				 page230b, page231],
	[ timer:apply_after(Seq * 1000, demo, lists:nth(Seq, Functions), []) || Seq <- lists:seq(1, length(Functions)) ].

page15() ->
	chapter2:draw([?LONG_UNDERLYING, ?SHORT_UNDERLYING]).

page16() ->
	chapter2:draw(?LONG_CALL).

page17() ->
	chapter2:draw(?SHORT_CALL).

page18() ->
	chapter2:draw(?LONG_PUT).

page19() ->
	chapter2:draw(?SHORT_PUT).

page20() ->
	chapter2:draw(?LONG_STRADDLE).	

page21() ->
	chapter2:draw(?SHORT_STRADDLE).

page22() ->
	chapter2:draw(?SHORT_STRANGLE).

page23() ->
	chapter2:draw(?CALL_BULL_SPREAD).

page24() ->
	chapter2:draw(?PUT_BEAR_SPREAD).

page26() ->
	chapter2:draw(?CALL_RATIO_VERTICAL_SPREAD).

page29() ->
	chapter2:draw(?PAGE_29).

page138() ->
	chapter2:draw(?CALL_BACKSPREAD).

page139() ->
	chapter2:draw(?PUT_BACKSPREAD).

page140() ->
	chapter2:draw(?PUT_RATIO_VERTICAL_SPREAD).

page143() ->
	chapter2:draw(?LONG_STRANGLE).
	
page146() ->
	chapter2:draw(?LONG_BUTTERFLY).

page147() ->
	chapter2:draw(?SHORT_BUTTERFLY).

page158a() ->
	chapter2:draw(?LONG_CALL_XMAS_TREE).

page158b() ->
	chapter2:draw(?LONG_PUT_XMAS_TREE).

page158c() ->
	chapter2:draw(?SHORT_CALL_XMAS_TREE).

page158d() ->
	chapter2:draw(?SHORT_PUT_XMAS_TREE).

page159a() ->
	chapter2:draw(?LONG_IRON_BUTTERFLY).

page159b() ->
	chapter2:draw(?SHORT_IRON_BUTTERFLY).

page159c() ->
	chapter2:draw(?LONG_CALL_CONDOR).

page159d() ->
	chapter2:draw(?LONG_PUT_CONDOR).

page159e() ->
	chapter2:draw(?SHORT_CALL_CONDOR).

page159f() ->
	chapter2:draw(?SHORT_PUT_CONDOR).

many_long_calls() ->
	Long95Call = long_call(5.5, 95.0),
	Long105Call = long_call(1.15, 105.0),
	chapter2:draw([Long95Call, ?LONG_CALL, Long105Call]).

long_call(Px, Strike) ->
	#position{description = "Long Call on Pg 17",
			  long = #side{calls = [#option{px = Px, strike = Strike}]}}.
	  
many_short_calls() ->
	Short95Call = short_call(5.5, 95.0),
	Short105Call = short_call(2.7, 100.0),
	chapter2:draw([Short95Call, Short105Call, ?SHORT_CALL]).

short_call(Px, Strike) ->
	LongCall = long_call(Px, Strike),
	#position{description = "Short Call on Pg 17", short = LongCall#position.long}.

page215() ->
	Call = #option{px = 3.0, strike = 100.0},
	Put = #option{px = 5.0, strike = 100.0},
	LongPosition = #position{description = "Long Position", long = #side{calls = [Call]}},
	ShortPosition = #position{description = "Short Position", short = #side{puts = [Put]}},
	Combined = #position{description = "Synthetic",
						  long = #side{calls = [Call]},
						  short = #side{puts = [Put]}},
	chapter2:draw([LongPosition, Combined, ShortPosition]).

-define(TO_OPTION, dict:from_list([{?UNDERLYING, {3.0, 5.0}}])).

page218() ->
	Synthetic = chapter11:synthetic_long(?LONG_UNDERLYING, ?TO_OPTION),
	Conversion = chapter11:conversion(?LONG_UNDERLYING, ?TO_OPTION),
	chapter2:draw([?LONG_UNDERLYING, Synthetic, Conversion]).

page219() ->
	Synthetic = chapter11:synthetic_short(?SHORT_UNDERLYING, ?TO_OPTION),
	Reversal = chapter11:reversal(?SHORT_UNDERLYING, ?TO_OPTION),
	chapter2:draw([?SHORT_UNDERLYING, Synthetic, Reversal]).

page229a() ->
	Synthetic = chapter11:synthetic_long(?LONG_UNDERLYING, ?TO_OPTION),
	ThreeWay = chapter11:three_way_long(?LONG_UNDERLYING, ?TO_OPTION),
	chapter2:draw([?LONG_UNDERLYING, Synthetic, ThreeWay]).

page229b() ->
	Synthetic = chapter11:synthetic_short(?SHORT_UNDERLYING, ?TO_OPTION),
	ThreeWay = chapter11:three_way_short(?SHORT_UNDERLYING, ?TO_OPTION),
	chapter2:draw([?SHORT_UNDERLYING, Synthetic, ThreeWay]).

page230a() ->
	Low = #position{description = "Synthetic Long",
					long = #side{calls = [#option{px = 3.0, strike = 90.0}]},
					short = #side{puts = [#option{px = 5.0, strike = 90.0}]}},
	High = #position{description = "Synthetic Short",
					 long = #side{puts = [#option{px = 5.0, strike = 100.0}]},
					 short = #side{calls = [#option{px = 3.0, strike = 100.0}]}},
	chapter2:draw([Low, High, position:merge(Low, High, "Box")]).

page230b() ->
	PutBearSpread = #position{description = "Call Bear Spread",
				  			  long = #side{calls = [#option{px = 3.76, strike = 100.0}]}, 
				  			  short = #side{calls = [#option{px = 6.53, strike = 95.0}]}},
	CallBullSpread = #position{description = "Call Bull Spread",
				  			   long = #side{calls = [#option{px = 6.53, strike = 95.0}]}, 
				  			   short = #side{calls = [#option{px = 3.76, strike = 100.0}]}},	
	Box = position:merge(CallBullSpread, PutBearSpread, "Hidden Box"),
	chapter2:draw([CallBullSpread, PutBearSpread, Box]).

page231() ->
	Reversal = #position{description = "Reversal - Underlying",
			  			 long = #side{calls = [(?MARCH_OPTION)#option{px = 2.69, strike = 100.0}]},
			  			 short = #side{puts = [(?MARCH_OPTION)#option{px = 2.68, strike = 100.0}]}},
	Conversion = #position{description = "Conversion - Underlying",
			  			 long = #side{puts = [(?JUNE_OPTION)#option{px = 4.71, strike = 100.0}]},
			  			 short = #side{calls = [(?JUNE_OPTION)#option{px = 4.71, strike = 100.0}]}},
	Roll = position:merge(Reversal, Conversion, "Jelly Roll"),
	chapter2:draw([Reversal, Conversion, Roll]).
