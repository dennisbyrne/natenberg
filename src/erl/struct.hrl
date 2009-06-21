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

-record(underlying, {px, name}).
-record(option, {px, strike, expiration}).
-record(side, {underlyings=[], calls=[], puts=[]}).
-record(position, {long=#side{}, short=#side{}, deltas=dict:new(), description}).

-define(UNDERLYING, #side{underlyings = [#underlying{px = 99.0}]}).

-define(LONG_UNDERLYING, 
		#position{description = "Long Underlying, Page 15", long = ?UNDERLYING}).

-define(SHORT_UNDERLYING, 
		#position{description = "Short Underlying, Page 15", short = ?UNDERLYING}).

-define(LONG_CALL, 
		#position{description = "Long Call, page 16",
				  long = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).

-define(SHORT_CALL, 
		#position{description = "Short Call",
				  short = #side{calls = [#option{px = 1.15, strike = 105.0}]}}).

-define(LONG_PUT, 
		#position{description = "Long Put",
				  long = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).

-define(SHORT_PUT, 
		#position{description = "Short Put",
				  short = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).

-define(LONG_STRADDLE, 
		#position{description = "Long Straddle",
				  long = #side{calls = [#option{px = 2.7, strike = 100.0}], 
				  			   puts = [#option{px = 3.7, strike = 100.0}]}}).

-define(SHORT_STRADDLE, 
		#position{description = "Short Straddle",
				  short = #side{calls = [#option{px = 2.7, strike = 100.0}], 
				  				puts = [#option{px = 3.7, strike = 100.0}]}}).

-define(SHORT_STRANGLE, 
		#position{description = "Short Strangle",
				  short = #side{calls = [#option{px = 1.15, strike = 105.0}], 
				  				puts = [#option{px = 1.55, strike = 95.0}]}}).

-define(CALL_BULL_SPREAD,
		#position{description = "Call Bull Spread, Page 23",
				  long = #side{calls = [#option{px = 9.35, strike = 90.0}]}, 
				  short = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).

-define(PUT_BEAR_SPREAD,
		#position{description = "Put Bear Spread, Page 24",
				  long = #side{puts = [#option{px = 7.1, strike = 105.0}]}, 
				  short = #side{puts = [#option{px = 3.7, strike = 100.0}]}}).

-define(CALL_RATIO_VERTICAL_SPREAD,
		#position{description = "Call Ratio Vertical Spread",
				  long = #side{calls = [#option{px = 5.5, strike = 95.0}]}, 
				  short = #side{calls = lists:duplicate(3, #option{px = 1.15, strike = 105.0})}}).

-define(PAGE_29,
		#position{description = "Page 29",
				  long = #side{calls = lists:duplicate(2, #option{px = 2.7, strike = 100.0}), 
							   puts = lists:duplicate(2, #option{px = 3.7, strike = 100.0})}, 
				  short = #side{calls = [#option{px = 9.35, strike = 90.0}], 
								puts = lists:duplicate(4, #option{px = 1.55, strike = 95.0})}}).

-define(MARCH_OPTION, #option{expiration = {2009, 3}}).

-define(CALL_BACKSPREAD,
		#position{description = "Call Backspread",
				  long = #side{calls = lists:duplicate(30, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0})},
				  short = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0})}}).

-define(JUNE_OPTION, #option{expiration = {2009, 6}}).

-define(PUT_BACKSPREAD,
		#position{description = "Put Backspread",
				  long = #side{puts = lists:duplicate(45, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0})},
				  short = #side{puts = lists:duplicate(30, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0})}}).

-define(PUT_RATIO_VERTICAL_SPREAD,
		#position{description = "Put Ratio Verical Spread, Page 140",
				  long = #side{puts = lists:duplicate(20, (?MARCH_OPTION)#option{px = 0.85, strike = 95.0})},
				  short = #side{puts = lists:duplicate(60, (?MARCH_OPTION)#option{px = 0.17, strike = 90.0})}}).

-define(LONG_STRANGLE,
		#position{description = "Long Strangle, Page 143",
				  long = #side{calls = lists:duplicate(20, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0}),
							   puts = lists:duplicate(20, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0})}}).

-define(LONG_BUTTERFLY,
		#position{description = "Long Butterfly, Page 146",
				  long = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0}) ++
							  		   lists:duplicate(10, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0})},
				  short = #side{calls = lists:duplicate(20, (?MARCH_OPTION)#option{px = 2.69, strike = 100.0})}}).

-define(SHORT_BUTTERFLY,
		#position{description = "Short Butterfly, Page 146",
				  long = #side{puts = lists:duplicate(50, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0})},
				  short = #side{puts = lists:duplicate(25, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0}) ++
									   lists:duplicate(25, (?JUNE_OPTION)#option{px = 7.66, strike = 105.0})}}).

-define(LONG_CALL_XMAS_TREE,
		#position{description = "Long Call Christmas Tree, Page 158",
				  long = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0})},
				  short = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 2.69, strike = 100.0}) ++
										lists:duplicate(10, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0})}}).

-define(LONG_PUT_XMAS_TREE,
		#position{description = "Long Put Christmas Tree, Page 158",
				  long = #side{puts = lists:duplicate(25, (?JUNE_OPTION)#option{px = 11.3, strike = 110.0})},
				  short = #side{puts = lists:duplicate(25, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0}) ++
									   lists:duplicate(25, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0})}}).

-define(SHORT_CALL_XMAS_TREE,
		#position{description = "Short Call Christmas Tree, Page 158",
				  long = #side{calls = lists:duplicate(5, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0}) ++
									   lists:duplicate(5, (?JUNE_OPTION)#option{px = 2.77, strike = 105.0})},
				  short = #side{calls = lists:duplicate(5, (?JUNE_OPTION)#option{px = 10.97, strike = 90.0})}}).

-define(SHORT_PUT_XMAS_TREE,
		#position{description = "Short Put Christmas Tree, Page 158",
				  long = #side{puts = lists:duplicate(50, (?MARCH_OPTION)#option{px = 5.92, strike = 105.0}) ++
									  lists:duplicate(50, (?MARCH_OPTION)#option{px = 0.85, strike = 95.0})},
				  short = #side{puts = lists:duplicate(50, (?MARCH_OPTION)#option{px = 10.16, strike = 110.0})}}).

-define(LONG_IRON_BUTTERFLY, 
		#position{description = "Long Iron Butterfly, Page 159",
				  long = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 2.69, strike = 100.0}),
							   puts = lists:duplicate(10, (?MARCH_OPTION)#option{px = 2.68, strike = 100.0})},
				  short = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0}),
								puts = lists:duplicate(10, (?MARCH_OPTION)#option{px = 0.85, strike = 95.0})}}).

-define(SHORT_IRON_BUTTERFLY,
		#position{description = "Short Iron Butterfly, Page 159",
				  long = #side{calls = lists:duplicate(15, (?JUNE_OPTION)#option{px = 2.77, strike = 105.0}),
							   puts = lists:duplicate(15, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0})},
				  short = #side{calls = lists:duplicate(15, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0}),
							    puts = lists:duplicate(15, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0})}}).

-define(LONG_CALL_CONDOR,
   		#position{description = "Long Condor, Page 159",
				  long = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 10.1, strike = 90.0}) ++
									   lists:duplicate(10, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0})},
				  short = #side{calls = lists:duplicate(10, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0}) ++
										lists:duplicate(10, (?MARCH_OPTION)#option{px = 2.69, strike = 100.0})}}).

-define(LONG_PUT_CONDOR,
		#position{description = "Long Put Condor, Page 159",
				  long = #side{puts = lists:duplicate(25, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0}) ++
									  lists:duplicate(25, (?JUNE_OPTION)#option{px = 11.33, strike = 110.0})},
				  short = #side{puts = lists:duplicate(25, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0}) ++
									   lists:duplicate(25, (?JUNE_OPTION)#option{px = 7.66, strike = 105.0})}}).

-define(SHORT_CALL_CONDOR,
		#position{description = "Short Call Condor, Page 159",
				  long = #side{calls = lists:duplicate(50, (?MARCH_OPTION)#option{px = 2.69, strike = 100.0}) ++
									   lists:duplicate(50, (?MARCH_OPTION)#option{px = 0.95, strike = 105.0})},
				  short = #side{calls = lists:duplicate(50, (?MARCH_OPTION)#option{px = 5.82, strike = 95.0}) ++
										lists:duplicate(50, (?MARCH_OPTION)#option{px = 0.26, strike = 110.0})}}).

-define(SHORT_PUT_CONDOR,
		#position{description = "Short Put Condor, Page 159",
				  long = #side{puts = lists:duplicate(5, (?JUNE_OPTION)#option{px = 2.55, strike = 95.0}) ++
									  lists:duplicate(5, (?JUNE_OPTION)#option{px = 4.71, strike = 100.0})},
				  short = #side{puts = lists:duplicate(5, (?JUNE_OPTION)#option{px = 1.18, strike = 90.0}) ++
									   lists:duplicate(5, (?JUNE_OPTION)#option{px = 7.66, strike = 105.0})}}).
