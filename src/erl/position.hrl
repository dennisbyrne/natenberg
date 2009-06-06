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

-define(LONG_UNDERLYING, 
		#position{description = "Long Underlying",
				  long = #side{underlyings = [#underlying{px = 99.0}]}}).

-define(LONG_CALL, 
		#position{description = "Long Call",
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

-define(PAGE_23, 
		#position{description = "Page 23",
				  long = #side{calls = [#option{px = 9.35, strike = 90.0}]}, 
				  short = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).

-define(PAGE_24, 
		#position{description = "Page 24",
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
