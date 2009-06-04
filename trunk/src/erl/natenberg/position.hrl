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
-record(option, {px, strike}).
-record(side, {underlyings=[], calls=[], puts=[]}).
-record(position, {long=#side{}, short=#side{}, deltas=dict:new()}).

-define(LONG_UNDERLYING, #position{long = #side{underlyings = [#underlying{px = 99.0}]}}).
-define(LONG_CALL, #position{long = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).
-define(SHORT_CALL, #position{short = #side{calls = [#option{px = 1.15, strike = 105.0}]}}).
-define(LONG_PUT, #position{long = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).
-define(SHORT_PUT, #position{short = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).
-define(LONG_STRADDLE, #position{long = #side{calls = [#option{px = 2.7, strike = 100.0}], 
											  puts = [#option{px = 3.7, strike = 100.0}]}}).
-define(SHORT_STRADDLE, #position{short = #side{calls = [#option{px = 2.7, strike = 100.0}], 
												puts = [#option{px = 3.7, strike = 100.0}]}}).
-define(SHORT_STRANGLE, #position{short = #side{calls = [#option{px = 1.15, strike = 105.0}], 
												puts = [#option{px = 1.55, strike = 95.0}]}}).
-define(PAGE_23, #position{long = #side{calls = [#option{px = 9.35, strike = 90.0}]}, 
						   short = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).
-define(PAGE_24, #position{long = #side{puts = [#option{px = 7.1, strike = 105.0}]}, 
						   short = #side{puts = [#option{px = 3.7, strike = 100.0}]}}).
-define(CALL_RATIO_VERTICAL_SPREAD, #position{long = #side{calls = [#option{px = 5.5, strike = 95.0}]}, 
											  short = #side{calls = lists:duplicate(3, #option{px = 1.15, strike = 105.0})}}).
-define(PAGE_29, #position{long = #side{calls = lists:duplicate(2, #option{px = 2.7, strike = 100.0}), 
										puts = lists:duplicate(2, #option{px = 3.7, strike = 100.0})}, 
						   short = #side{calls = [#option{px = 9.35, strike = 90.0}], 
										 puts = lists:duplicate(4, #option{px = 1.55, strike = 95.0})}}).