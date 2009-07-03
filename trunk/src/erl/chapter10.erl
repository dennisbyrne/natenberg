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

-module(chapter10).
-export([is_vertical_spread/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_vertical_spread(Position) -> %page 202
	position:long_count(Position) =:= position:short_count(Position) andalso
		position:expiration_count(Position) =:= 1 andalso
			position:strike_count(Position) =:= 2 andalso
				((position:call_count(Position) =:= 0) xor (position:put_count(Position) =:= 0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_vertical_spread_test() ->
	?assertEqual(true, is_vertical_spread(?CALL_BULL_SPREAD)),
	?assertEqual(true, is_vertical_spread(?PUT_BEAR_SPREAD)),
	?assertEqual(false, is_vertical_spread(?LONG_BUTTERFLY)),
	?assertEqual(false, is_vertical_spread(?SHORT_STRANGLE)),
	?assertEqual(false, is_vertical_spread(?LONG_STRADDLE)),
	?assertEqual(false, is_vertical_spread(?PUT_BACKSPREAD)),
	?assertEqual(false, is_vertical_spread(?CALL_RATIO_VERTICAL_SPREAD)).
