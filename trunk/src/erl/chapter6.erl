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

-module(chapter6).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gamma(Px, Rise, Position) ->
	abs((chapter5:delta(Px + Rise, Position) - chapter5:delta(Px, Position)) / Rise).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gamma_page_103_test() ->
	Pxs = [8.0, 9.0, 10.0, 11.0, 12.0],
	Deltas = [15.0, 20.0, 25.0, 30.0, 35.0],
	DeltasByPx = dict:from_list(lists:zip(Pxs, Deltas)),
	Position = #position{long = #side{calls = [#option{}]}, 
						 deltas = DeltasByPx},
	5.0 = gamma(10.0, 1.0, Position),
	5.0 = gamma(10.0, 2.0, Position),
	5.0 = gamma(10.0, -1.0, Position).
