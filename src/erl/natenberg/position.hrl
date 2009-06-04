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

-define(LONG_UNDERLYING, #position{ long = #side{underlyings = [#underlying{px = 99.0}]}}).
-define(LONG_CALL, #position{long = #side{calls = [#option{px = 2.7, strike = 100.0}]}}).
-define(SHORT_CALL, #position{short = #side{calls = [#option{px = 1.15, strike = 105.0}]}}).
-define(LONG_PUT, #position{long = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).
-define(SHORT_PUT, #position{short = #side{puts = [#option{px = 1.55, strike = 95.0}]}}).
