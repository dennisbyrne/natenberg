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

-module(chapter5).
-include_lib("eunit/include/eunit.hrl").

-define(DELTA, [57, 62, 46, 53, 56, 74, 45, 35, 50, 93]).
-define(PXS, [101.35, 102.26, 99.07, 100.39, 100.76, 103.59, 99.26, 98.28, 99.98, 103.78]).
-define(DELTA_BY_PRICE, dict:from_list(lists:zip(?PXS, ?DELTA))).