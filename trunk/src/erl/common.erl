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

-module(common).
-export([floor/1, ceiling/1, join/1, mean/1, min/2, max/2]).

% things that should just be in erlang

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

mean([]) ->
	0.0;
mean(List) ->
	lists:sum(List) / length(List).

min(X, Y) when X < Y ->
	X;
min(_, Y) ->
  	Y.

max(X, Y) when X > Y ->
	X;
max(_, Y) ->
	Y.

join(List) ->
    join(List, []).
join([], Acc) ->
    Acc;
join([H|T], []) ->
    join(T, H);
join([H|T], Acc) ->
    join(T, lists:append([Acc, ",", H])).