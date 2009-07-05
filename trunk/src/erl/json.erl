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

-module(json).
-export([to_json/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

to_json(Desc, Lines, Labels) ->
	JsonDesc = "\"description\":\"" ++ Desc ++ "\"",
	JsonLines = "\"lines\":" ++ to_lines(Lines),
	JsonLabels = "\"labels\":" ++ to_labels(Labels), 
	to_json([JsonDesc, JsonLines, JsonLabels]).

to_lines(Lines) ->
	JsonLines = lists:map(fun to_line/1, Lines),
	"[" ++ common:join(JsonLines) ++ "]".

to_line({To, From, Color}) ->
	to_json(["\"to\":" ++ to_point(To), "\"from\":" ++ to_point(From), "\"color\":" ++ integer_to_list(Color)]).

to_labels(Labels) ->
	JsonLabels = lists:map(fun to_label/1, Labels),
	"[" ++ common:join(JsonLabels) ++ "]".

to_label({Point, Chars}) ->
	to_json(["\"pt\":" ++ to_point(Point), "\"text\":\"" ++  Chars ++ "\""]).

to_point({X, Y}) ->
	to_json(["\"x\":" ++ integer_to_list(X), "\"y\":" ++ integer_to_list(Y)]).

to_json(List) ->
	"{" ++ common:join(List) ++ "}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_json_graph_test() ->
	Line = {{1,2}, {3,4}, 0},
	Expected = "{\"description\":\"Msg\",\"lines\":[{\"to\":{\"x\":1,\"y\":2},\"from\":{\"x\":3,\"y\":4},\"color\":0}],\"labels\":[{\"pt\":{\"x\":1,\"y\":3},\"text\":\"A\"}]}",
	?assertMatch(Expected, to_json("Msg", [Line], [{{1,3}, "A"}])).

to_lines_two_points_test() ->
	First = {{1,2}, {3,4}, 0},
	Second = {{5,6}, {7,8}, 0},
	Expected = "[{\"to\":{\"x\":1,\"y\":2},\"from\":{\"x\":3,\"y\":4},\"color\":0},{\"to\":{\"x\":5,\"y\":6},\"from\":{\"x\":7,\"y\":8},\"color\":0}]",
	?assertMatch(Expected, to_lines([First, Second])).

to_lines_test() ->
	Line = {{1,2}, {3,4}, 0},
	?assertMatch("[{\"to\":{\"x\":1,\"y\":2},\"from\":{\"x\":3,\"y\":4},\"color\":0}]", to_lines([Line])).

to_lines_empty_test() ->
	?assertMatch("[]", to_lines([])).

to_line_test() ->
	Line = {{1,2}, {3,4},0},
	?assertMatch("{\"to\":{\"x\":1,\"y\":2},\"from\":{\"x\":3,\"y\":4},\"color\":0}", to_line(Line)).

to_point_test() ->
	?assertMatch("{\"x\":2,\"y\":7}", to_point({2, 7})).
