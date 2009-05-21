-module(json).
-export([to_json/4]).
-include_lib("eunit/include/eunit.hrl").

to_json(Width, Height, Lines, Labels) ->
	JsonWidth = "width:" ++ integer_to_list(Width),
	JsonHeight = "height:" ++ integer_to_list(Height),
	JsonLines = "lines:" ++ to_lines(Lines),
	JsonLabels = "labels:" ++ to_labels(Labels), 
	to_json([JsonWidth, JsonHeight, JsonLines, JsonLabels]).

to_lines(Lines) ->
	JsonLines = lists:map(fun to_line/1, Lines),
	"[" ++ common:join(JsonLines) ++ "]".

to_line({To, From}) ->
	to_json([ "to:" ++ to_point(To), "from:" ++ to_point(From)]).

to_labels(Labels) ->
	JsonLabels = lists:map(fun to_label/1, Labels),
	"[" ++ common:join(JsonLabels) ++ "]".

to_label({Point, Chars}) ->
	to_json(["pt:" ++ to_point(Point), "text:'" ++  Chars ++ "'"]).

to_point({X, Y}) ->
	to_json(["x:" ++ integer_to_list(X), "y:" ++ integer_to_list(Y)]).

to_json(List) ->
	"{" ++ common:join(List) ++ "}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_json_graph_test() ->
	Line = {{1,2}, {3,4}},
	Expected = "{width:650,height:650,lines:[{to:{x:1,y:2},from:{x:3,y:4}}],labels:[{pt:{x:1,y:3},text:'A'}]}",
	?assertMatch(Expected, to_json(650, 650, [Line], [{{1,3}, "A"}])).

to_lines_two_points_test() ->
	First = {{1,2}, {3,4}},
	Second = {{5,6}, {7,8}},
	Expected = "[{to:{x:1,y:2},from:{x:3,y:4}},{to:{x:5,y:6},from:{x:7,y:8}}]",
	?assertMatch(Expected, to_lines([First, Second])).

to_lines_test() ->
	Line = {{1,2}, {3,4}},
	?assertMatch("[{to:{x:1,y:2},from:{x:3,y:4}}]", to_lines([Line])).

to_lines_empty_test() ->
	?assertMatch("[]", to_lines([])).

to_line_test() ->
	Line = {{1,2}, {3,4}},
	?assertMatch("{to:{x:1,y:2},from:{x:3,y:4}}", to_line(Line)).

to_point_test() ->
	?assertMatch("{x:2,y:7}", to_point({2, 7})).
