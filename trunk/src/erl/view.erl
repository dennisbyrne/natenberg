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

-module(view).
-define(TO, {graph, 'view@127.0.0.1'}).
-define(WIDTH, 650).
-define(HEIGHT, 650).
-export([start/0, draw/2]).
-record(rectangle, {minX = 0, maxX = 100, minY = -100, maxY = 100}).
-include_lib("eunit/include/eunit.hrl").
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	Fun = fun() -> os:cmd("cd ../.. && ant") end,
	spawn(Fun).

draw(Points, Desc) ->
	Rectangle = min_bounding_rectangle(Points),
	{XAxis, YAxis} = axes(Rectangle),
	Lines = points_to_lines(Points, Rectangle),
	{Scales, Labels} = scale(XAxis, YAxis, Rectangle),
	Json = json:to_json(Desc, ?WIDTH, ?HEIGHT, Lines ++ [XAxis, YAxis] ++ Scales, Labels),
	?TO ! {draw, Json}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min_bounding_rectangle(Points) ->
	{X, Y} = lists:last(Points),
	{MinX, MaxX, MinY, MaxY} = lists:foldl(fun rectangle/2, {X, X, Y, Y}, Points),
	#rectangle{minX = common:floor(MinX), 
			   maxX = common:ceiling(MaxX),
			   minY = common:floor(MinY) - 1, 
			   maxY = common:ceiling(MaxY) + 1}.

rectangle({X, Y}, {MinX, MaxX, MinY, MaxY}) ->
	{common:min(X, MinX),
	 common:max(X, MaxX),
	 common:min(Y, MinY),
	 common:max(Y, MaxY)}.

axes(Rectangle = #rectangle{minX = MinX, maxX = MaxX, minY = MinY, maxY = MaxY}) ->
	XAxis = {{MinX, 0}, {MaxX, 0}},
	YAxis = {{MinX, MinY}, {MinX, MaxY}},
	{to_line(XAxis, Rectangle), to_line(YAxis, Rectangle)}.

to_lines(Lines, Rectangle) ->
	[ to_line(Line, Rectangle) || Line <- Lines ].

to_line({To, From}, Rectangle) ->
	{to_point(To, Rectangle), to_point(From, Rectangle)}.

to_point({X, Y}, #rectangle{minX = MinX, maxX = MaxX, minY = MinY, maxY = MaxY}) ->
	{to_x(X, MinX, MaxX), to_y(Y, MinY, MaxY)}.

to_x(Value, Min, Max) when Max >= Value andalso Value >= Min andalso Min >= 0 ->
	translate(Value - Min, {Min, Max}, ?WIDTH).

to_y(Value, Min, Max) when Max >= Value andalso Value >= Min ->
	translate(abs(Max - Value), {Min, Max}, ?HEIGHT).

translate(Value, {Min, Max}, Pixels) when Max >= Min ->
	round(Pixels * Value / (Max - Min)).

points_to_lines([_|T] = List, Rectangle) ->
	Zipped = lists:zip(List, T ++ [junk]),
	Lines = lists:sublist(Zipped, length(Zipped) - 1),
	to_lines(Lines, Rectangle).

scale(XAxis, YAxis, Rectangle) ->
	{XTix, XLabels} = scaleX(XAxis, Rectangle#rectangle.minX, Rectangle#rectangle.maxX),
	{YTix, YLabels} = scaleY(YAxis, Rectangle#rectangle.minY, Rectangle#rectangle.maxY),
	{XTix ++ YTix, XLabels ++ YLabels}.

scaleX(Axis, Min, Max) ->
	{{_, Y},_} = Axis,
	scaleX(Y, Min + 1, Min, Max, [], []).

scaleX(_, X, _, Max, Tix, Labels) when X > Max -> % base case
	{Tix, Labels};
scaleX(Y, X, Min, Max, Tix, Labels) ->
	NewX = to_x(X, Min, Max),
	Tick = {{NewX, Y + 5}, {NewX, Y - 5}},
	Label = {{NewX - 4, Y}, integer_to_list(X)},
	scaleX(Y, X + 1, Min, Max, Tix ++ [Tick], Labels ++ [Label]).

scaleY(Axis, Min, Max) ->
	{{X, _},_} = Axis,
	scaleY(X, Min, Min, Max, [], []).

scaleY(_, Y, _, Max, Tix, Labels) when Y > Max -> % base case
	{Tix, Labels};
scaleY(X, Y, Min, Max, Tix, Labels) ->
	NewY = to_y(Y, Min, Max),
	Tick = {{X, NewY}, {X + 5, NewY}},
	Label = {{X, NewY}, integer_to_list(Y)},
	TickSize = if Max - Min < 20 -> 1;
				  true -> 10
			   end,
	scaleY(X, Y + TickSize, Min, Max, Tix ++ [Tick], Labels ++ [Label]).

color([]) ->
	[];
color(Lines) ->
	Length = length(Lines),
	Step = 16777215 div Length, % 0xFFFFFF
	Colors = lists:seq(0, Length * Step, Step),
	Fun = fun(Seq) ->
				  {X, Y} = lists:nth(Seq, Lines),
				  {X, Y, lists:nth(Seq, Colors)} 
		  end,
	[ Fun(Seq) || Seq <- lists:seq(1, length(Lines)) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scaleX_test() ->
	XAxis = {{0, 400}, {650, 400}},
	MinX = 10,
	MaxX = 12,
	{Ticks, Labels} = scaleX(XAxis, MinX, MaxX),
	?assertMatch([{{325, 405}, {325, 395}}, {{650, 405}, {650, 395}}], Ticks),
	?assertMatch([{{321, 400}, "11"}, {{646, 400}, "12"}], Labels).

axes_test() ->
	Rectangle = #rectangle{minX = 50, maxX = 150, minY = -10, maxY = 10},
	?assertMatch({{{0,325},{650,325}},{{0,650},{0,0}}}, axes(Rectangle)).

points_to_lines_test() ->
	Rectangle = #rectangle{minX = 1, maxX = 3, minY = 2, maxY = 4},
	?assertMatch([{{0, 650},{650, 0}}], points_to_lines([{1,2},{3,4}], Rectangle)).

min_bounding_rectangle_single_test() ->
	Left = {100.0, 1.0},
	Right = {103.0, -1.0},
	Expected = #rectangle{minX = 100, maxX = 103, minY = -2, maxY = 2},
	?assertMatch(Expected, min_bounding_rectangle([Left, Right])).

min_bounding_rectangle_range_test() ->
	Left = {100.0, 1.0},
	Middle = {103.0, 0.0},
	Right = {106.0, -1.0},
	Expected = #rectangle{minX = 100, maxX = 106, minY = -2, maxY = 2},
	?assertMatch(Expected, min_bounding_rectangle([Left, Middle, Right])).

to_x_test() ->
	?assertMatch(325, to_x(325.0, 0, 650)).

to_x_lowest_price_test() ->
	?assertMatch(0, to_x(0.0, 0, 200)).

to_x_fractional_bounded_at_zero_test() ->
	?assertMatch(217, to_x(33.0, 0, 99)).

to_x_fractional_test() ->
	?assertMatch(0, to_x(33.0, 33, 99)),
	?assertMatch(325, to_x(66.0, 33, 99)).

to_y_breakeven_test() ->
	?assertMatch(325, to_y(0.0, -325, 325)).

to_y_positive_ranges_test() ->
	?assertMatch(325, to_y(162.5, 0, 325)).

to_y_negative_ranges_test() ->
	?assertMatch(325, to_y(-162.5, -325, 0)).

to_y_negative_corner_case_test() ->
	?assertMatch(650, to_y(-100.0, -100, 100)).

to_y_fractional_test() ->
	?assertMatch(217, to_y(66.0, 0, 99)).

to_y_fractional_negative_test() ->
	?assertMatch(0, to_y(-33.0, -99, -33)),
	?assertMatch(325, to_y(-66.0, -99, -33)),
	?assertMatch(650, to_y(-99.0, -99, -33)).

to_y_fractional_negative_and_positive_test() ->
	?assertMatch(488, to_y(-66.0, -99, 33)),
	?assertMatch(163, to_y(0.0, -99, 33)),
	?assertMatch(0, to_y(33.0, -99, 33)).

% pnl/px -> (x,y) pixels
to_point_test() ->
	% the underlying costs $325, so this is break even
	Point = {325.0, 0.0},
	Rectangle = #rectangle{minX = 0, maxX = 650, minY = -325, maxY = 325},
	% should come out to be right in the middle of the panel
	?assertMatch({325, 325}, to_point(Point, Rectangle)).

to_point_fractional_test() ->
	Point = {33.0, -33.0},
	Rectangle = #rectangle{minX = 33, maxX = 99, minY = -33, maxY = 33},
	?assertMatch({0, 650}, to_point(Point, Rectangle)).

% buy an underlying @ $100, showing the pnl from $0 and $200
to_line_test() ->
	Left = {0.0, -100.0},
	Right = {200.0, 100.0},
	Line = {Left, Right},
	Rectangle = #rectangle{minX = 0, maxX = 200, minY = -100, maxY = 100},
	?assertMatch({{0,650}, {650,0}}, to_line(Line, Rectangle)).

to_line_fractional_test() ->
	Left = {33.0, -33.0},
	Middle = {66.0, 0.0},
	Right = {99.0, 33.0},
	Rectangle = #rectangle{minX = 33, maxX = 99, minY = -33, maxY = 33},
	?assertMatch({{0,650}, {325,325}}, to_line({Left, Middle}, Rectangle)),
	?assertMatch({{325,325}, {650,0}}, to_line({Middle, Right}, Rectangle)),
	?assertMatch({{0,650}, {650,0}}, to_line({Left, Right}, Rectangle)).

color_empty_test() ->
	?assertEqual([], color([])).

color_single_test() ->
	?assertEqual([{1,2,0}], color([{1,2}])).

color_multiple_test() ->
	?assertEqual([{1,2,0},{1,3,8388607}], color([{1,2},{1,3}])).
