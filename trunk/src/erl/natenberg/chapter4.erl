-module(chapter4).
-include_lib("eunit/include/eunit.hrl").

compute_standard_deviation([]) ->
	0.0;
compute_standard_deviation(Sample) ->
	SampleSize = length(Sample),
	Sum = lists:foldl(fun common:sum/2, 0, Sample),
	Mean = Sum / SampleSize,
	Fun = fun(X) -> math:pow(X - Mean, 2) end,
	MeanDifferences = lists:map(Fun, Sample),
	Variance = lists:foldl(fun common:sum/2, 0, MeanDifferences) / SampleSize, % div by 0?
	math:sqrt(Variance).

volatile_year(Mean, Volatility, Interest) ->
	% forward px sensitive to interest
	% for week and day, the M is the forward px
	ForwardPrice = Mean + Mean * Interest / 100,
	StandardDeviation = Volatility / 1 / 100 * ForwardPrice,
	expect(ForwardPrice, StandardDeviation).

volatile_week(ForwardPx, Volatility) ->
	no_interest_forward_px(ForwardPx, Volatility, 7.2).

volatile_day(ForwardPx, Volatility) ->
	no_interest_forward_px(ForwardPx, Volatility, 16.0).

no_interest_forward_px(ForwardPx, Volatility, Adjustment) ->
	StandardDeviation = Volatility / Adjustment / 100 * ForwardPx,
	expect(ForwardPx, StandardDeviation).	

expect(ForwardPx, StandardDeviation) ->
	{expect(1, ForwardPx, StandardDeviation),
	 expect(2, ForwardPx, StandardDeviation),
	 expect(3, ForwardPx, StandardDeviation)}.	

expect(N, ForwardPx, StandardDeviation) ->
	{ForwardPx - (N * StandardDeviation), ForwardPx + (N * StandardDeviation)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

volatile_year_no_interest_test() ->
	Px = 100.0,
	Volatility = 20.0,
	Interest = 0.0,
	?assertMatch({{80.0, 120.0}, {60.0, 140.0}, {40.0, 160.0}}, volatile_year(Px, Volatility, Interest)).

volatile_year_with_interest_test() ->
	Px = 100.0,
	Volatility = 20.0,
	Interest = 8.0,
	?assertMatch({{86.4, 129.6}, {64.8, 151.2}, {_, 172.8}}, volatile_year(Px, Volatility, Interest)).

volatile_week_test() ->
	{{Left, Right}, _, _} = volatile_week(100.0, 20.0),
 	?assertMatch(97, round(Left)),
	?assertMatch(103, round(Right)).

compute_standard_deviation_test() ->
	Sample = [2, 4, 4, 4, 5, 5, 7, 9],
	?assertMatch(2.0, compute_standard_deviation(Sample)).

compute_standard_deviation_outlier_test() ->
	Sample = [3, 7, 7, 19],
	?assertMatch(6.0, compute_standard_deviation(Sample)).