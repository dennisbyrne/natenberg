-module(chapter5).
-include_lib("eunit/include/eunit.hrl").

-define(DELTA, [57, 62, 46, 53, 56, 74, 45, 35, 50, 93]).
-define(PXS, [101.35, 102.26, 99.07, 100.39, 100.76, 103.59, 99.26, 98.28, 99.98, 103.78]).
-define(DELTA_BY_PRICE, dict:from_list(lists:zip(?PXS, ?DELTA))).