-module(demo).
-export([pages/0, page15/0, page16/0, page17/0, page18/0, page19/0, page20/0, 
		 page21/0, page22/0, page23/0, page24/0, page26/0, page29/0, page138/0,
		 page139/0, page140/0, page143/0, page146/0, page147/0, page158a/0, 
		 page158b/0, page158c/0, page158d/0, page159a/0, page159b/0, page159c/0,
		 page159d/0, page159e/0, page159f/0]).
-include_lib("position.hrl").

pages() ->
	timer:start(),
	Functions = [page15, page16, page17, page18, page19, page20, 
				 page21, page22, page23, page24, page26, page29, 
				 page138, page139, page140, page143, page146, 
				 page147, page158a, page158b, page158c, page158d,
				 page159a, page159b, page159c, page159d, page159e,
				 page159f],
	[ timer:apply_after(Seq * 1000, demo, lists:nth(Seq, Functions), []) || Seq <- lists:seq(1, length(Functions)) ].

page15() ->
	chapter2:draw(?LONG_UNDERLYING).

page16() ->
	chapter2:draw(?LONG_CALL).

page17() ->
	chapter2:draw(?SHORT_CALL).

page18() ->
	chapter2:draw(?LONG_PUT).

page19() ->
	chapter2:draw(?SHORT_PUT).

page20() ->
	chapter2:draw(?LONG_STRADDLE).	

page21() ->
	chapter2:draw(?SHORT_STRADDLE).

page22() ->
	chapter2:draw(?SHORT_STRANGLE).

page23() ->
	chapter2:draw(?PAGE_23).

page24() ->
	chapter2:draw(?PAGE_24).

page26() ->
	chapter2:draw(?CALL_RATIO_VERTICAL_SPREAD).

page29() ->
	chapter2:draw(?PAGE_29).

page138() ->
	chapter2:draw(?CALL_BACKSPREAD).

page139() ->
	chapter2:draw(?PUT_BACKSPREAD).

page140() ->
	chapter2:draw(?PUT_RATIO_VERTICAL_SPREAD).

page143() ->
	chapter2:draw(?LONG_STRANGLE).
	
page146() ->
	chapter2:draw(?LONG_BUTTERFLY).

page147() ->
	chapter2:draw(?SHORT_BUTTERFLY).

page158a() ->
	chapter2:draw(?LONG_CALL_XMAS_TREE).

page158b() ->
	chapter2:draw(?LONG_PUT_XMAS_TREE).

page158c() ->
	chapter2:draw(?SHORT_CALL_XMAS_TREE).

page158d() ->
	chapter2:draw(?SHORT_PUT_XMAS_TREE).

page159a() ->
	chapter2:draw(?LONG_IRON_BUTTERFLY).

page159b() ->
	chapter2:draw(?SHORT_IRON_BUTTERFLY).

page159c() ->
	chapter2:draw(?LONG_CALL_CONDOR).

page159d() ->
	chapter2:draw(?LONG_PUT_CONDOR).

page159e() ->
	chapter2:draw(?SHORT_CALL_CONDOR).

page159f() ->
	chapter2:draw(?SHORT_PUT_CONDOR).
