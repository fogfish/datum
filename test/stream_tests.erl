-module(stream_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
	{} = stream:new(),

	S  = stream:new(1),
	1  = stream:head(S),
	{} = stream:tail(S).

constant_test() ->
	S = stream:constant([1,2,3]),
	1 = stream:head(S),
	2 = stream:head(stream:tail(S)),
	3 = stream:head(stream:tail(stream:tail(S))),
	1 = stream:head(stream:tail(stream:tail(stream:tail(S)))),
	2 = stream:head(stream:tail(stream:tail(stream:tail(stream:tail(S))))).

unfold_test() ->
	S = stream:unfold(1, fun(X) -> X * 2 end),
	1 = stream:head(S),
	2 = stream:head(stream:tail(S)),
	4 = stream:head(stream:tail(stream:tail(S))),
	8 = stream:head(stream:tail(stream:tail(stream:tail(S)))).

drop_test() ->
	S = stream:drop(2,
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	4 = stream:head(S),
	8 = stream:head(stream:tail(S)).

dropwhile_test() ->
	S = stream:dropwhile(
		fun(X) -> X < 4 end,
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	4 = stream:head(S),
	8 = stream:head(stream:tail(S)).

filter_test() ->
	S = stream:filter(
		fun(X) -> (X rem 4) =:= 0 end,
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	4 = stream:head(S),
	8 = stream:head(stream:tail(S)).

fold_test() ->
	15 = stream:fold(
		fun(X, Acc) -> X + Acc end, 0,
		stream:take(4, 
			stream:unfold(1, fun(X) -> X * 2 end)
		)
	).

foreach_test() ->
	ok = stream:foreach(
		fun(X) -> X * X end,
		stream:take(4, 
			stream:unfold(1, fun(X) -> X * 2 end)
		)
	).

map_test() ->
	S = stream:map(
		fun(X) -> X + 1 end,
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	2 = stream:head(S),
	3 = stream:head(stream:tail(S)),
	5 = stream:head(stream:tail(stream:tail(S))),
	9 = stream:head(stream:tail(stream:tail(stream:tail(S)))).

scan_test() ->
	S = stream:scan(
		fun(X, Acc) -> Acc + X end, 0,
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	1 = stream:head(S),
	3 = stream:head(stream:tail(S)),
	7 = stream:head(stream:tail(stream:tail(S))),
	15 = stream:head(stream:tail(stream:tail(stream:tail(S)))).

take_test() ->
	S = stream:take(2, 
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	1  = stream:head(S),
	2  = stream:head(stream:tail(S)),
	{} = stream:tail(stream:tail(S)).


takewhile_test() ->
	S = stream:takewhile(
		fun(X) -> X < 8 end, 
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	1  = stream:head(S),
	2  = stream:head(stream:tail(S)),
	4  = stream:head(stream:tail(stream:tail(S))),
	{} = stream:tail(stream:tail(stream:tail(S))).


zip_test() ->
	A = stream:unfold(1,  fun(X) -> X * 2 end),
	B = stream:unfold(10, fun(X) -> X * 2 end),
	S = stream:zip([A, B]), 
	[1, 10] = stream:head(S),
	[2, 20] = stream:head(stream:tail(S)),
	[4, 40] = stream:head(stream:tail(stream:tail(S))),
	[8, 80] = stream:head(stream:tail(stream:tail(stream:tail(S)))).


list_test() ->
	[1, 2, 4, 8] = stream:list(4, 
		stream:unfold(1, fun(X) -> X * 2 end)
	),
	[1, 2, 4, 8] = stream:list(
		stream:take(4, stream:unfold(1, fun(X) -> X * 2 end))
	).
