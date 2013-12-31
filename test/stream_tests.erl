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


  % ,drop/2
  % ,dropwhile/2
  % ,filter/2
  % ,fold/3
  % ,foreach/2
  % ,map/2
  % ,scan/3
  % ,take/2
  % ,takewhile/2
  % ,unfold/2
  % ,zip/1
  % ,zip/2

  % ,list/1
  % ,list/2
