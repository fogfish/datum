-module(q_tests).
-include_lib("eunit/include/eunit.hrl").

q_enc_deq_test() ->
   Seq = lists:seq(1, 5),
   Q0  = lists:foldl(fun q:enq/2, q:new(), Seq),
   {1, Q1} = q:deq(Q0),   
   {2, Q2} = q:deq(Q1),   
   {3, Q3} = q:deq(Q2),   
   {4, Q4} = q:deq(Q3),   
   {5, {}} = q:deq(Q4).   

q_hd_tl_test() ->
   Seq = lists:seq(1, 5),
   Q   = lists:foldl(fun q:enq/2, q:new(), Seq),
   1   = q:head(Q),
   2   = q:head(q:tail(Q)),
   3   = q:head(q:tail(q:tail(Q))),
   4   = q:head(q:tail(q:tail(q:tail(Q)))),
   5   = q:head(q:tail(q:tail(q:tail(q:tail(Q))))),
   {}  = q:tail(q:tail(q:tail(q:tail(q:tail(Q))))).

q_dropwhile_test() ->
   Seq = lists:seq(1, 5),
   Q   = q:dropwhile(
      fun(X) -> X =< 2 end,
      lists:foldl(fun q:enq/2, q:new(), Seq)
   ),
   3   = q:head(Q),
   4   = q:head(q:tail(Q)),
   5   = q:head(q:tail(q:tail(Q))),
   {}  = q:tail(q:tail(q:tail(Q))).

