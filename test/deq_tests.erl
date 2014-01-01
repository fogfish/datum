-module(deq_tests).
-include_lib("eunit/include/eunit.hrl").

q_enc_deq_test() ->
   Seq = lists:seq(1, 5),
   Q0  = lists:foldl(fun deq:enq/2, deq:new(), Seq),
   {1, Q1} = deq:deq(Q0),   
   {2, Q2} = deq:deq(Q1),   
   {3, Q3} = deq:deq(Q2),   
   {4, Q4} = deq:deq(Q3),   
   {5, {}} = deq:deq(Q4).   

q_hd_tl_test() ->
   Seq = lists:seq(1, 5),
   Q   = lists:foldl(fun deq:enq/2, deq:new(), Seq),
   1   = deq:head(Q),
   2   = deq:head(deq:tail(Q)),
   3   = deq:head(deq:tail(deq:tail(Q))),
   4   = deq:head(deq:tail(deq:tail(deq:tail(Q)))),
   5   = deq:head(deq:tail(deq:tail(deq:tail(deq:tail(Q))))),
   {}  = deq:tail(deq:tail(deq:tail(deq:tail(deq:tail(Q))))).

q_dropwhile_test() ->
   Seq = lists:seq(1, 5),
   Q   = deq:dropwhile(
      fun(X) -> X =< 2 end,
      lists:foldl(fun deq:enq/2, deq:new(), Seq)
   ),
   3   = deq:head(Q),
   4   = deq:head(deq:tail(Q)),
   5   = deq:head(deq:tail(deq:tail(Q))),
   {}  = deq:tail(deq:tail(deq:tail(Q))).

