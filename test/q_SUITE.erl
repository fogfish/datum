-module(q_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

-export([
   queue/1,
   deq_option/1,
   dequeue/1,
   deqt_option/1,
   last/1,
   liat/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, q},
      {group, deq}
   ].

groups() ->
   [
      {q, [parallel], 
         [queue, deq_option]},

      {deq, [parallel], 
         [queue, deq_option, dequeue, deqt_option, last, liat]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(Type, Config) ->
   [{type, Type}|Config].

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

queue(Config) ->
   Type   = ?config(type, Config),
   Seq    = lists:seq(1, 5),
   Queue0 = lists:foldl(fun Type:enq/2, Type:new(), Seq),
   {1, Queue1} = Type:deq(Queue0),
   false = Type:is_empty(Queue1),

   {2, Queue2} = Type:deq(Queue1),
   false = Type:is_empty(Queue2),

   {3, Queue3} = Type:deq(Queue2),
   false = Type:is_empty(Queue3),

   {4, Queue4} = Type:deq(Queue3),
   false = Type:is_empty(Queue4),

   {5, Queue5} = Type:deq(Queue4),
   true = Type:is_empty(Queue5).


deq_option(Config) ->
   Type   = ?config(type, Config),
   Empty  = Type:new(),
   {undefined, Empty} = Type:deq(Empty).


dequeue(Config) ->
   Type   = ?config(type, Config),
   Seq    = lists:seq(1, 5),
   Queue0 = lists:foldl(fun Type:enqh/2, Type:new(), Seq),
   {1, Queue1} = Type:deqt(Queue0),
   false = Type:is_empty(Queue1),

   {2, Queue2} = Type:deqt(Queue1),
   false = Type:is_empty(Queue2),

   {3, Queue3} = Type:deqt(Queue2),
   false = Type:is_empty(Queue3),

   {4, Queue4} = Type:deqt(Queue3),
   false = Type:is_empty(Queue4),

   {5, Queue5} = Type:deqt(Queue4),
   true = Type:is_empty(Queue5).

deqt_option(Config) ->
   Type   = ?config(type, Config),
   Empty  = Type:new(),
   {undefined, Empty} = Type:deqt(Empty).


last(Config) ->
   Type   = ?config(type, Config),
   Queue  = Type:build([1, 2, 3, 4]),
   4      = Type:last(Queue).

liat(Config) ->
   Type   = ?config(type, Config),
   Queue  = Type:build([1, 2, 3, 4]),
   Queue  = Type:liat(Type:enq(x, Queue)).

