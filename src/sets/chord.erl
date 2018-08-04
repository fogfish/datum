%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   consistent hashing - chord ring. The node is positioned to the ring.
%%   The node address is derived from it's identifier. Node controls all
%%   complete shards clockwise from its address (successor shards)
-module(chord).

-export([
   new/0
  ,new/1
  ,size/1
  ,n/1
  ,q/1
  ,address/2
  ,address/1
  ,whereis/2
  ,successors/2
  ,successors/3
  ,predecessors/2
  ,predecessors/3
  ,members/1
  ,stats/1
  ,filter/2
  ,whois/2
  ,join/2
  ,leave/2
]).

-type(key()  :: any()).
-type(val()  :: any()).
-type(addr() :: integer()).

%%
-record(ring, {
   m      =   8       :: integer() % ring modulo
  ,n      =   3       :: integer() % number of replica
  ,q      =   8       :: integer() % number of ranges (shards)
  ,hash   = md5       :: atom()    % hash algorithm
  ,size   =   0       :: integer() % size of ring
  ,keys   = []        :: [{addr(), key()}]
}).

%%
%% create new chord ring
%%
%% Options:
%%   {m,    integer()} - ring module power of 2 is required
%%   {n,    integer()} - number of replicas
%%   {q,    integer()}  - number of shard 
%%   {hash, md5 | sha1} - ring hashing algorithm
-spec new() -> #ring{}.
-spec new(list()) -> #ring{}.

new() ->
   new([]).
new(Opts) ->
   init(Opts, #ring{}).

init([{modulo, X} | Opts], R) ->
   init(Opts, R#ring{m=X});
init([{m, X} | Opts], R) ->
   init(Opts, R#ring{m=X});

init([{replica, X} | Opts], R) ->
   init(Opts, R#ring{n=X});
init([{n, X} | Opts], R) ->
   init(Opts, R#ring{n=X});

init([{shard, X} | Opts], R) ->
   init(Opts, R#ring{q=X});   
init([{q, X} | Opts], R) ->
   init(Opts, R#ring{q=X});   

init([{hash, X} | Opts], R) ->
   init(Opts, R#ring{hash=X});
init([{_, _} | Opts], R) ->
   init(Opts, R);
init([], R) ->
   empty(R).

%%
%% number of replica
-spec n(#ring{}) -> integer().

n(#ring{n = N}) ->
   N.

%%
%% number of shards
-spec q(#ring{}) -> integer().

q(#ring{q = Q}) ->
   Q.

%%
%% number of hashed nodes
-spec size(#ring{}) -> integer().

size(#ring{}=R) ->
   length(R#ring.keys).

%%
%% maps key into address on the ring
-spec address(key() | addr(), #ring{}) -> addr().

address(X, #ring{}=R)
 when is_integer(X) ->
   X rem ringtop(R);

address({hash, X}, #ring{m=M}) ->
   <<Addr:M, _/bits>> = X,
   Addr;

address(X, #ring{}=R)
 when is_binary(X) ->
   Hash = crypto:hash(R#ring.hash, X),
   address({hash, Hash}, R);

address(X, #ring{}=R) ->
   Hash = crypto:hash(R#ring.hash, term_to_binary(X)),
   address({hash, Hash}, R).

%%
%% return complete set of ring addresses
-spec address(#ring{}) -> [addr()].

address(#ring{}=R) ->
   Top = ringtop(R),
   Inc = Top div R#ring.q,
   lists:seq(Inc - 1, Top - 1, Inc).

%%
%% lookup the key position on the ring
-spec whereis(key() | addr(), #ring{}) -> {addr(), key()}.

whereis(Addr, #ring{}=R)
 when is_integer(Addr) ->
   {X, Key} = case lists:dropwhile(fun({Shard, _}) -> Shard < Addr end, R#ring.keys) of
      []   -> hd(R#ring.keys);
      List -> hd(List)
   end,
   {X, Key};
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).

%%
%% return list of predecessors 
%% [ {X,Y} || {_, X} <- ring:predecessors(3, 0, R), Y <- [ring:get(X, R)] ].
-spec predecessors(key() | addr(), #ring{}) -> [{addr(), key()}].
-spec predecessors(integer(), key() | addr(), #ring{}) -> [{addr(), key()}].

predecessors(Key, #ring{}=R) ->
   predecessors(R#ring.n, Key, R).

predecessors(_, _Addr, #ring{keys=[]}) ->
   [];
predecessors(N,  Addr, #ring{}=R)
 when is_integer(Addr) ->
   %% split tokens to before and after address
   {Head, Tail} = lists:splitwith(fun({Shard, _}) -> Shard < Addr end, R#ring.keys),
   List = case length(Head) of
      L when L >= N ->
         element(1, lists:split(N, lists:reverse(Head)));
      L when (N - L) =< length(Tail) ->
         lists:reverse(Head) ++ element(1, lists:split(N - L, lists:reverse(Tail)));
      _ ->
         lists:reverse(Head) ++ lists:reverse(Tail)
   end,
   [{X, Key} || {X, Key} <- List];

predecessors(N, Key, Ring) ->
   predecessors(N, address(Key, Ring), Ring).

%% 
%% return list of successors
%% [ {X,Y} || {_, X} <- ring:successors(3, 0, R), Y <- [ring:get(X, R)] ].
-spec successors(key() | addr(), #ring{}) ->[{addr(), key()}].
-spec successors(integer(), key() | addr(), #ring{}) -> [{addr(), key()}].

successors(Key, #ring{}=R) ->
   successors(R#ring.n, Key, R).

successors(_,_Addr, #ring{keys=[]}) ->
   [];
successors(N, Addr, #ring{}=R)
 when is_integer(Addr) ->
   {Head, Tail} = lists:splitwith(fun({Shard, _}) -> Shard < Addr end, R#ring.keys),
   List = case length(Tail) of
      L when L >= N ->
         element(1, lists:split(N, Tail));
      L when (N - L) =< length(Head) ->
         Tail ++ element(1, lists:split(N - L, Head));
      _ ->
         Tail ++ Head
   end,
   [{X, Key} || {X, Key} <- List];

successors(N, Key, Ring) ->
   successors(N, address(Key, Ring), Ring).

%%
%% return list of ring members
-spec members(#ring{}) -> [{key(), val()}].

members(#ring{}=S) ->
   [X || {_, X} <- S#ring.keys].

%%
%% return list of ring key and ring allocation in percentage
-spec stats(#ring{}) -> [{key(), float()}].

stats(#ring{keys=[]}) ->
   [];
stats(#ring{keys=[{Addr0, _}|_]=Keys}=Ring) ->
   Top = ringtop(Ring),
   stats(lists:reverse(Keys), Top + Addr0, Top).

stats([{Addr, {Key, _}}|Tail], Prev, Top) ->
   [{Key, 100 * (Prev - Addr) / Top} | stats(Tail, Addr, Top)];
stats([], _Prev, _Top) ->
   [].

%%
%% filter
-spec filter(function(), #ring{}) -> #ring{}.

filter(Fun, #ring{}=R) ->
   Keys = lists:filter(fun({_, X}) -> Fun(X) end, R#ring.keys),
   R#ring{
      size = length(Keys)
     ,keys = Keys
   }.

%%
%% return list of addresses associated with given key
-spec whois(key() | addr(), #ring{}) -> [{addr(), key()}].

whois(Key, #ring{}=R) ->
   Addr = address(Key, R),
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         [];
      {Addr, Key} ->
         [{Addr, Key}]
   end.

%%
%% join key-value to the ring
-spec join(key(), val(), #ring{}) -> #ring{}.

join(Key, #ring{}=R) ->
   join(address(Key, R), Key, R).

join(Addr, Key, #ring{}=R) ->
   R#ring{
      keys = orddict:store(Addr, Key, R#ring.keys)
   }.

%%
%% leave node from ring
-spec leave(key() | addr(), #ring{}) -> #ring{}.

leave(Addr, #ring{}=R)
 when is_integer(Addr) ->
   R#ring{
      keys = orddict:erase(Addr, R#ring.keys)
   };
leave(Key, #ring{}=R) ->
   leave(address(Key, R), R).

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% ring 
ringtop(#ring{}=R) ->
   trunc(math:pow(2, R#ring.m)).

%%
%% empties ring
empty(#ring{}=R) ->
   R#ring{
      size = 0
     ,keys = orddict:new()
   }.



