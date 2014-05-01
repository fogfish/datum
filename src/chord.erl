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
  ,address/2
  ,address/1
  ,whereis/2
  ,successors/2
  ,successors/3
  ,predecessors/2
  ,predecessors/3
  ,members/1
  ,lookup/2
  ,join/2
  ,leave/2
]).

-type(key()  :: any()).
-type(addr() :: integer()).

%%
-record(ring, {
   m      =   8       :: integer() % ring modulo
  ,n      =   3       :: integer() % number of replica
  ,q      =   8       :: integer() % number of shards (ranges)
  ,hash   = md5       :: atom()    % hash algorithm
  ,size   =   0       :: integer() % size of ring
  ,keys   = []        :: [{addr(), key()}]
}).

%%
%% create new chord ring
%%
%% Options:
%%   {modulo,  integer()}  - ring module power of 2 is required
%%   {hash,    md5 | sha1} - ring hashing algorithm
%%   {shard,   integer()}  - number of shard 
%%   {replica, integer()}  - number of replicas
-spec(new/0 :: () -> #ring{}).
-spec(new/1 :: (list()) -> #ring{}).

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
%% number of hashed nodes
-spec(size/1 :: (#ring{}) -> integer()).

size(#ring{}=R) ->
   R#ring.size.

%%
%% maps key into address on the ring
-spec(address/2 :: (key() | addr(), #ring{}) -> addr()).

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
-spec(address/1 :: (#ring{}) -> [addr()]).

address(#ring{}=R) ->
   Top = ringtop(R),
   Inc = Top div R#ring.q,
   lists:seq(Inc - 1, Top - 1, Inc).

%%
%% lookup key value at address
-spec(whereis/2 :: (key() | addr(), #ring{}) -> key()).

whereis(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:dropwhile(fun({Shard, _}) -> Shard < Addr end, R#ring.keys) of
      []   -> hd(R#ring.keys);
      List -> hd(List)
   end;
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).

%%
%% return list of predecessors 
-spec(predecessors/2 :: (key() | addr(), #ring{}) -> [key()]).
-spec(predecessors/3 :: (integer(), key() | addr(), #ring{}) -> [key()]).

predecessors(Key, #ring{}=R) ->
   predecessors(R#ring.n, Key, R).

predecessors(N, Addr, #ring{}=R)
 when is_integer(Addr) ->
   {Head, Tail} = lists:splitwith(fun({Shard, _}) -> Shard < Addr end, R#ring.keys),
   List = case length(Head) of
      L when L >= N ->
         element(1, lists:split(N, lists:reverse(Head)));
      L ->
         lists:reverse(Head) ++ element(1, lists:split(N - L, lists:reverse(Tail)))
   end,
   [X || {_, X} <- List];

predecessors(N, Key, Ring) ->
   predecessors(N, address(Key, Ring), Ring).

%% 
%% return list of successors
-spec(successors/2 :: (key() | addr(), #ring{}) -> [key()]).
-spec(successors/3 :: (integer(), key() | addr(), #ring{}) -> [key()]).

successors(Key, #ring{}=R) ->
   successors(R#ring.n, Key, R).

successors(N, Addr, #ring{}=R)
 when is_integer(Addr) ->
   {Head, Tail} = lists:splitwith(fun({Shard, _}) -> Shard < Addr end, R#ring.keys),
   List = case length(Tail) of
      L when L >= N ->
         element(1, lists:split(N, Tail));
      L ->
         Tail ++ element(1, lists:split(N - L, Head))
   end,
   [X || {_, X} <- List];
successors(N, Key, Ring) ->
   successors(N, address(Key, Ring), Ring).

%%
%% return list of ring members
-spec(members/1 :: (#ring{}) -> [key()]).

members(#ring{}=S) ->
   [Key || {_, Key} <- S#ring.keys].

%%
%% lookup key / shard (in contrast with whereis return actual shard)
-spec(lookup/2 :: (key() | addr(), #ring{}) -> [{addr(), key()}]).

lookup(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         [];
      Shard ->
         [Shard]
   end;

lookup(Key, #ring{}=R) ->
   lookup(address(Key, R), R).


%%
%% join node to the ring
-spec(join/2 :: (key(), #ring{}) -> #ring{}).

join(Key, #ring{}=R) ->
   join(address(Key, R), Key, R).

join(Addr, Key, #ring{}=R) ->
   R#ring{
      size = R#ring.size + 1
     ,keys = orddict:store(Addr, Key, R#ring.keys)
   }.

%%
%% leave node from ring
-spec(leave/2 :: (key() | addr(), #ring{}) -> #ring{}).

leave(Addr, #ring{}=R)
 when is_integer(Addr) ->
   R#ring{
      size = R#ring.size - 1
     ,keys = orddict:erase(Addr, R#ring.keys)
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



