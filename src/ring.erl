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
%%   consistent hashing - token ring. Each node claims about S/N shards (S number
%%   of shards, N number of nodes). The shard to node allocation is controlled
%%   by token. Ring is a consistent hashing schema on ring modulo 2^m
%%   The key space is divided into equally sized shards.
%%   Shards are claimed and release by nodes. 
-module(ring).

-export([
   new/0
  ,new/1
  ,size/1
  ,n/1
  ,address/2
  ,address/1
  ,whereis/2
  ,predecessors/2
  ,predecessors/3
  ,successors/2
  ,successors/3
  ,members/1
  ,lookup/2
  ,get/2
  ,join/3
  ,leave/2
]).

-type(key()  :: any()).
-type(val()  :: any()).
-type(addr() :: integer()).

%%
-record(ring, {
   m      =   8       :: integer()    % ring modulo
  ,n      =   3       :: integer()    % number of replica   
  ,q      =   8       :: integer()    % number of ranges (shards)
  ,hash   = md5       :: atom()       % hash algorithm
  ,size   =   0       :: integer()    % number of nodes
  ,tokens = undefined :: datum:tree() % token table
  ,keys   = []        :: [{addr(), {key(), val()}}]
}).


%%
%% create new token ring
%%
%% Options:
%%   {m,    integer()} - ring module power of 2 is required
%%   {n,    integer()} - number of replicas
%%   {q,    integer()}  - number of shard 
%%   {hash, md5 | sha1} - ring hashing algorithm
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
%% number of ring members
-spec(size/1 :: (#ring{}) -> integer()).

size(#ring{}=R) ->
   length(R#ring.keys).

%%
%% number of replica
-spec(n/1 :: (#ring{}) -> integer()).

n(#ring{n=N}) ->
   N.

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
-spec(address/1 :: (#ring{}) -> integer()).

address(#ring{}=R) ->
   Top = ringtop(R),
   Inc = Top div R#ring.q,
   lists:seq(Inc - 1, Top - 1, Inc).

%%
%% lookup the key position on the ring
-spec(whereis/2 :: (key() | addr(), #ring{}) -> {addr(), key()}).

whereis(Addr, #ring{}=R)
 when is_integer(Addr) ->
   value(where_is_it(Addr, R));
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).
   
where_is_it(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case bst:dropwhile(fun(Shard, _) -> Shard < Addr end, R#ring.tokens) of
      nil  -> 
         bst:min(R#ring.tokens);
      Tree -> 
         bst:min(Tree)
   end.

%%
%% return list of N - predecessors slots
%% those N slots are claimed by hopefully distinct N nodes 
%% [ {X,Y} || {_, X} <- ring:predecessors(3, 0, R), Y <- [ring:get(X, R)] ].
-spec(predecessors/2 :: (key() | addr(), #ring{}) -> [{addr(), key()}]).
-spec(predecessors/3 :: (integer(), key() | addr(), #ring{}) -> [{addr(), key()}]).

predecessors(Key, #ring{}=R) ->
   predecessors(R#ring.n, Key, R).

predecessors(_, _Addr, #ring{keys=[]}) ->
   [];
predecessors(N,  Addr, #ring{}=R)
 when is_integer(Addr) ->
   %% @todo: - fix performance for ring with small number of nodes
   %%          full shard table scan is performed to accumulate N shards
   {Head, Tail} = bst:splitwith(fun(Shard, _) -> Shard < Addr end, R#ring.tokens),
   List = (
      catch bst:foldr(
         fun(Key, Val, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         [], 
         Head)
   ),
   lists:reverse(
      catch bst:foldr(
         fun(Key, Val, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         List, 
         Tail
      )
   );

predecessors(N, Key, Ring) ->
   predecessors(N, address(Key, Ring), Ring).

%% 
%% return list of N - successors slots
%% those N slots are claimed by hopefully distinct N nodes 
%% [ {X,Y} || {_, X} <- ring:successors(3, 0, R), Y <- [ring:get(X, R)] ].
-spec(successors/2 :: (key() | addr(), #ring{}) ->[{addr(), key()}]).
-spec(successors/3 :: (integer(), key() | addr(), #ring{}) -> [{addr(), key()}]).

successors(Key, #ring{}=R) ->
   successors(R#ring.n, Key, R).

successors(_,_Addr, #ring{keys=[]}) ->
   [];
successors(N, Addr, #ring{}=R)
 when is_integer(Addr) ->
   {Head, Tail} = bst:splitwith(fun(Shard, _) -> Shard < Addr end, R#ring.tokens),
   List = (
      catch bst:foldl(
         fun(Key, Val, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         [], 
         Tail
      )
   ),
   lists:reverse(
      catch bst:foldl(
         fun(Key, Val, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         List, 
         Head
      )
   );

successors(N, Key, Ring) ->
   successors(N, address(Key, Ring), Ring).

%%
%% return list of ring members
-spec(members/1 :: (#ring{}) -> [{key(), val()}]).

members(#ring{}=S) ->
   [X || {_, X} <- S#ring.keys].


%%
%% return list of addresses associated with given key
-spec(lookup/2 :: (any() | function(), #ring{}) -> [{addr(), key()}]).

lookup(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         [];
      {_, {Key0, _}} ->
         bst:foldr(
            fun
            (X, {_, _, Key}, Acc) when Key =:= Key0 -> 
               [{X, Key}|Acc]; 
            (_, _, Acc) -> 
               Acc 
            end,
            [],
            R#ring.tokens
         )
   end;

lookup(Key, #ring{}=R) ->
   lookup(address(Key, R), R).

%%
%% return value associated with given key
-spec(get/2 :: (key(), #ring{}) -> val()).

get(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         exit(badarg);
      {_X, {_Key, Val}} ->
         Val
   end;

get(Key, #ring{}=R) ->
   ?MODULE:get(address(Key, R), R).


%%
%% join key-value to the ring
-spec(join/3 :: (key(), val(), #ring{}) -> #ring{}).

join(Key, Val, #ring{}=R) ->
   join(address(Key, R), Key, Val, R).

join(Addr, Key, Val, #ring{}=R) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      %% new key, update token allocation
      false ->
         join_token(hashes(R#ring.q, Key, R), Key, 
            R#ring{
               keys = orddict:store(Addr, {Key, Val}, R#ring.keys)
            }
         );
      %% existed key update value only
      _     ->
         R#ring{
            keys = orddict:store(Addr, {Key, Val}, R#ring.keys)
         }
   end.

join_token([{I, Hash}|Tail], Key, #ring{}=R) ->
   %% allocate hash token from address space
   Addr = address({hash, Hash}, R),
   case where_is_it(Addr, R) of
      %% slot is not allocated
      {Shard, undefined} ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Shard, {I, Addr, Key}, R#ring.tokens)}
         );

      %% claim master slot
      {Shard, {X, _, _}} when X =/= 0, I =:= 0 ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Shard, {I, Addr, Key}, R#ring.tokens)}
         );

      %% slot collision, closest key wins
      {Shard, {X, Y, _}} when X =:= I, Y > Addr ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Shard, {I, Addr, Key}, R#ring.tokens)}
         );

      %% slot allocated but new key has higher priority 
      {Shard, {X, Y, _}} when X =/= 0, Y > Addr ->
         join_token(Tail, Key, 
            R#ring{tokens = bst:insert(Shard, {I, Addr, Key}, R#ring.tokens)}
         );

      %% slot is allocated, previous key has priority
      _ ->
         join_token(Tail, Key, R)
   end;

join_token([], _Key, #ring{}=R) ->
   R.

%%
%% leave node from ring
-spec(leave/2 :: (key() | addr(), #ring{}) -> #ring{}).

leave(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keytake(Addr, 1, R#ring.keys) of
      false ->
         R;
      {value, {_, {_Key, _}}, Keys} ->
         %% re-join remaining keys to empty ring
         lists:foldl(
            fun({_, {Key, Val}}, Acc) ->
               join(Key, Val, Acc)
            end,
            empty(R),
            Keys
         )
   end;

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
      size   = 0
     ,tokens = bst:build(address(R))
     ,keys   = []
   }.

%%
%% return value associated with token
value({Addr, {_, _, Key}}) ->
   {Addr, Key};
value({Addr, undefined}) ->
   {Addr, undefined}.


%%
%% accumulate N nodes to list 
%% (throw list out when N node collected)
accumulate(_, _, undefined, Acc) ->
   Acc;
accumulate(N,  Addr, {_, _,  Key}, Acc) 
 when length(Acc) < N  ->
   case lists:keyfind(Key, 2, Acc) of
      false ->
         [{Addr, Key}|Acc];
      _     ->
         Acc
   end;
accumulate(_, _Addr, {_, _, _Key}, Acc) ->
   throw(Acc).

%%
%% return N hashes, derived from address
hashes(N, Key, #ring{}=R) ->
   Token = R#ring.q - N,
   Hash  = crypto:hash(R#ring.hash, s(Key)),
   hashes(N - 1, [{Token, Hash}], Key, R).

hashes(0, Acc, _Addr, #ring{}) ->
   lists:reverse(Acc);
hashes(N, Acc,  Key, #ring{}=R) ->
   {_, Hash0} = hd(Acc),
   Token = R#ring.q - N,
   Hash  = crypto:hash(R#ring.hash, [s(Key), Hash0]),
   hashes(N - 1, [{Token,Hash}|Acc], Key, R).


s(X)
 when is_binary(X) ->
   X;
s(X)
 when is_list(X) ->
   erlang:list_to_binary(X);
s(X)
 when is_integer(X) ->
   erlang:list_to_binary(erlang:integer_to_list(X));
s(X)
 when is_atom(X) ->
   erlang:list_to_binary(erlang:atom_to_list(X)).


