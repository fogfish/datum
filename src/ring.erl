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
%%   consistent hashing using tokens ring. 
%%
%%   The ring is a consistent hashing schema on ring modulo 2^m,
%%   it is divided on equally sized Q shards. Each key claims 
%%   about Q/N shards (Q number of shards, N number of keys/nodes). 
%%   The shard allocation algorithm uses token approach to bind shard to key.
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
  ,i/1
  ,dump/1
  ,whois/2
  ,get/2
  ,join/3
  ,leave/2

  ,hashes/2
  ,lookup/2
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

%% token
-record(t, {
   h      = -1        :: integer()    % hash generation
  ,addr   = undefined :: integer()    % ring address of hash generation 
  ,key    = undefined :: any()        % key claiming shard
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
   {Addr0, T} = lookup(Addr, R),
   {Addr0, T#t.key};
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).
   
lookup(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case bst:dropwhile(fun(Shard) -> Shard < Addr end, R#ring.tokens) of
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
predecessors(N,  Addr, #ring{tokens = Tokens})
 when is_integer(Addr) ->
   {Head, Tail} = bst:splitwith(fun(Addr0) -> Addr0 < Addr end, Tokens),
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
successors(N, Addr, #ring{tokens = Tokens})
 when is_integer(Addr) ->
   {Head, Tail} = bst:splitwith(fun(Addr0) -> Addr0 < Addr end, Tokens),
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
%% return ring statistic
-spec(i/1 :: (#ring{}) -> [{key(), integer()}]).

i(#ring{tokens=Tokens}) ->
   bst:foldr(fun i/3, [], Tokens).

i(_, #t{h = -1, key = Key}, Acc) ->
   orddict:update_counter(Key, 1,
      orddict:update_counter(undefined, 1, Acc)
   );
i(_, #t{key = Key}, Acc) -> 
   orddict:update_counter(Key, 1, Acc).

%%
%% dump ring allocation
-spec(dump/1 :: (#ring{}) -> [{addr(), key()}]).

dump(#ring{tokens=Tokens}) ->
   bst:foldr(fun dump/3, [], Tokens).

dump(Addr, #t{h  = -1}, Acc) -> 
   [{Addr, undefined} | Acc];
dump(Addr, #t{key=Key}, Acc) -> 
   [{Addr, Key} | Acc].


%%
%% return list of addresses associated with given key
-spec(whois/2 :: (any() | function(), #ring{}) -> [{addr(), key()}]).

whois(Key, #ring{keys = Keys, tokens = Tokens}=R) ->
   Addr = address(Key, R),
   case lists:keyfind(Addr, 1, Keys) of
      false ->
         [];
      {_, {Key0, _}} ->
         bst:foldr(
            fun
            (X, #t{key = Key1}, Acc) when Key1 =:= Key0 -> 
               [{X, Key1}|Acc]; 
            (_, _, Acc) -> 
               Acc 
            end,
            [],
            Tokens
         )
   end.

%%
%% return value associated with given key
-spec(get/2 :: (key(), #ring{}) -> val()).

get(Key, #ring{}=R) ->
   Addr = address(Key, R),
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         exit(badarg);
      {_X, {_Key, Val}} ->
         Val
   end.


%%
%% join key-value to the ring
-spec(join/3 :: (key(), val(), #ring{}) -> #ring{}).

join(Key, Val, #ring{}=R) ->
   join(address(Key, R), Key, Val, R).

join(Addr, Key, Val, #ring{q = Q, keys = Keys}=R) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      %% new key, update token allocation
      false ->
         repair(
            join_token(hashes(Key, R), Key, 
               R#ring{
                  keys = orddict:store(Addr, {Key, Val}, Keys)
               }
            )
         );
      %% existed key update value only
      _     ->
         R#ring{
            keys = orddict:store(Addr, {Key, Val}, R#ring.keys)
         }
   end.

join_token([{I, Addr}|Tail], Key, #ring{tokens = Tokens}=R) ->
   %% allocate hash token from address space
   case lookup(Addr, R) of
      %% slot is not allocated to any one
      {Addr0, #t{addr = undefined}} ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Addr0, #t{h = I, addr = Addr, key = Key}, Tokens)}
         );

      %% Key own master shard, claim it unconditionally
      {Addr0, #t{h = X}} when X =/= 0, I =:= 0 ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Addr0, #t{h = I, addr = Addr, key = Key}, Tokens)}
         );

      %% Key collides with allocated shard, smaller address wins
      {Addr0, #t{h = X, addr =Y}} when X =:= I, Y > Addr ->
         join_token(Tail, Key,
            R#ring{tokens = bst:insert(Addr0, #t{h = I, addr = Addr, key = Key}, Tokens)}
         );

      %% Key collides with allocated shard, smaller hash wins
      %% @todo: evaluate role of N-hash generation on allocation
      % {Addr0, #t{h = X, addr = Y}} when X =/= 0, Y > Addr ->
      {Addr0, #t{h = X, addr = _}} when X > I ->
         join_token(Tail, Key, 
            R#ring{tokens = bst:insert(Addr0, #t{h = I, addr = Addr, key = Key}, Tokens)}
         );

      %% shard is allocated, previous key has priority
      _ ->
         join_token(Tail, Key, R)
   end;

join_token([], _Key, #ring{}=R) ->
   R.

%%
%% repair ring, N-hashes set do not claim all shards
%% some tokens collides to one shard, thus ring has unallocated shards 
%% repair operation allocates these empty shards to keys in consistent manner
repair(#ring{tokens = Tokens0}=R) ->
   {Tkns, _} = bst:mapfoldl(
      fun(_, #t{h = H, key = Key}=T, Acc) ->
         case H of
            -1 ->
               {T#t{key = Acc}, Acc};
            _  ->
               {T, Key}
         end
      end,
      undefined,
      Tokens0
   ),
   Tokens = case bst:min(Tkns) of
      {_, #t{key = undefined}} ->
         {_, #t{key = Key}} = bst:max(Tkns),
         bst:map(
            fun(_, #t{key = undefined}=T) -> T#t{key = Key}; (_, T) -> T end, 
            Tkns
         );
      _ ->
         Tkns
   end,
   R#ring{tokens = Tokens}.


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
     ,tokens = bst:build([{Addr, #t{}} || Addr <- address(R)])
     ,keys   = []
   }.

%%
%% accumulate N nodes to list 
%% (throw list out when N node collected)
accumulate(N,  Addr, #t{key = Key}, Acc) 
 when length(Acc) < N  ->
   [{Addr, Key} | Acc];
accumulate(_, _Addr, _T, Acc) ->
   throw(Acc).

%%
%% return N generation hashes, derived from key
%% the function ensure that there is N-shard distance
hashes(Key, #ring{q=Q, n=N, hash=Mthd}=Ring) ->
   naddr(nhash(lists:seq(0, Q), Mthd, s(Key)), Ring).

nhash([I|Tail], Mthd, Key) ->
   Hash = crypto:hash(Mthd, Key),
   [{I, Hash} | nhash(Tail, Hash, Mthd, Key)].
nhash([I|Tail], Hash0, Mthd, Key) ->
   Hash = crypto:hash(Mthd, [Key, Hash0]),
   [{I, Hash} | nhash(Tail, Hash, Mthd, Key)];
nhash([], _Hash0, _Mthd, _Key) ->
   [].

naddr([{I, Hash}|Tail], Ring) ->
   Addr = address({hash, Hash}, Ring),
   [{I, Addr} | naddr(Tail, Ring)];
naddr([], _Ring) ->
   [].

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





