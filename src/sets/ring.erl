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

-include_lib("datum/include/datum.hrl").

-export([
   new/0,
   new/1,
   size/1,
   n/1,
   q/1,
   address/2,
   address/1,
   whereis/2,
   predecessors/2,
   predecessors/3,
   successors/2,
   successors/3,
   members/1,
   i/1,
   dump/1,
   whois/2,
   join/2,
   leave/2
]).

-type(key()  :: any()).
-type(val()  :: any()).
-type(addr() :: integer()).

%%
-record(ring, {
   m      =   8       :: integer()    % ring modulo
  ,n      =   3       :: integer()    % number of replica   
  ,q      =   8       :: integer()    % number of ranges (shards)
  ,hash   = sha       :: atom()       % hash algorithm (md5, sha)
  ,size   =   0       :: integer()    % number of nodes
  ,tokens = undefined :: datum:tree() % token table
  ,keys   = []        :: [key()]      % set of keys
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
%%   {m,    integer()}  - ring module power of 2 is required
%%   {n,    integer()}  - number of replicas
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
%% number of ring members
-spec size(#ring{}) -> integer().

size(#ring{keys = Keys}) ->
   length(Keys).

%%
%% maps key into address on the ring
-spec address(key() | addr(), #ring{}) -> addr().

address(X, #ring{}=R)
 when is_integer(X) ->
   X rem ringtop(R);

address({hash, X}, #ring{m = M})
 when is_binary(X) ->
   <<Addr:M, _/bits>> = X,
   Addr;

address(X, #ring{hash = Mthd}=R)
 when is_binary(X) ->
   Hash = crypto:hash(Mthd, X),
   address({hash, Hash}, R);

address(X, #ring{hash = Mthd}=R) ->
   Hash = crypto:hash(Mthd, term_to_binary(X)),
   address({hash, Hash}, R).

%%
%% return complete set of ring addresses
-spec address(#ring{}) -> integer().

address(#ring{}=R) ->
   Top = ringtop(R),
   Inc = Top div R#ring.q,
   lists:seq(Inc - 1, Top - 1, Inc).

%%
%% lookup the key position on the ring
-spec whereis(key() | addr(), #ring{}) -> {addr(), key()}.

whereis(Addr, #ring{} = Ring)
 when is_integer(Addr) ->
   {Shard, #t{key = Key}} = lookup(Addr, Ring),
   {Shard, Key};
whereis(Key, #ring{}=R) ->
   whereis(address(Key, R), R).
   
lookup(Addr, #ring{tokens = Tokens})
 when is_integer(Addr) ->
   case bst:dropwhile(fun({Shard, _}) -> Shard < Addr end, Tokens) of
      ?tree()  -> 
         bst:min(Tokens);
      Tree -> 
         bst:min(Tree)
   end.

%%
%% return list of N - predecessors slots
%% those N slots are claimed by hopefully distinct N nodes 
%% [ {X,Y} || {_, X} <- ring:predecessors(3, 0, R) ].
-spec predecessors(key() | addr(), #ring{}) -> [{addr(), key()}].
-spec predecessors(integer(), key() | addr(), #ring{}) -> [{addr(), key()}].

predecessors(Key, #ring{n = N} = Ring)->
   predecessors(N, Key, Ring).

predecessors(_, _, #ring{keys = []}) ->
   [];

predecessors(N,  Addr, #ring{tokens = Tokens})
 when is_integer(Addr) ->
   {Head, Tail} = bst:splitwhile(fun({Shard, _}) -> Shard < Addr end, Tokens),
   List = (
      catch bst:foldr(
         fun({Key, Val}, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         [], 
         Head)
   ),
   lists:reverse(
      catch bst:foldr(
         fun({Key, Val}, Acc) -> 
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
%% [ {X,Y} || {_, X} <- ring:successors(3, 0, R) ].
-spec successors(key() | addr(), #ring{}) ->[{addr(), key()}].
-spec successors(integer(), key() | addr(), #ring{}) -> [{addr(), key()}].

successors(Key, #ring{}=R) ->
   successors(R#ring.n, Key, R).

successors(_,_Addr, #ring{keys=[]}) ->
   [];
successors(N, Addr, #ring{tokens = Tokens})
 when is_integer(Addr) ->
   {Head, Tail} = bst:splitwhile(fun({Shard, _}) -> Shard < Addr end, Tokens),
   List = (
      catch bst:foldl(
         fun({Key, Val}, Acc) -> 
            accumulate(N, Key, Val, Acc) 
         end, 
         [], 
         Tail
      )
   ),
   lists:reverse(
      catch bst:foldl(
         fun({Key, Val}, Acc) -> 
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
-spec members(#ring{}) -> [{key(), val()}].

members(#ring{keys = Keys}) ->
   [X || {_, X} <- Keys].

%%
%% return ring statistic
-spec i(#ring{}) -> [{key(), integer()}].

i(#ring{tokens=Tokens}) ->
   bst:foldr(fun i/2, [], Tokens).

i({_, #t{h  = -1, key = Key}}, Acc) -> 
   orddict:update_counter(undefined, 1,
      orddict:update_counter(Key, 1, Acc)
   );
i({_, #t{key=Key}}, Acc) -> 
   orddict:update_counter(Key, 1, Acc).

%%
%% dump ring
-spec dump(#ring{}) -> [{addr(), key()}].

dump(#ring{tokens=Tokens}) ->
   bst:foldr(fun dump/2, [], Tokens).

dump({Addr, #t{h  = -1}}, Acc) -> 
   [{Addr, undefined} | Acc];
dump({Addr, #t{key=Key}}, Acc) -> 
   [{Addr, Key} | Acc].


%%
%% return list of addresses associated with given key
-spec whois(any() | function(), #ring{}) -> [{addr(), key()}].

whois(Key, #ring{tokens = Tokens}) ->
   bst:foldr(
      fun
      ({Shard, #t{key = X}}, Acc) when X =:= Key -> 
         [{Shard, Key} | Acc]; 
      (_, Acc) -> 
         Acc 
      end,
      [],
      Tokens
   ).


has(Addr, #ring{keys = Keys}) ->
   orddict:is_key(Addr, Keys).


%%
%% Join an actor to topology, the operation causes restructure of the topology
%% according its internal optimization algorithms and returns a new topology.
%%
%% A key space is divided into equally sized shards, there are Q shards.
%% Each actor claims about Q/N shards (Q number of shards, N number of keys/nodes).
%% The shard re-allocation process is triggered every time when new actor
%% joins the topology. The ring algorithm uses consistent schema to allocate shards
%% (e.g. other algorithms uses random allocation). Each actor builds a consistent,
%% ordered list of allocations that is used to resolve allocation conflicts.
%%
-spec join(key(), #ring{}) -> #ring{}.

join(Key, #ring{} = Ring)
 when is_binary(Key) ->
   join(address(Key, Ring), Key, Ring).

join(Addr, Key, Ring) ->
   join(has(Addr, Ring), Addr, Key, Ring).

join(false, Addr, Key, Ring) ->
   repair(allocate(hashes(Key, Ring), append(Addr, Key, Ring)));

join(_, _, _, Ring) ->
   Ring.

%%
%%
append(Addr, Key, #ring{keys = Keys} = Ring) ->
   Ring#ring{
      keys = orddict:store(Addr, Key, Keys)
   }.

%%
%% return N generation hashes, derived from key
%% the function ensure that there is N-shard distance
hashes(Key, #ring{q=Q, hash=Mthd}=Ring)
 when is_binary(Key) ->
   lists:sort(
      fun(A, B) -> A#t.addr =< B#t.addr end, 
      naddr(nhash(lists:seq(0, 2 * Q), Mthd, Key), Key, Ring)
   ).

nhash([I|Tail], Mthd, Key) ->
   Hash = crypto:hash(Mthd, Key),
   [{I, Hash} | nhash(Tail, Hash, Mthd, Key)].
nhash([I|Tail], Hash0, Mthd, Key) ->
   Hash = crypto:hash(Mthd, [Key, Hash0]),
   [{I, Hash} | nhash(Tail, Hash, Mthd, Key)];
nhash([], _Hash0, _Mthd, _Key) ->
   [].

naddr(Hashes, Key, Ring) ->
   [#t{h = I, addr = address({hash, Hash}, Ring), key = Key} || {I, Hash} <- Hashes].

%%
%%
allocate(Hashes, Ring) ->
   lists:foldl(fun allocate_hash/2, Ring, Hashes).

allocate_hash(#t{addr = Addr} = Hash, Ring) ->
   allocate_hash_to_shard(lookup(Addr, Ring), Hash, Ring).

%% shard is not allocated to any one
allocate_hash_to_shard({Shard, #t{addr = undefined}}, Hash, Ring) ->
   allocate_shard(Shard, Hash, Ring);

%% this is a master shard of key (key is shard owner), claim it unconditionally
allocate_hash_to_shard({Shard, #t{h = X}}, #t{h = 0} = Hash, Ring)
 when X =/= 0 ->
   allocate_shard(Shard, Hash, Ring);

%% Key collides with allocated shard, smaller address wins
allocate_hash_to_shard({Shard, #t{h = X, addr = Y}}, #t{h = I, addr = Addr} = Hash, Ring) 
 when X =:= I, Y > Addr ->
   allocate_shard(Shard, Hash, Ring);

%% Key collides with allocated shard, smaller hash wins
allocate_hash_to_shard({Shard, #t{h = X}}, #t{h = I} = Hash, Ring) 
 when X > I ->
   allocate_shard(Shard, Hash, Ring);

%% shard is allocated, previous key has higher priority
allocate_hash_to_shard(_, _, Ring) ->
   Ring.

allocate_shard(Shard, Hash, #ring{tokens = Tokens} = Ring) ->
   Ring#ring{tokens = bst:insert(Shard, Hash, Tokens)}.

%%
%% N-hashes set do not claim all shards, some tokens collides to one shard, 
%% thus ring has unallocated shards. repair operation allocates these empty 
%% shards to keys in consistent manner
repair(#ring{tokens = Tokens} = Ring) ->
   Ring#ring{
      tokens = repair_head_shards(
         bst:mapfoldl(fun repair_shard/2, undefined, Tokens)
      )
   }.

%% shard is not allocate, let's temporary assign previous key as holder 
repair_shard({_, #t{h = -1} = Hash}, Key) ->
   {Hash#t{key = Key}, Key};

%% shard is allocated, use its key as candidate 
repair_shard({_, #t{key = Key} = Hash}, _) ->
   {Hash, Key}.

%% head shards might not be allocated, we need to use ring tail 
repair_head_shards({Tokens, _}) ->
   repair_head_shards(bst:min(Tokens), Tokens).

repair_head_shards({_, #t{key = undefined}}, Tokens) ->
   {_, #t{key = Key}} = bst:max(Tokens),
   bst:map(
      fun({_, #t{key = undefined} = T}) -> T#t{key = Key}; ({_, T}) -> T end, 
      Tokens
   );

repair_head_shards(_, Tokens) ->
   Tokens.


%%
%% Leave an actor from topology, the operation causes restructure of the topology
%% according its internal optimization algorithms and returns a new topology.
%% Note, semantic of leave operation do not assume leave due to transitive failures.
%%
-spec leave(key() | addr(), #ring{}) -> #ring{}.

leave(Key, #ring{keys = Keys} = Ring)
 when is_binary(Key) ->
   lists:foldl(
      fun join/2,
      empty(Ring),
      [X || {_, X} <- orddict:erase(address(Key, Ring), Keys)]
   ).

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% ring 
ringtop(#ring{m = Modulo}) ->
   trunc(math:pow(2, Modulo)).

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

