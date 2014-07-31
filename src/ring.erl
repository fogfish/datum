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
  ,address/2
  ,address/1
  ,whereis/2
  ,predecessors/2
  ,predecessors/3
  ,successors/2
  ,successors/3
  ,members/1
  ,lookup/2
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
%% number of nodes
-spec(size/1 :: (#ring{}) -> integer()).

size(#ring{}=R) ->
   length(R#ring.keys).

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
%% lookup key-value at address
-spec(whereis/2 :: (key() | addr(), #ring{}) -> {addr(), key(), val()}).

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
-spec(predecessors/2 :: (key() | addr(), #ring{}) -> [{addr(), key(), val()}]).
-spec(predecessors/3 :: (integer(), key() | addr(), #ring{}) -> [{addr(), key(), val()}]).

predecessors(Key, #ring{}=R) ->
   predecessors(R#ring.n, Key, R).

predecessors(_, _Addr, #ring{keys=[]}) ->
   [];
predecessors(N,  Addr, #ring{}=R)
 when is_integer(Addr) ->
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
-spec(successors/2 :: (key() | addr(), #ring{}) ->[{addr(), key(), val()}]).
-spec(successors/3 :: (integer(), key() | addr(), #ring{}) -> [{addr(), key(), val()}]).

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
-spec(members/1 :: (#ring{}) -> [key()]).

members(#ring{}=S) ->
   [X || {_, X} <- S#ring.keys].


%%
%% lookup key / shard (in contrast with whereis return actual shard)
-spec(lookup/2 :: (any() | function(), #ring{}) -> [{addr(), key(), val()}]).

lookup(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keyfind(Addr, 1, R#ring.keys) of
      false ->
         [];
      {_, {Key0, _}} ->
         bst:foldr(
            fun
            (X, {_, _, {Key, Val}}, Acc) when Key =:= Key0 -> 
               [{X, Key, Val}|Acc]; 
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
%% join key-value to the ring
-spec(join/3 :: (key(), val(), #ring{}) -> #ring{}).

join(Key, Val, #ring{}=R) ->
   join(address(Key, R), Key, Val, R).

join(Addr, Key, Val, #ring{}=R) ->
   Keys = orddict:store(Addr, {Key, Val}, R#ring.keys),
   %% estimate Q hashes, they act as allocation token
   join_token(hashes(R#ring.q, Key, R), Key, Val, R#ring{keys=Keys}).

join_token([{I, Hash}|Tail], Key, Val, #ring{}=R) ->
   %% allocate hash token from address space
   Addr = address({hash, Hash}, R),
   case where_is_it(Addr, R) of
      %% slot is not allocated
      {Shard, undefined} ->
         join_token(Tail, Key, Val, 
            R#ring{tokens = bst:insert(Shard, {I, Addr, {Key, Val}}, R#ring.tokens)}
         );

      %% slot is allocated by same key but new value joins
      {Shard, {_, _, {Key, _}}} ->
         join_token(Tail, Key, Val, 
            R#ring{tokens = bst:insert(Shard, {I, Addr, {Key, Val}}, R#ring.tokens)}
         );

      %% slot allocated but new key has higher priority 
      {Shard, {X, Y, _}} when X =/= 0, Y > Addr ->
         join_token(Tail, Key, Val, 
            R#ring{tokens = bst:insert(Shard, {I, Addr, {Key, Val}}, R#ring.tokens)}
         );

      %% slot is allocated, previous key has priority
      _ ->
         join_token(Tail, Key, Val, R)
   end;

join_token([], _Key, _Val, #ring{}=R) ->
   R.

%%
%% leave node from ring
-spec(leave/2 :: (key() | addr(), #ring{}) -> #ring{}).

leave(Addr, #ring{}=R)
 when is_integer(Addr) ->
   case lists:keytake(Addr, 1, R#ring.keys) of
      false ->
         R;
      {value, {_, {Key, _}}, Keys} ->
         leave_token(hashes(R#ring.q, Key, R), R#ring{keys=Keys})
   end;

leave(Key, #ring{}=R) ->
   leave(address(Key, R), R).

leave_token([{_, Hash}|Tail], #ring{}=R) ->
   {_, Key, _} = whereis(address({hash, Hash}, R), R),
   leave_token(Tail, R#ring{tokens=bst:insert(Key, undefined, R#ring.tokens)});

leave_token([], #ring{}=R) ->
   %% all existed nodes has to be re-joined
   %% unless there is smarter way to trace token/hash to previous allocation
   lists:foldl(
      fun({Addr, {Key, Val}}, Acc) ->
         join(Addr, Key, Val, Acc)
      end,
      R,
      R#ring.keys
   ).

%%%------------------------------------------------------------------
%%%
%%% token ring
%%%
%%%------------------------------------------------------------------   

% %%
% token_join(Addr, Node, #ring{size=0, keys=Master, tokens=Shards}=R) ->
%    %% @todo:
%    %%  * initial node allocation shall claim only it own tokens
%    %%  * predecessor / successors shall take into account undefined node and return previous value
%    %%  * list is not optimal for high number of shards > 100
%    R#ring{
%       size   = 1,
%       keys = [{Addr, Node} | Master],
%       tokens = [{X, Node} || {X, _} <- Shards] 
%    };

% token_join(Addr, Node, #ring{size=S, q=Q, keys=Master, tokens=Shards}=R) ->
%    %T = tokens([Node | members(R)], R), %% give priority of shard to new node
%    %% keep priority of shard to old node
%    T = tokens(Q, members(R) ++ [Node], R), 
%    L = lists:map(
%       fun({X, N}) ->
%          case lists:keyfind(X, 1, T) of
%             false   -> {X, N};
%             {_, NN} -> {X, NN}
%          end
%       end,
%       Shards
%    ),
%    R#ring{
%       size   = S + 1,
%       keys = lists:keystore(Node, 2, Master, {Addr, Node}),
%       tokens = L
%    }.

 
% token_leave(Node, #ring{size=S, q=Q, keys=Master, tokens=Shards}=R) ->
%    NShards = lists:filter(fun({_, N}) -> N =/= Node end, Shards),
%    case tokens(2 * Q, lists:usort([X || {_, X} <- NShards]), R) of
%       [] ->
%          reset(R);
%       T  ->
%          % @todo: this is a quick fix to eliminate ring failure
%          {_, Fallback} = hd(T),
%          L = lists:map(
%             fun
%             ({X, N}) when N =:= Node ->
%                case lists:keyfind(X, 1, T) of
%                   false   -> {X, Fallback};
%                   {_, NN} -> {X, NN}
%                end;
%             ({_, _}=X) -> X
%             end,
%             Shards
%          ),
%          R#ring{
%             size   = S - 1,
%             keys = lists:keydelete(Node, 2, Master),
%             tokens = L
%          }
%    end.

% %%
% %% return list of N tokens for each node (ordered by token weight)
% tokens(N, Nodes, Ring) ->
%    lists:flatten(
%       lists:map(
%          fun(X) ->
%             lists:map(
%                fun(Node) ->
%                   % {Shard,    _} = whereis(address(hash(X, Node), Ring), Ring),
%                   % {Shard, Node}
%                   ok
%                end,
%                Nodes
%             )
%          end,
%          lists:seq(1, N)
%       )
%    ).


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
   }.

%%
%% return value associated with token
value({Addr, {_, _, {Key, Val}}}) ->
   {Addr, Key, Val};
value({Addr, undefined}) ->
   {Addr, undefined, undefined}.


%%
%% accumulate N nodes to list 
%% (throw list out when N node collected)
accumulate(_, _, undefined, Acc) ->
   Acc;
accumulate(N, Addr, {_, _, {Key, Val}}, Acc) 
 when length(Acc) < N  ->
   [{Addr, Key, Val}|Acc];
accumulate(_, Addr, {_, _, {Key, Val}}, Acc) ->
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


