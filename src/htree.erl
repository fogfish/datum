%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%%   hash tree - functional data structure for large set reconciliation 
-module(htree).

-export([
   new/0
  ,build/1
  ,insert/3
  ,lookup/2
  ,remove/2
  ,foldl/3
  ,foldr/3
  ,hash/1
  ,hash/2
  ,evict/2
  ,diff/2
  ,list/1
]).


%%
%% hash tree configuration
-define(HASH(X),   crypto:hash(sha, X)).  %% hash function
-define(BITS,      160).                  %% width of hash function
-define(NULL,      nil).                  %% empty node

%%
%% width and node capacity impacts on tree performance
%% smaller capacity faster reconciliation due to excessive node eviction  
%% but higher demand on memory consumption
-ifndef(CONFIG_HTREE_CAPACITY).
-define(CONFIG_HTREE_CAPACITY,  4).
-endif.

-ifndef(CONFIG_HTREE_WIDTH).
-define(CONFIG_HTREE_WIDTH,     2).
-endif.

%%
%% data types
-type(leaf()     :: [{key(), val()}]).   %% leaf node container
-type(inner()    :: [tree()]).           %% tree node container   
-type(tree()     :: {n, hash(), integer(), inner() | leaf()} | ?NULL).
-type(hash()     :: binary()).
-type(key()      :: any()).
-type(val()      :: any()).
-type(sign()     :: {hash, integer(), [hash()]}).

%% tree nodes
-record(n, {hash, uid = 0, nodes = []}).

%%
%% create new hash tree
-spec(new/0 :: () -> datum:tree()).

new()  ->
   {t, ?NULL}.

%%
%% build tree from data type
-spec(build/1 :: ([{key(), val()}]) -> datum:tree()).

build(List) ->
   lists:foldl(
      fun({Key, Val}, Acc) -> insert(Key, Val, Acc) end, 
      new(), 
      List
   ).

%%
%% insert element to hash tree
-spec(insert/3 :: (key(), val(), datum:tree()) -> datum:tree()).

insert(K, V, {t, T}) ->
   {_, Tx} = ht_insert(K, V, T),
   {t, Tx}.

ht_insert(K, V, ?NULL) ->
   ht_insert(K, V, #n{});
ht_insert(K, V, T) ->
   ht_insert(1, fhash(K), K, V, T).

ht_insert(_, H, K, V, ?NULL) ->
   %% insert new leaf node
   {H, {K, V}};
ht_insert(_, _, K, V, {K,_}) ->
   %% update existing leaf node
   {undefined, {K, V}};
ht_insert(L, H, K, V, #n{hash = Hash, nodes = Nodes}=T) ->
   {value, N, NN} = ht_select(uid(L, H), K, Nodes),   %% peek next child on path
   {Hx, Nx} = ht_insert(L + 1, H, K, V, N),           %% insert key/val
   {Hx, ht_split(L, T#n{hash = hadd(Hx, Hash), nodes = [Nx | NN]})}.

%%
%% select node on path
ht_select(_, _, []) ->
   {value, ?NULL, []};
ht_select(I, _, [#n{  } | _]=NN) ->
   case lists:keytake(I, #n.uid, NN) of
      false ->
         {value, #n{uid=I}, NN};
      Value ->
         Value
   end;
ht_select(_, K, [{_, _} | _]=NN) ->
   case lists:keytake(K, 1, NN) of
      false ->
         {value, ?NULL, NN};
      Value ->
         Value
   end.

%%
%% split leaf node
ht_split(L, #n{nodes = [{_, _} | _]=Nodes}=T)
 when length(Nodes) > ?CONFIG_HTREE_CAPACITY ->
   X = lists:foldr(
      fun({K, V}, Acc) ->
         H = fhash(K),
         I = uid(L, H),
         {value, N, NN} = case lists:keytake(I, #n.uid, Acc) of
            false ->
               {value, #n{uid=I}, Acc};
            Value ->
               Value
         end,
         {_,   Nx} = ht_insert(L + 1, H, K, V, N), 
         [Nx | NN]
      end,
      [],
      Nodes
   ),
   T#n{nodes = X};
ht_split(_, T) ->
   T.


%%
%% lookup element
-spec(lookup/2 :: (key(), datum:tree()) -> val() | undefined).

lookup(K, {t, T}) ->
   ht_lookup(K, T).

ht_lookup(K, T) ->
   ht_lookup(1, fhash(K), K, T).

ht_lookup(_, _, _, ?NULL) ->
   undefined;
ht_lookup(_, _, K, {K,V}) ->
   V;
ht_lookup(L, H, K, #n{nodes = Nodes}) ->
   {value, N, _} = ht_select(uid(L, H), K, Nodes),
   ht_lookup(L + 1, H, K, N).

%%
%% remove element
-spec(remove/2 :: (key(), datum:tree()) -> datum:tree()).

remove(K, {t, T}) ->
   {_, Tx} = ht_remove(K, T),
   {t, Tx}.

ht_remove(K, T) ->
   ht_remove(1, fhash(K), K, T).

ht_remove(_, _, _, ?NULL) ->
   {undefined, ?NULL};
ht_remove(_, H, K, {K,_}) ->
   {H, ?NULL};
ht_remove(L, H, K, #n{hash = Hash, nodes = Nodes}=T) ->
   {value, N, NN} = ht_select(uid(L, H), K, Nodes),   %% peek next child on path
   case {ht_remove(L + 1, H, K, N), NN} of
      {{Hx, ?NULL}, []} ->
         {Hx, ?NULL};
      {{Hx, ?NULL},  _} ->
         {Hx, T#n{hash = hsub(Hx, Hash), nodes = NN}};
      {{Hx,    Nx},  _} ->
         {Hx, T#n{hash = hsub(Hx, Hash), nodes = [Nx | NN]}}
   end.

%%
%% fold function over tree 
-spec(foldl/3 :: (function(), any(), datum:tree()) -> any()).

foldl(Fun, Acc, {t, T}) ->
   ht_foldl(Fun, Acc, T).

ht_foldl(_Fun, Acc0, ?NULL) ->
   Acc0;
ht_foldl(Fun, Acc0,  {K,V}) ->
   Fun(K, V, Acc0);
ht_foldl(Fun, Acc0, #n{nodes = Nodes}) ->
   lists:foldl(fun(X, Acc) -> ht_foldl(Fun, Acc, X) end, Acc0, Nodes).

%%
%% fold function over tree 
-spec(foldr/3 :: (function(), any(), datum:tree()) -> any()).

foldr(Fun, Acc, {t, T}) ->
   ht_foldr(Fun, Acc, T).

ht_foldr(_Fun, Acc0, ?NULL) ->
   Acc0;
ht_foldr(Fun, Acc0,  {K,V}) ->
   Fun(K, V, Acc0);
ht_foldr(Fun, Acc0, #n{nodes = Nodes}) ->
   lists:foldr(fun(X, Acc) -> ht_foldr(Fun, Acc, X) end, Acc0, Nodes).

%%
%% return list of signatures at level 
-spec(hash/1 :: (datum:tree()) -> sign()).
-spec(hash/2 :: (integer(), datum:tree()) -> sign() | undefined).

hash(T) ->
   {hash, -1, foldl(fun(X, _, Acc) -> gb_sets:add(fhash(X), Acc) end, gb_sets:new(), T)}.

hash(L, {t, T}) ->
   Hashes = ht_hash(L, T),
   case gb_sets:is_empty(Hashes) of
      true  ->
         undefined;
      false ->
         {hash, L, Hashes}
   end.

ht_hash(_, ?NULL) ->
   gb_sets:new();
ht_hash(L, T) ->
   ht_hash(L, gb_sets:new(), T).
ht_hash(0, Acc,  #n{hash  = Hash}) ->
   gb_sets:add(Hash, Acc);
ht_hash(L, Acc0, #n{nodes = Nodes}) ->
   lists:foldl(fun(X, Acc) -> ht_hash(L - 1, Acc, X) end, Acc0, Nodes);
ht_hash(_, Acc0, _) ->
   Acc0.

%%
%% evict subtrees that matches a signature
-spec(evict/2 :: (sign(), datum:tree()) -> datum:tree()).

evict({hash, L, Hashes}, {t, T}) ->
   {t, ht_evict(L, Hashes, T)}.

ht_evict(_,_Hashes, ?NULL) ->
   ?NULL;
ht_evict(0, Hashes, #n{hash = Hash}=T) ->
   case gb_sets:is_member(Hash, Hashes) of
      true  ->
         ?NULL;
      false ->
         T
   end;
ht_evict(L, Hashes, {K, _}=T)
 when L < 0 ->
   case gb_sets:is_member(fhash(K), Hashes) of
      true  ->
         ?NULL;
      false ->
         T
   end;
ht_evict(_,_Hashes, {_, _}=T) ->
   T;
ht_evict(L, Hashes, #n{nodes = Nodes}=T) ->
   case ht_evict_bits(L, Hashes, Nodes) of
      [] -> 
         ?NULL;
      X  ->
         T#n{nodes = X}
   end.

ht_evict_bits(L, Hashes, Nodes) ->
   lists:foldl(
      fun(Node0, Acc) ->
         case ht_evict(L - 1, Hashes, Node0) of
            ?NULL ->
               Acc;
            Node  ->
               [Node | Acc]
         end
      end,
      [],
      Nodes
   ).


%%
%% calculate difference of signature or tree
-spec(diff/2 :: (sign() | datum:tree(), sign() | datum:tree()) -> sign() | datum:tree()).

diff({hash, LA, HA}, {hash, LB, HB})
 when LA =:= LB ->
   {hash, LA, gb_sets:intersection(HA, HB)};
diff({hash, LA,  _}, {hash, LB,  _}) ->
   {hash, erlang:min(LA, LB), []};
diff({t, _}=A, {t, _}=B) ->
   ht_diff(0, A, B).

ht_diff(_, {t, nil}=A, B) ->
   {A, B};
ht_diff(_, A, {t, nil}=B) ->
   {A, B};
ht_diff(L, A, B) ->
   case {hash(L, A), hash(L, B)} of
      %% bottom of tree is reached, evict leaves
      {undefined, _} ->
         I = diff(hash(A), hash(B)),
         {evict(I, A), evict(I, B)};
      %% bottom of tree is reached, evict leaves
      {_, undefined} ->
         I = diff(hash(A), hash(B)),
         {evict(I, A), evict(I, B)};
      {HA, HB} ->
         I = diff(HA, HB),
         ht_diff(L + 1, evict(I, A), evict(I, B))
   end.

%%
%%
list(T) ->
   foldr(fun(K, V, Acc) -> [{K,V}|Acc] end, [], T).


%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

%%
%% calculate node identity (offset) at level L
uid(0, _) -> 0;
uid(L, Hash) ->
   Skip = (L - 1) * ?CONFIG_HTREE_WIDTH,
   <<_:Skip, Val:?CONFIG_HTREE_WIDTH, _/bitstring>> = Hash,
   Val. 

%%
%% hash function
fhash(X)
 when is_binary(X) ->
   ?HASH(X);
fhash(X) ->
   ?HASH(erlang:term_to_binary(X)).

%%
%% hash add
hadd(undefined, Y) ->
   Y;
hadd(X, undefined) ->
   X;
hadd(X, Y) ->
   <<A:?BITS>> = X,
   <<B:?BITS>> = Y,
   <<(A bxor B):?BITS>>.

%%
%% hash subtract
hsub(X, Y) ->
   hadd(X, Y).




