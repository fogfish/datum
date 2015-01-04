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
%%   hash tree (merkle tree)
-module(htree).

-export([
   new/0
  ,build/1
  ,insert/2
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
-type(tree()     :: {l | n, binary(), integer(), [tree() | element()]} | ?NULL).
-type(hash()     :: binary()).
-type(element()  :: any()).
-type(sign()     :: {hash, integer(), [hash()]}).

%% tree nodes
-record(l, {hash, uid = 0, leafs = []}).
-record(n, {hash, uid = 0, nodes = []}).

%%
%% create new hash tree
-spec(new/0 :: () -> datum:tree()).

new()  ->
   {t, ?NULL}.

%%
%% build tree from data type
-spec(build/1 :: ([element()]) -> datum:tree()).

build(List) ->
   lists:foldl(fun insert/2, new(), List).

%%
%% insert element to hash tree
-spec(insert/2 :: (element(), datum:tree()) -> datum:tree()).

insert(E, {t, T}) ->
   case ht_insert(h(E), E, T) of
      false ->
         {t, T};
      X ->
         {t, X}
   end.

ht_insert(H, E, T) ->
   ht_insert(1, H, E, T).

ht_insert(_, H, E, ?NULL) ->
   #l{hash = H, leafs = [E]};
ht_insert(L, H, E, #l{hash = Hash, leafs = Leafs}=T) ->
   case lists:member(E, Leafs) of
      false ->
         ht_split(L, T#l{hash = hadd(H, Hash), leafs = [E|Leafs]});
      true  ->
         false
   end;
ht_insert(L, H, E, #n{hash = Hash, nodes = Nodes}=T) ->
   {N, NN} = ht_select(L, H, Nodes),
   case ht_insert(L + 1, H, E, N) of
      false ->
         false;
      Node  ->
         T#n{hash = hadd(H, Hash), nodes = [Node | NN]}
   end.


ht_split(L, #l{uid = Uid, leafs = Leafs})
 when length(Leafs) > ?CONFIG_HTREE_CAPACITY ->
   lists:foldr(
      fun(K, Acc) -> ht_insert(L, h(K), K, Acc) end,
      #n{uid = Uid},
      Leafs
   );
ht_split(_, T) ->
   T.

%%
%% select and create node
ht_select(L, Hash, Nodes) ->
   I = uid(L, Hash),
   case lists:keytake(I, #n.uid, Nodes) of
      {value, N, NN} -> 
         {N, NN};
      false          ->
         {#l{uid=I}, Nodes}
   end.

%%
%% lookup element
-spec(lookup/2 :: (element(), datum:tree()) -> hash() | undefined).

lookup(E, {t, T}) ->
   ht_lookup(h(E), E, T).

ht_lookup(H, E, T) ->
   ht_lookup(1, H, E, T).

ht_lookup(_, H, E, #l{leafs = Leafs}) ->
   case lists:member(E, Leafs) of
      false ->
         undefined;
      true  ->
         H
   end;
ht_lookup(L, H, E, #n{nodes = Nodes}) ->
   I = uid(L, H),
   case lists:keyfind(I, #n.uid, Nodes) of
      false ->
         undefined;
      Node  ->
         ht_lookup(L + 1, H, E, Node)
   end.

%%
%% remove element
-spec(remove/2 :: (element(), datum:tree()) -> datum:tree()).

remove(E, {t, T}) ->
   case ht_remove(h(E), E, T) of
      false ->
         {t, T};
      X     ->
         {t, X}
   end.

ht_remove(H, E, T) ->
   ht_remove(1, H, E, T).

ht_remove(_, _, _, #l{leafs = []}) ->
   ?NULL;
ht_remove(_, _, E, #l{leafs = [E]}) ->
   ?NULL;
ht_remove(_, H, E, #l{hash = Hash, leafs = Leafs}=T) ->
   case lists:member(E, Leafs) of
      true  ->
         T#l{hash = hsub(H, Hash), leafs = lists:delete(E, Leafs)};
      false ->
         false
   end;
ht_remove(L, H, E, #n{nodes = Nodes}=T) ->
   {N, NN} = ht_select(L, H, Nodes),
   case ht_remove(L + 1, H, E, N) of
      false ->
         false;
      Node  ->
         ht_join(Node, NN, H, T)
   end.

ht_join(?NULL, [], _H, _T) ->
   ?NULL;
ht_join(?NULL, Nodes, H, #n{hash = Hash}=T) ->
   T#n{hash = hsub(H, Hash), nodes = Nodes};
ht_join(Node, Nodes,  H, #n{hash = Hash}=T) ->
   T#n{hash = hsub(H, Hash), nodes = [Node | Nodes]}.

%%
%% fold function over tree 
-spec(foldl/3 :: (function(), any(), datum:tree()) -> any()).

foldl(Fun, Acc, {t, T}) ->
   ht_foldl(Fun, Acc, T).

ht_foldl(_Fun, Acc0, ?NULL) ->
   Acc0;
ht_foldl(Fun, Acc0, #l{leafs = Leafs}) ->
   lists:foldl(Fun, Acc0, Leafs);
ht_foldl(Fun, Acc0, #n{nodes = Nodes}) ->
   lists:foldl(fun(X, Acc) -> ht_foldl(Fun, Acc, X) end, Acc0, Nodes).


%%
%% fold function over tree 
-spec(foldr/3 :: (function(), any(), datum:tree()) -> any()).

foldr(Fun, Acc, {t, T}) ->
   ht_foldr(Fun, Acc, T).

ht_foldr(_Fun, Acc0, ?NULL) ->
   Acc0;
ht_foldr(Fun, Acc0, #l{leafs = Leafs}) ->
   lists:foldr(Fun, Acc0, Leafs);
ht_foldr(Fun, Acc0, #n{nodes = Nodes}) ->
   lists:foldr(fun(X, Acc) -> ht_foldr(Fun, Acc, X) end, Acc0, Nodes).

%%
%% return list of element signatures (intermediate signatures) 
-spec(hash/1 :: (datum:tree()) -> sign()).
-spec(hash/2 :: (integer(), datum:tree()) -> sign() | undefined).

hash(T) ->
   {hash, -1, foldl(fun(X, Acc) -> gb_sets:add(h(X), Acc) end, gb_sets:new(), T)}.

hash(L, {t, T}) ->
   {hash, L, ht_hash(L, T)}.

ht_hash(_, ?NULL) ->
   gb_sets:new();
ht_hash(L, T) ->
   ht_hash(L, gb_sets:new(), T).
ht_hash(0, Acc,  #n{hash  = Hash}) ->
   gb_sets:add(Hash, Acc);
ht_hash(L, Acc0, #n{nodes = Nodes}) ->
   lists:foldl(fun(X, Acc) -> ht_hash(L - 1, Acc, X) end, Acc0, Nodes);
ht_hash(_, Acc0, #l{}) ->
   Acc0.

%%
%% evict subtrees that matches a signature
-spec(evict/2 :: (sign(), datum:tree()) -> {ok | go, datum:tree()}).

evict({hash, L, {0,nil}}, {t, T}) ->
   {ok, {t, T}};
evict({hash, L,  Hashes}, {t, T}) ->
   {go, {t, ht_evict(L, Hashes, T)}}.

ht_evict(0, Hashes, #n{hash = Hash}=T) ->
   case gb_sets:is_member(Hash, Hashes) of
      true  ->
         ?NULL;
      false ->
         T
   end;
ht_evict(L, Hashes, #l{leafs = Leafs}=T)
 when L < 0 ->
   case [E || E <- Leafs, not gb_sets:is_member(h(E), Hashes)] of
      [] ->
         ?NULL;
      X  ->
         T#l{leafs = X}
   end;
ht_evict(_,_Hashes, #l{}=T) ->
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
ht_diff(L, A0, B0) ->
   case evict(hash(L, A0), B0) of
      {ok, B1} ->
         {_,  A2} = evict(hash(B1), A0),
         {_,  B2} = evict(hash(A0), B1),
         {A2, B2};

      {go, B1} ->
         case evict(hash(L, B1), A0) of
            {ok, A1} ->
               {_, A2} = evict(hash(B1), A1),
               {_, B2} = evict(hash(A1), B1),
               {A2, B2};

            {go, A1} ->
               ht_diff(L + 1, A1, B1)
         end
   end.


   % case {hash(L, A), hash(L, B)} of
   %    %% bottom of tree is reached, evict leaves
   %    {undefined, _} ->
   %       I = diff(hash(A), hash(B)),
   %       {evict(I, A), evict(I, B)};
   %    %% bottom of tree is reached, evict leaves
   %    {_, undefined} ->
   %       I = diff(hash(A), hash(B)),
   %       {evict(I, A), evict(I, B)};
   %    {HA, HB} ->
   %       I = diff(HA, HB),
   %       ht_diff(L + 1, evict(I, A), evict(I, B))
   % end.

%%
%%
list(T) ->
   foldr(fun(X, Acc) -> [X|Acc] end, [], T).


%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------

%%
%% calculate node identity (offset) at level L
uid(L, Hash) ->
   Skip = (L - 1) * ?CONFIG_HTREE_WIDTH,
   <<_:Skip, Val:?CONFIG_HTREE_WIDTH, _/bitstring>> = Hash,
   Val. 

%%
%% calculate hash
h(X)
 when is_binary(X) ->
   ?HASH(X);
h(X) ->
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




