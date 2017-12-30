%%
%%   Copyright (c) 2012 - 2016, Dmitry Kolesnikov
%%   All Rights Reserved.
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
%%  @doc
%%   scalable bloom filter, based on idea discussed in the paper
%%   http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf
-module(sbf).

-export([
   new/1,
   new/2,
   new/3,
   new/4,
   add/2,
   has/2
]).


%%
%% scalable bloom filter
-record(sbf, {
   r    :: integer(),
   s    :: integer(),
   size :: integer(),
   list :: [_]
}).

%%
%% bloom filter (bf)
%%   partition the M bits on k-slices of size m = M/k bits, 
%%   each slices per hash function.
-record(bf, {
   p    :: float(),
   k    :: integer(),
   m    :: integer(),
   n    :: integer(),
   size :: integer(),
   bits :: [_]
}).

%%
%% create new scalable bloom filter
%%  C - initial capacity 
%%  P - false positive probability
%%  R - tightening ratio of error probability (as defined by paper)
%%  S - growth ratio (as defined by paper)
new(C) ->
   new(C, 0.001).

new(C, P) ->
   new(C, P, 0.85).

new(C, P, R) ->
   new(C, P, R, 1).

new(C, P, R, S) ->
   %% n ≈ -m ln (1 - p)
   P0 = P * (1 - R),
   K  = bf_k(P0),
   Pk = math:pow(P0, 1 / K),
   M  = 1 + trunc(log2(-C / math:log(1 - Pk))), 
   #sbf{r = R, s = S, size = 0, list = [bf_new(M, P0)]}.

%%
%% add element 
add(E, #sbf{r = R, s = S, size = Size, list = [H | T]} = State) ->
   case has(E, State) of
      true  ->
         State;
      false ->
         case bf_add(E, H) of
            %% filter overflow
            #bf{n = N, size = N} = F ->
               State#sbf{size = Size + 1, list = [bf_scale(S, R, F), F | T]};
      
            F ->
               State#sbf{size = Size + 1, list = [F | T]}
         end
   end.

%%
%% check membership
has(E, #sbf{list = List}) ->
   lists:any(fun(X) -> bf_has(E, X) end, List).   


%%%------------------------------------------------------------------
%%%
%%% bloom filter
%%%
%%%------------------------------------------------------------------

%% M - segment modulo 
%% P - desired error probability
bf_new(M, P) ->
   K = bf_k(P),
   Pk= math:pow(P, 1 / K),
   N = trunc(-(1 bsl M) * math:log(1 - Pk)),
   #bf{
      p    = P,
      k    = K,
      m    = M,
      n    = N, 
      size = 0,
      bits = [bits_new(1 bsl M) || _ <- lists:seq(1, K)]
   }.

%% number of hash functions with 50% fill rate (optimal rate)
bf_k(P) ->
   1 + erlang:trunc(log2(1 / P)).

%%
%% insert element to set
bf_add(E, #bf{m = M, k = K, size = Size, bits = Bits0} = State) ->
   Mask = 1 bsl M - 1,
   Hash = hashes(E, Mask, K),
   {Bool, Bits} = lists:unzip([bits_set(H, B) || {H, B} <- lists:zip(Hash, Bits0)]),
   case lists:all(fun(X) -> not X end, Bool) of
      true  ->
         State;
      false ->
         State#bf{size = Size + 1, bits = Bits}
   end.

%%
%% lookup element membership
bf_has(E, #bf{m = M, k = K, bits = Bits0}) ->
   Mask = 1 bsl M - 1,
   Hash = hashes(E, Mask, K),
   Bool = [bits_get(H, B) || {H, B} <- lists:zip(Hash, Bits0)],
   lists:all(fun(X) -> X end, Bool).


%% The SBF starts with one filter with k0 slices and error probability P0.
%% When this filter gets full, a new one is added with k1 slices and 
%% P1 = P0 * r error probability, where r is the tightening ratio with 0 < r < 1.
%% k0 = log2 P0 ^ −1
%% ki = log2 Pi ^ −1
bf_scale(S, R, #bf{p = P, m = M}) ->
   bf_new(M + S, P * R).

%%%------------------------------------------------------------------
%%%
%%% bits set
%%%
%%%------------------------------------------------------------------

-define(WORD,  128). % tuned for space / performance

bits_new(N) ->
   array:new(1 + N div ?WORD, [{default, 0}]).

bits_set(I, Bits) ->
   Cell = I div ?WORD,
   Word = array:get(Cell, Bits),
   case (Word band (1 bsl (I rem ?WORD))) of
      0 ->
         {true,  array:set(Cell, (Word bor (1 bsl (I rem ?WORD))), Bits)};
      _ ->
         {false, Bits}
   end.

bits_get(I, Bits) ->
   Cell = I div ?WORD,
   Word = array:get(Cell, Bits),
   case (Word band (1 bsl (I rem ?WORD))) of
      0 ->
         false;
      _ ->
         true
   end.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
log2(X) -> 
   math:log(X) / math:log(2). 

%%
%% calculates K hashes 
%% double hashing technique is defined at
%% http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
-define(HASH1(X), erlang:phash2([X], 1 bsl 32)).
-define(HASH2(X), erlang:phash2({X}, 1 bsl 32)).

hashes(X, Mask, K) ->
   hashes(?HASH1(X), ?HASH2(X), Mask, K).

hashes(_, _, _, 0) ->
   [];
hashes(A, B, Mask, K) ->
   X = (A + B) band Mask,
   [ X | hashes(A, X, Mask, K - 1) ].
