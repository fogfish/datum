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
   add/2,
   has/2
]).
-compile(export_all).

%%
%% scalable bloom filter
-record(sbf, {
   r    :: integer(),
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
new(B) ->
   new(B, 0.001).

new(B, P) ->
   new(B, P, 0.85).

new(B, P, R) ->
   #sbf{r = R, size = 0, list = [bf_new(B, P)]}.

%%
%% add element 
add(E, #sbf{r = R, size = Size, list = [H | T]} = State) ->
   case has(E, State) of
      true  ->
         State;
      false ->
         case bf_add(E, H) of
            %% filter overflow
            #bf{n = N, size = N} = F ->
               State#sbf{size = Size + 1, list = [bf_scale(R, F), F | T]};
      
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

%% B - approximate filter size in kilobytes 
%% P - desired error probability
bf_new(B, P) ->
   K = bf_k(P),
   M = bf_m(B, K),
   #bf{
      p    = P,
      k    = K,
      m    = M,
      n    = bf_n(M * K, P),
      size = 0,
      bits = [bits_new(M) || _ <- lists:seq(1, K)]
   }.

%% number of hash functions with 50% fill rate (optimal rate)
bf_k(P) ->
   1 + erlang:trunc(log2(1 / P)).

%% number of bits per slice m = M / k aligned to x^2
bf_m(B, K) ->
   1 bsl (1 + erlang:trunc(log2(B * 8 * 1024 / K))).

%% number of elements n ≈ M * (ln 2)^2 / |ln P|
bf_n(M, P) ->
   1 + erlang:trunc(M * math:pow(math:log(2), 2) / erlang:abs(math:log(P))).

%%
%% insert element to set
bf_add(E, #bf{m = M, k = K, size = Size, bits = Bits0} = State) ->
   Hash = hashes(E, K),
   {Bool, Bits} = lists:unzip([bits_set(H rem M, B) || {H, B} <- lists:zip(Hash, Bits0)]),
   case lists:all(fun(X) -> not X end, Bool) of
      true  ->
         State;
      false ->
         State#bf{size = Size + 1, bits = Bits}
   end.

%%
%% lookup element membership
bf_has(E, #bf{m = M, k = K, bits = Bits0}) ->
   Hash = hashes(E, K),
   Bool = [bits_get(H rem M, B) || {H, B} <- lists:zip(Hash, Bits0)],
   lists:all(fun(X) -> X end, Bool).


%% The SBF starts with one filter with k0 slices and error probability P0.
%% When this filter gets full, a new one is added with k1 slices and 
%% P1 = P0 * r error probability, where r is the tightening ratio with 0 < r < 1.
%% k0 = log2 P0 ^ −1
%% ki = log2 Pi ^ −1
bf_scale(R, #bf{p = P, k = K, m = M}) ->
   B = erlang:trunc((K * M) / (8 * 1024)),
   bf_new(B, P * R).

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

hashes(X, K) ->
   hashes(?HASH1(X), ?HASH2(X), K).
hashes(_, _, 0) ->
   [];
hashes(A, B, K) ->
   [ (A band (K * B)) | hashes(A, B, K - 1)].

