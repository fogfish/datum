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
%%   heap ordered tree - each element at node is no lager then elements at its children
-module(heap).

-include("datum.hrl").

-export([
   new/0
  % ,new/1

  ,head/1     %% O(1)
  ,tail/1     %% O(log n)
  ,insert/2   %% O(log n)

  ,is_empty/1 %% O(1)
  ,list/1     
]).

% %%
% %% internal state
% -record(heap, {

% }).

%%
%% create new empty heap
new() ->
   ?NULL.


%%
%%
head({_, X, _, _}) ->
   X;

head(_) ->
   exit(badarg).

%%
%%
tail({_, _, A, B}) ->
   merge(A, B);

tail(_) ->
   exit(badarg).

%%
%%
insert(E, Heap) ->
   merge({1, E, ?NULL, ?NULL}, Heap).

%%
%%
is_empty(?NULL) ->
   true;

is_empty(_) ->
   false.

%%
%% return list of elements
list(?NULL) -> [];
list(Heap)  -> list(Heap, []).

list(?NULL, Acc) ->
   lists:reverse(Acc);
list(Heap,  Acc) ->
   list(tail(Heap), [head(Heap)|Acc]).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% merge two heap keeping leftist property. 
merge(X, ?NULL) ->
   X;

merge(?NULL, X) ->
   X;

merge({_, X, A, B}, {_, Y, _, _}=H)
 when X =< Y ->
   make(X, A, merge(B, H));

merge(H, {_, Y, A, B}) ->
   make(Y, A, merge(H, B)).

%%
%%
rank(?NULL) ->
   0;
rank({R, _, _, _}) ->
   R.

%%
%%
make(X, A, B) ->
   case rank(A) >= rank(B) of
      true  ->
         {rank(B) + 1, X, A, B};
      false ->
         {rank(A) + 1, X, B, A}
   end.




