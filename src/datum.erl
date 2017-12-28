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
%%   pure functional data structures
-module(datum).

-export([
   typeof/1,
   compare/2
]).

%%
%% types
-type option(X)    :: undefined | X.

-type either(L, R) :: {error, L} | {ok, R}.
-type either()     :: {error, _} | ok.
-type either(R)    :: {error, _} | {ok, R}.
-type either(L, R1, R2) :: {error, L} | {ok, R1, R2}.

-type foldable(T)    :: T.
-type traversable(T) :: T.

-type q()       :: {q, integer(), list(), list()}.
-type stream()  :: {s, _, function()}.
-type tree()    :: {t, _, _}.
-type heap()    :: {h, integer(), _}.
-type ring()    :: tuple().

-type monoid(T)    :: fun((T, T) -> T).
-type predicate(T) :: fun((T) -> true | false).
-type effect(T)    :: fun((T) -> ok).  

-export_type([
   option/1
  ,either/0
  ,either/1
  ,either/2
  ,either/3
  ,foldable/1
  ,monoid/1
  ,q/0
  ,stream/0
  ,tree/0
  ,heap/0
  ,ring/0
]).

%%
%%
-spec typeof(_) -> q | stream | tree | heap | lens | undefined.

typeof(X)
 when is_tuple(X) ->
  type(erlang:element(1, X));

typeof(_) ->
   undefined.

type(q) -> q;
type(s) -> stream;
type(h) -> heap;
type(t) -> tree;
type(_) -> undefined.


%%
%% order functor
-spec compare(_, _) -> eq | gt | lt.

%%
%% default ordering functor
compare(A, B) when A =:= B -> eq;
compare(A, B) when A  >  B -> gt;
compare(A, B) when A  <  B -> lt.



