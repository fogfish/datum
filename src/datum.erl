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
   typeof/1
]).

%%
%% data types
-type(q()      :: {q, integer(), list(), list()}).
-type(stream() :: {s, any(), function()}).
-type(tree()   :: {t, any()}).
-type(heap()   :: {h, integer(), any()}).
-type(lens()   :: {l, function(), function(), function()}).

-type(ring()   :: tuple()).

-export_type([
   q/0
  ,stream/0
  ,tree/0
  ,heap/0
  ,ring/0
  ,lens/0
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
type(l) -> lens;
type(_) -> undefined.




