%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
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
%% @doc
%%   identity monad
-module(m_identity).

-export([unit/1, fail/1, '>>='/2, put/1, get/1]).

-type m(A)    :: A.
-type f(A, B) :: fun((A) -> m(B)).

%%
%%
-spec unit(A) -> m(A).

unit(X) ->
   X. 

%%
%%
-spec fail(_) -> _.

fail(X) ->
   throw(X).

%%
%%
-spec '>>='(m(A), f(A, B)) -> m(B).

'>>='(X, Fun) ->
   Fun(X).

%%
%%
-spec put(_) -> m(_).

put(X) -> 
   X.

%%
%%
-spec get(_) -> m(_).

get(X) -> 
   X.
