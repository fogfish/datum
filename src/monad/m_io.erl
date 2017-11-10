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
%%   IO monad
-module(m_io).

-export([unit/1, fail/1, '>>='/2]).

-type m(A)    :: fun(( ) -> A).
-type f(A, B) :: fun((A) -> m(B)).

%%
%%
-spec unit(A) -> m(A).

unit(X) ->
   fun() -> X end.

%%
%%
-spec fail(_) -> _.

fail(X) ->
   throw(X).

%%
%%
-spec '>>='(m(A), f(A, B)) -> m(B).

'>>='(IO, Fn) ->
   join(fmap(Fn, IO)).


%%
%%
-spec join( m(m(A)) ) -> m(A).

join(IO) ->
   fun() -> 
      ( IO() )() 
   end.

%%
%%
-spec fmap(fun((A) -> B), m(A)) -> m(B).

fmap(Fun, IO) ->
   fun() -> Fun(IO()) end.
