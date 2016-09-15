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

-export([return/1, fail/1, '>>='/2]).

-type io(A)   :: fun(( ) -> A).
-type f(A, B) :: fun((A) -> io(B)).

%%
%%
-spec return(A) -> io(A).

return(X) ->
   fun() -> X end.

%%
%%
-spec fail(_) -> _.

fail(X) ->
   exit(X).

%%
%%
-spec '>>='(io(A), f(A, B)) -> io(B).

'>>='(IO, Fn) ->
   join(fmap(Fn, IO)).


%%
%%
-spec join(io(io(A))) -> io(A).

join(IO) ->
   fun() -> 
      ( IO() )() 
   end.

%%
%%
-spec fmap(fun((A) -> B), io(A)) -> io(B).

fmap(Fun, IO) ->
   fun() -> Fun(IO()) end.







