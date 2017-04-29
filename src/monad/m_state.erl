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
%%   state monad 
-module(m_state).

-export([return/1, yield/1, fail/1, '>>='/2]).
-export([put/2, get/1]).

-type m(A)    :: fun((_) -> [A|_]).
-type f(A, B) :: fun((A) -> m(B)).

%%
%%
-spec return(A) -> m(A).

return(X) ->
   fun(State) -> [X|State] end.

%%
%%
-spec yield(A) -> m(A).

yield([_|_]=X) ->
   fun(State) -> [erlang:list_to_tuple(X)|State] end;
yield(X) ->
   fun(State) -> [X|State] end.


%%
%%
-spec fail(_) -> _.

fail(X) ->
   exit(X).


%%
%%
-spec '>>='(m(A), f(A, B)) -> m(B).

'>>='(X, Fun) ->
   join(fmap(Fun, X)).

%%
%%
-spec join( m(m(A)) ) -> m(A).

join(IO) ->
   fun(State) -> 
      [Fun|Y] = IO(State),
      Fun(Y)
   end.

%%
%%
-spec fmap(fun((A) -> B), m(A)) -> m(B).

fmap(Fun, IO) ->
   fun(State) ->
      [A|Y]=IO(State),
      [Fun(A)|Y]
   end.

%%
%%
get(Ln) ->
   fun(State) ->
      [lens:get(Ln, State)|State]
   end.

%%
%%
put(Ln, X) ->
   fun(State) ->
      [X|lens:put(Ln, X, State)]
   end.
