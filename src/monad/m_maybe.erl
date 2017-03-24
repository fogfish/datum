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
%%   maybe / optional monad
-module(m_maybe).

-export([return/1, yield/1, fail/1, '>>='/2]).

-type m(A)    :: undefined | A.
-type f(A, B) :: fun((A) -> m(B)).

%%
%%
-spec return(A) -> m(A).

return(X)  -> 
   X.

%%
%%
-spec yield(A) -> m(A).

yield([_|_] = X) ->
   erlang:list_to_tuple(X);
yield(X) ->
   X.

%%
%%
-spec fail(_) -> _.

fail(_) ->
   undefined.

%%
%%
-spec '>>='(m(A), f(A, B)) -> m(B).

'>>='(undefined, _) ->
   undefined;
'>>='(X, Fun) ->
   Fun(X).
