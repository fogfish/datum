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
%%   Erlang Monad Library
-module('M').

%%
%% identity
-export(['id.return'/1, 'id.fail'/1, 'id.>>='/2]).

%%
%% error
-export(['error.return'/1, 'error.fail'/1, 'error.>>='/2]).

%%
%% IO 
-export(['io.return'/1, 'io.fail'/1, 'io.>>='/2]).


%%%------------------------------------------------------------------
%%%
%%% Identity monad
%%%
%%%------------------------------------------------------------------

'id.return'(X) ->
   X. 


'id.fail'(X) ->
   exit(X).


'id.>>='(X, Fun) ->
   Fun(X).


%%%------------------------------------------------------------------
%%%
%%% Error monad
%%%
%%%------------------------------------------------------------------

'error.return'(ok) -> 
   ok;
'error.return'(X)  -> 
   {ok, X}.


'error.fail'(X) ->
   {error, X}.


'error.>>='({ok, X}, Fun) ->
   Fun(X);
'error.>>='(ok, Fun) ->
   Fun(ok);
'error.>>='({error, _} = Error, _) ->
   Error.


%%%------------------------------------------------------------------
%%%
%%% IO monad
%%%
%%%------------------------------------------------------------------

'io.return'(X) ->
   fun(_) -> X end.


'io.fail'(X) ->
   exit(X).


'io.>>='(IO1, Fun) ->
   fun(World) ->
      % IO is a function with side-effect that takes World as argument.
      X = IO1(World),      

      % Fun is next chained computation, it takes the result as argument.
      % It either produces next IO operation or scalar result
      case Fun( X ) of
         IO2 when is_function(IO2) ->
            IO2(World);
         Y ->
            Y
      end
   end.

