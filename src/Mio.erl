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
-module('Mio').

-export([return/1, fail/1, '>>='/2]).


return(X) ->
   fun(_) -> X end.


fail(X) ->
   exit(X).


'>>='(IO1, Fun) ->
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

