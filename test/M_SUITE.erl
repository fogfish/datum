%%
%%   Copyright (c) 2015, Dmitry Kolesnikov
%%   All Rights Reserved.
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
%%
-module('M_SUITE').
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, monad}).

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

-export([id_do/1]).
-export([error_do/1, error_fail/1]).
-export([io_do/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, id}
     ,{group, error}
     ,{group, io}
   ].

groups() ->
   [
      {id, [parallel], 
         [id_do]}

     ,{error,  [parallel], 
         [error_do, error_fail]}

     ,{io, [parallel], 
         [io_do]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% unit(s) 
%%%
%%%----------------------------------------------------------------------------   

%%
%%
id_do(_Config) ->
   111 = id_do_m().

id_do_m() ->
   [{'M', id} ||
      A <- id_do_100(),
      B <- return(10),
      C =< 1,
      return(A + B + C)
   ].

id_do_100() -> 
   100.


%%
%%
error_do(_Config) ->
   {ok, 111} = error_do_m().

error_do_m() ->
   [{'M', error} ||
      A <- error_do_100(),
      B <- return(10),
      C =< 1,
      return(A + B + C)
   ].

error_do_100() -> 
   {ok, 100}.


%%
%%
error_fail(_Config) ->
   {error, 1} = error_fail_m().

error_fail_m() ->
   [{'M', error} ||
      A >= 1,
      B =< 2,
      return(A, B)
   ].


%%
%%
io_do(_Config) ->
   Fun = io_do_m(),
   "a-b-c-d" = Fun("-"),
   "a:b:c:d" = Fun(":").
   
io_do_m() ->
   [{'M', io} ||
      A =< "a",
      B <- io_req(A, "b"),
      C <- io_req(B, "c"),
      D <- io_req(C, "d"),
      D
   ].

io_req(State, X) ->
   [{'M', id} ||
      A <- io_struct(X),
      io_action(State, A)
   ].

io_struct(X) ->
   X.

io_action(State, X) ->
   fun(Y) ->
      State ++ Y ++ X
   end. 









