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
-module(m_SUITE).
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
-export([maybe_do/1, maybe_none/1]).
-export([xor_do/1, xor_fail/1]).
-export([io_do/1]).
-export([state_do/1, state_get/1, state_put/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, id}
     ,{group, maybe}
     ,{group, 'xor'}
     ,{group, io}
     ,{group, state}
   ].

groups() ->
   [
      {id, [parallel], 
         [id_do]}

     ,{maybe,  [parallel], 
         [maybe_do, maybe_none]}

     ,{'xor',  [parallel], 
         [xor_do, xor_fail]}

     ,{io, [parallel], 
         [io_do]}

     ,{state, [parallel], 
         [state_do, state_put, state_get]}

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
   111 = do_M(m_id).

%%
%%
maybe_do(_Config) ->
   111 = do_M(m_maybe).

%%
%%
maybe_none(_Config) ->
   undefined = do_M_fail(m_maybe).


%%
%%
xor_do(_Config) ->
   {ok, 111} = do_M(m_xor).

%%
%%
xor_fail(_Config) ->
   {error, 1} = do_M_fail(m_xor).



%%
%%
io_do(_Config) ->
   "a:b:c:d" = ( io_do_m() )().
   
io_do_m() ->
   do([m_io ||
      A =< "a",
      B <- io_req(A, "b"),
      C <- io_req(B, "c"),
      D <- io_req(C, "d"),
      return(D)
   ]).

io_req(State, X) ->
   do([m_id ||
      A <- io_struct(X),
      io_action(State, A)
   ]).

io_struct(X) ->
   X.

io_action(State, X) ->
   fun() ->
      State ++ ":" ++ X
   end. 

%%
%%
state_do(_Config) ->
   [111|{0}] = ( do_M(m_state) )({0}).

%%
%%
state_put(_Config) ->
   [3|{3}] = (state_put_m())({0}).


state_put_m() ->
   do([m_state || 
      A =< 1,
      B =< 2,
      _ /= put(lens:t1(), A + B),
      return(A + B)
   ]).

%%
%%
state_get(_Config) ->
   [3|{2}] = (state_get_m())({2}).

state_get_m() ->
   do([m_state ||
      A =< 1,
      B /= get(lens:t1()),
      return(A + B)
   ]).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% successful computation 
do_M(Mtype) ->
   do([Mtype ||
      A <- value(Mtype),
      B <- return(10),
      C =< 1,
      return(A + B + C)
   ]).

value(Mtype) -> 
   Mtype:return(100).

%%
%% failed computation
do_M_fail(Mtype) ->
   do([Mtype ||
      A >= 1,
      B =< 2,
      return(A, B)
   ]).




