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
-compile({parse_transform, category}).

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

-export([
   syntax_identity_expr/1,
   syntax_identity_unit/1,
   syntax_identity_fail/1,
   syntax_identity_state/1,
   syntax_identity_transformer/1,
   syntax_identity_partial/1,

   syntax_io_expr/1,
   syntax_io_unit/1,
   syntax_io_fail/1,
   syntax_io_state/1,
   syntax_io_transformer/1,
   syntax_io_partial/1,

   syntax_state_expr/1,
   syntax_state_unit/1,
   syntax_state_fail/1,
   syntax_state_state/1,
   syntax_state_transformer/1,
   syntax_state_partial/1,
   syntax_state_lenses/1
]).



%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, syntax}
   ].

groups() ->
   [
      {syntax, [parallel], [
         syntax_identity_expr,
         syntax_identity_unit,
         syntax_identity_fail,
         syntax_identity_state,
         syntax_identity_transformer,
         syntax_identity_partial,

         syntax_io_expr,
         syntax_io_unit,
         syntax_io_fail,
         syntax_io_state,
         syntax_io_transformer,
         syntax_io_partial,

         syntax_state_expr,
         syntax_state_unit,
         syntax_state_fail,
         syntax_state_state,
         syntax_state_transformer,
         syntax_state_partial,
         syntax_state_lenses
      ]}
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
%%% unit(s) : syntax  
%%%
%%%----------------------------------------------------------------------------   

x(X) -> X.

a(m_identity,   X) -> X;
a(m_io,         X) -> fun( ) -> X end;
a(m_state,      X) -> fun(S) -> [X|S] end.

b(m_identity,   X) -> X + 2;
b(m_io,         X) -> fun( ) -> X + 2 end;
b(m_state,      X) -> fun(S) -> [X + 2|S] end.

c(m_identity,   X) -> X + 3;
c(m_io,         X) -> fun( ) -> X + 3 end;
c(m_state,      X) -> fun(S) -> [X + 3|S] end.

d(m_identity,   X, Y, Z) -> X * Y * Z;
d(m_io,         X, Y, Z) -> fun( ) -> X * Y * Z end;
d(m_state,      X, Y, Z) -> fun(S) -> [X * Y * Z|S] end.

t(m_identity,   X) -> X + 2;
t(m_io,         X) -> fun( ) -> X + 2 end;
t(m_state,      X) -> fun(S) -> [X + 2|S] end.


%% eq 6.
-define(cat_compose_expr(Type),
   [Type ||
      a(Type, 1),  %% 1
      b(Type, _),  %% 3
      c(Type, _)   %% 6
   ]
).

%% eq 6.
-define(cat_compose_unit(Type),
   [Type ||
      A =< x(1),
      unit(A + 0), %% 1
      unit(_ + 2), %% 3
      unit(_ + 3)  %% 6  
   ]
).

%% eq error
-define(cat_compose_fail(Type),
   [Type ||
      a(Type, 1),
      fail(_ + 2),
      c(Type, _)
   ]
).

%% eq 12.
-define(cat_compose_state(Type),
   [Type ||
      A <- a(Type, 1),      %% 1
           b(Type, A),      
      B <- c(Type, _),      %% 6
      C <- a(Type, 2),      %% 2
           d(Type, A, B, C) %% 12
   ]
).

%% eq 24.
-define(cat_compose_transformer(Type),
   [Type ||
      A <- a(Type, 1),      %% 1
           b(Type, A),
      B <- c(Type, _),      %% 6
           a(Type, 2),      %% 2
           cats:unit(_),    
      C /= t(Type, _),      %% 4
           d(Type, A, B, C) %% 24   
   ]
).

-define(cat_compose_partial(Type),
   [Type || 
      a(Type, _),
      b(Type, _),
      c(Type, _)
   ]
).



%%
syntax_identity_expr(_) ->
   6 = ?cat_compose_expr(m_identity).

syntax_identity_unit(_) ->
   6 = ?cat_compose_unit(m_identity).

syntax_identity_fail(_) ->
   3 = (catch ?cat_compose_fail(m_identity)).

syntax_identity_state(_) ->
   12 = ?cat_compose_state(m_identity).

syntax_identity_transformer(_) ->
   24 = ?cat_compose_transformer(m_identity).

syntax_identity_partial(_) ->
   6 = (?cat_compose_partial(m_identity))(1).


%%
syntax_io_expr(_) ->
   6 = (?cat_compose_expr(m_io))().

syntax_io_unit(_) ->
   6 = (?cat_compose_unit(m_io))().

syntax_io_fail(_) ->
   3 = (catch (?cat_compose_fail(m_io))()).

syntax_io_state(_) ->
   12 = (?cat_compose_state(m_io))().

syntax_io_transformer(_) ->
   24 = (?cat_compose_transformer(m_io))().

syntax_io_partial(_) ->
   6 = ((?cat_compose_partial(m_io))(1))().


%%
syntax_state_expr(_) ->
   [6|state] = (?cat_compose_expr(m_state))(state).

syntax_state_unit(_) ->
   [6|state] = (?cat_compose_unit(m_state))(state).

syntax_state_fail(_) ->
   3 = (catch (?cat_compose_fail(m_state))(state)).

syntax_state_state(_) ->
   [12|state] = (?cat_compose_state(m_state))(state).

syntax_state_transformer(_) ->
   [24|state] = (?cat_compose_transformer(m_state))(state).

syntax_state_partial(_) ->
   [6|state] = ((?cat_compose_partial(m_state))(1))(state).


syntax_state_lenses(_) ->
   Expr = [m_state ||
      X /= cats:get(lens:at(a)),
      unit(X + 10),
      cats:put(lens:at(b), _)
   ],

   [11|#{a := 1, b := 11}] = Expr(#{a => 1, b => 0}).

