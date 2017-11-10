%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
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
%%   category pattern test suite
-module(category_SUITE).
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

   syntax_option_expr/1,
   syntax_option_unit/1,
   syntax_option_fail/1,
   syntax_option_state/1,
   syntax_option_transformer/1,
   syntax_option_partial/1,

   syntax_either_expr/1,
   syntax_either_unit/1,
   syntax_either_fail/1,
   syntax_either_state/1,
   syntax_either_transformer/1,
   syntax_either_partial/1,

   syntax_reader_expr/1,
   syntax_reader_unit/1,
   syntax_reader_fail/1,
   syntax_reader_state/1,
   syntax_reader_transformer/1,
   syntax_reader_partial/1,

   syntax_kleisli_expr/1,
   syntax_kleisli_unit/1,
   syntax_kleisli_fail/1,
   syntax_kleisli_state/1,
   syntax_kleisli_transformer/1,
   syntax_kleisli_partial/1,

   laws_identity_left_identity/1,
   laws_identity_right_identity/1,
   laws_identity_associativity_1/1,
   laws_identity_associativity_2/1,

   laws_option_left_identity/1,
   laws_option_right_identity/1,
   laws_option_associativity_1/1,
   laws_option_associativity_2/1,

   laws_either_left_identity/1,
   laws_either_right_identity/1,
   laws_either_associativity_1/1,
   laws_either_associativity_2/1,

   laws_reader_left_identity/1,
   laws_reader_right_identity/1,
   laws_reader_associativity_1/1,
   laws_reader_associativity_2/1,

   laws_kleisli_left_identity/1,
   laws_kleisli_right_identity/1,
   laws_kleisli_associativity_1/1,
   laws_kleisli_associativity_2/1,

   transformer_seq_option/1,
   transformer_seq_either/1,
   transformer_seq_reader/1,

   transformer_cat_option/1,
   transformer_cat_either/1,
   transformer_cat_reader/1,

   transformer_flatten_either/1,
   transformer_flatten_reader/1
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, syntax}
     ,{group, laws}
     ,{group, transformers}
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

         syntax_option_expr,
         syntax_option_unit,
         syntax_option_fail,
         syntax_option_state,
         syntax_option_transformer,
         syntax_option_partial,

         syntax_either_expr,
         syntax_either_unit,
         syntax_either_fail,
         syntax_either_state,
         syntax_either_transformer,
         syntax_either_partial,

         syntax_reader_expr,
         syntax_reader_unit,
         syntax_reader_fail,
         syntax_reader_state,
         syntax_reader_transformer,
         syntax_reader_partial,

         syntax_kleisli_expr,
         syntax_kleisli_unit
      ]}

     ,{laws, [parallel], [
         laws_identity_left_identity,
         laws_identity_right_identity,
         laws_identity_associativity_1,
         laws_identity_associativity_2,

         laws_option_left_identity,
         laws_option_right_identity,
         laws_option_associativity_1,
         laws_option_associativity_2,

         laws_either_left_identity,
         laws_either_right_identity,
         laws_either_associativity_1,
         laws_either_associativity_2,

         laws_reader_left_identity,
         laws_reader_right_identity,
         laws_reader_associativity_1,
         laws_reader_associativity_2,

         laws_kleisli_left_identity,
         laws_kleisli_right_identity,
         laws_kleisli_associativity_1,
         laws_kleisli_associativity_2
      ]}

     ,{transformers, [parallel], [
         transformer_seq_option,
         transformer_seq_either,
         transformer_seq_reader,

         transformer_cat_option,
         transformer_cat_either,
         transformer_cat_reader,

         transformer_flatten_either,
         transformer_flatten_reader
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
%%% unit(s): syntax 
%%%
%%%----------------------------------------------------------------------------   

a(identity, X) -> X;
a(option,   X) -> X;
a(either,   X) -> {ok, X};
a(reader,   X) -> {ok, X};
a(m_identity,   X) -> X.

b(identity, X) -> X + 2;
b(option,   X) -> X + 2;
b(either,   X) -> {ok, X + 2};
b(reader,   X) -> {ok, X + 2};
b(m_identity,   X) -> X + 2.

c(identity, X) -> X + 3;
c(option,   X) -> X + 3;
c(either,   X) -> {ok, X + 3};
c(reader,   X) -> {ok, X + 3};
c(m_identity,   X) -> X + 3.

d(identity, X, Y, Z) -> X * Y * Z;
d(option,   X, Y, Z) -> X * Y * Z;
d(either,   X, Y, Z) -> {ok, X * Y * Z};
d(reader,   X, Y, Z) -> {ok, X * Y * Z};
d(m_identity,   X, Y, Z) -> X * Y * Z.

t(identity, X) -> X + 2;
t(option,   X) -> X + 2;
t(either,   X) -> {ok, X + 2};
t(m_identity,   X) -> X + 2.

t(reader,   X, Y) -> {ok, X + Y}.


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
      A =< a(identity, 1),
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
   6 = ?cat_compose_expr(identity).

syntax_identity_unit(_) ->
   6 = ?cat_compose_unit(identity).

syntax_identity_fail(_) ->
   3 = (catch ?cat_compose_fail(identity)).

syntax_identity_state(_) ->
   12 = ?cat_compose_state(identity).

syntax_identity_transformer(_) ->
   24 = ?cat_compose_transformer(identity).

syntax_identity_partial(_) ->
   6 = (?cat_compose_partial(identity))(1).

%%
syntax_option_expr(_) ->
   6 = ?cat_compose_expr(option).

syntax_option_unit(_) ->
   6 = ?cat_compose_unit(option).

syntax_option_fail(_) ->
   undefined = ?cat_compose_fail(option).

syntax_option_state(_) ->
   12 = ?cat_compose_state(option).

syntax_option_transformer(_) ->
   24 = ?cat_compose_transformer(option).

syntax_option_partial(_) ->
   6 = (?cat_compose_partial(option))(1).


%%
syntax_either_expr(_) ->
   {ok, 6} = ?cat_compose_expr(either).

syntax_either_unit(_) ->
   {ok, 6} = ?cat_compose_unit(either).

syntax_either_fail(_) ->
   {error, 3} = ?cat_compose_fail(either).

syntax_either_state(_) ->
   {ok, 12} = ?cat_compose_state(either).

syntax_either_transformer(_) ->
   {ok, 24} = ?cat_compose_transformer(either).

syntax_either_partial(_) ->
   {ok, 6} = (?cat_compose_partial(either))(1).

%%
syntax_reader_expr(_) ->
   {ok, 6} = (?cat_compose_expr(reader))(2).

syntax_reader_unit(_) ->
   {ok, 6} = (?cat_compose_unit(reader))(2).

syntax_reader_fail(_) ->
   {error, 3} = (?cat_compose_fail(reader))(2).

syntax_reader_state(_) ->
   {ok, 12} = (?cat_compose_state(reader))(2).

syntax_reader_transformer(_) ->
   {ok, 24} = (?cat_compose_transformer(reader))(2).

syntax_reader_partial(_) ->
   {ok, 6} = ((?cat_compose_partial(reader))(1))(2).


%%
syntax_kleisli_expr(_) ->
   6 = ?cat_compose_expr(m_identity).

syntax_kleisli_unit(_) ->
   6 = ?cat_compose_unit(m_identity).

syntax_kleisli_fail(_) ->
   3 = (catch ?cat_compose_fail(m_identity)).

syntax_kleisli_state(_) ->
   12 = ?cat_compose_state(m_identity).

syntax_kleisli_transformer(_) ->
   24 = ?cat_compose_transformer(m_identity).

syntax_kleisli_partial(_) ->
   6 = (?cat_compose_partial(m_identity))(1).

%%%----------------------------------------------------------------------------   
%%%
%%% unit(s): category laws
%%%
%%%----------------------------------------------------------------------------   

%%
%% Category laws
%%  1. left identity
%%  2. right identity
%%  3. associativity law
%%

-define(cat_laws_left_identity(Type),
   [Type ||
      unit(_), b(Type, _)
   ]
).

-define(cat_laws_right_identity(Type),
   [Type ||
      b(Type, _), unit(_)
   ]
).

-define(cat_laws_associativity_1(Type), 
   [Type ||
      [Type || a(Type, _), b(Type, _)],
      c(Type, _)
   ]
).

-define(cat_laws_associativity_2(Type), 
   [Type ||
      a(Type, _),
      [Type || b(Type, _), c(Type, _)]
   ]
).

%%
laws_identity_left_identity(_) ->
   3 = (?cat_laws_left_identity(identity))(1).

laws_identity_right_identity(_) ->
   3 = (?cat_laws_right_identity(identity))(1).

laws_identity_associativity_1(_) ->
   6 = (?cat_laws_associativity_1(identity))(1).

laws_identity_associativity_2(_) ->
   6 = (?cat_laws_associativity_2(identity))(1).

%%
laws_option_left_identity(_) ->
   3 = (?cat_laws_left_identity(option))(1).

laws_option_right_identity(_) ->
   3 = (?cat_laws_right_identity(option))(1).

laws_option_associativity_1(_) ->
   6 = (?cat_laws_associativity_1(option))(1).

laws_option_associativity_2(_) ->
   6 = (?cat_laws_associativity_2(option))(1).


%%
laws_either_left_identity(_) ->
   {ok, 3} = (?cat_laws_left_identity(either))(1).

laws_either_right_identity(_) ->
   {ok, 3} = (?cat_laws_right_identity(either))(1).

laws_either_associativity_1(_) ->
   {ok, 6} = (?cat_laws_associativity_1(either))(1).

laws_either_associativity_2(_) ->
   {ok, 6} = (?cat_laws_associativity_2(either))(1).


%%
laws_reader_left_identity(_) ->
   {ok, 3} = ((?cat_laws_left_identity(reader))(1))(1).

laws_reader_right_identity(_) ->
   {ok, 3} = ((?cat_laws_right_identity(reader))(1))(1).

laws_reader_associativity_1(_) ->
   % associativity is not hold for reader
   % {ok, 6} = ((?cat_laws_associativity_1(reader))(1))(1).
   ok.

laws_reader_associativity_2(_) ->
   % associativity is not hold for reader
   % {ok, 6} = ((?cat_laws_associativity_2(either))(1))(1).
   ok.


%%
laws_kleisli_left_identity(_) ->
   3 = (?cat_laws_left_identity(m_identity))(1).

laws_kleisli_right_identity(_) ->
   3 = (?cat_laws_right_identity(m_identity))(1).

laws_kleisli_associativity_1(_) ->
   6 = (?cat_laws_associativity_1(m_identity))(1).

laws_kleisli_associativity_2(_) ->
   6 = (?cat_laws_associativity_2(m_identity))(1).


%%%----------------------------------------------------------------------------   
%%%
%%% unit(s): transformers
%%%
%%%----------------------------------------------------------------------------   

%%
transformer_seq_option(_) ->
   [1, 2, 3] = [option ||
      cats:sequence([1, 2, 3]),
      unit(_)
   ],

   undefined = [option ||
      cats:sequence([1, undefined, 3]),
      unit(_)
   ].

transformer_seq_either(_) ->
   {ok, [1, 2, 3]} = [either ||
      cats:sequence([{ok, 1}, {ok, 2}, {ok, 3}]),
      unit(_)
   ],

   {error, badarg} = [either ||
      cats:sequence([{ok, 1}, {error, badarg}, {ok, 3}]),
      unit(_)
   ].

transformer_seq_reader(_) ->
   {ok, [1, 2, 3]} = ([reader ||
      cats:sequence([{ok, 1}, {ok, 2}, {ok, 3}]),
      unit(_)
   ])(1),

   {error, badarg} = ([reader ||
      cats:sequence([{ok, 1}, {error, badarg}, {ok, 3}]),
      unit(_)
   ])(1).

%%
transformer_cat_option(_) ->
   1 = [option ||
      cats:eitherT({ok, 1}),
      unit(_)
   ],

   undefined = [option ||
      cats:eitherT({error, badarg}),
      unit(_)
   ].

transformer_cat_either(_) ->
   {ok, 1} = [either ||
      cats:optionT(badarg, 1),
      unit(_)
   ],

   {error, badarg} = [either ||
      cats:optionT(badarg, undefined),
      unit(_)
   ].

transformer_cat_reader(_) ->
   {ok, 1} = ([reader ||
      cats:optionT(badarg, 1),
      unit(_)
   ])(1),

   {error, badarg} = ([reader ||
      cats:optionT(badarg, undefined),
      unit(_)
   ])(1).


%%
transformer_flatten_either(_) ->
   {ok, 1} = [either ||
      unit({ok, {ok, {ok, 1}}}),
      cats:flatten(_)
   ],

   {error, badarg} = [either ||
      unit({ok, {ok, {error, badarg}}}),
      cats:flatten(_)
   ].

transformer_flatten_reader(_) ->
   {ok, 1} = ([reader ||
      unit({ok, {ok, {ok, 1}}}),
      cats:flatten(_)
   ])(1),

   {error, badarg} = ([reader ||
      unit({ok, {ok, {error, badarg}}}),
      cats:flatten(_)
   ])(1).

