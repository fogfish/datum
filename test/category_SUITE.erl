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
-export([all/0]).

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

   syntax_undefined_expr/1,
   % syntax_undefined_unit/1,
   syntax_undefined_fail/1,
   syntax_undefined_state/1,
   syntax_undefined_transformer/1,
   syntax_undefined_partial/1,

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
   syntax_kleisli_list/1,

   laws_identity_left_identity/1,
   laws_identity_right_identity/1,
   laws_identity_associativity_1/1,
   laws_identity_associativity_2/1,

   laws_option_left_identity/1,
   laws_option_right_identity/1,
   laws_option_associativity_1/1,
   laws_option_associativity_2/1,

   laws_undefined_left_identity/1,
   laws_undefined_right_identity/1,
   laws_undefined_associativity_1/1,
   laws_undefined_associativity_2/1,

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

   transformer_identity_unit/1,
   transformer_identity_fail/1,
   transformer_identity_require/1,
   transformer_identity_sequence/1,
   transformer_identity_flatten/1,
   transformer_identity_option/1,
   transformer_identity_either/1,
   transformer_identity_try/1,

   transformer_option_unit/1,
   transformer_option_fail/1,
   transformer_option_require/1,
   transformer_option_sequence/1,
   transformer_option_flatten/1,
   transformer_option_option/1,
   transformer_option_either/1,
   transformer_option_try/1,

   transformer_undefined_unit/1,
   transformer_undefined_fail/1,
   transformer_undefined_require/1,
   transformer_undefined_sequence/1,
   transformer_undefined_flatten/1,
   transformer_undefined_option/1,
   transformer_undefined_either/1,
   transformer_undefined_try/1,

   transformer_either_unit/1,
   transformer_either_fail/1,
   transformer_either_require/1,
   transformer_either_sequence/1,
   transformer_either_flatten/1,
   transformer_either_option/1,
   transformer_either_either/1,
   transformer_either_try/1,

   transformer_reader_unit/1,
   transformer_reader_fail/1,
   transformer_reader_require/1,
   transformer_reader_sequence/1,
   transformer_reader_flatten/1,
   transformer_reader_option/1,
   transformer_reader_either/1,
   transformer_reader_try/1,

   transformer_kleisli_io/1
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit(s): syntax 
%%%
%%%----------------------------------------------------------------------------   

a(identity, X) -> X;
a(option,   X) -> X;
a(undefined,_) -> undefined;
a(either,   X) -> {ok, X};
a(reader,   X) -> {ok, X};
a(m_identity,   X) -> X.

b(identity, X) -> X + 2;
b(option,   X) -> X + 2;
b(undefined,_) -> undefined;
b(either,   X) -> {ok, X + 2};
b(reader,   X) -> {ok, X + 2};
b(m_identity,   X) -> X + 2.

c(identity, X) -> X + 3;
c(option,   X) -> X + 3;
c(undefined,_) -> undefined;
c(either,   X) -> {ok, X + 3};
c(reader,   X) -> {ok, X + 3};
c(m_identity,   X) -> X + 3.

d(identity, X, Y, Z) -> X * Y * Z;
d(option,   X, Y, Z) -> X * Y * Z;
d(undefined,_, _, _) -> undefined;
d(either,   X, Y, Z) -> {ok, X * Y * Z};
d(reader,   X, Y, Z) -> {ok, X * Y * Z};
d(m_identity,   X, Y, Z) -> X * Y * Z.

t(identity, X) -> X + 2;
t(option,   X) -> X + 2;
t(undefined,_) -> undefined;
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

-define(cat_compose_unit_with_case(Type),
   [Type ||
      A =< case 1 of 1 -> 1 end,
      B =< case 1 of 1 -> 3 end,
      cats:unit(A + B)
   ]
).

%% eq error
-define(cat_compose_fail(Type), 
   [Type ||
      a(Type, 1),
      fail(3),
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
   6 = (fun() -> ?cat_compose_unit(identity) end)(),
   4 = (fun() -> ?cat_compose_unit_with_case(identity) end)().

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
   6 = (fun() -> ?cat_compose_unit(option) end)(),
   4 = (fun() -> ?cat_compose_unit_with_case(option) end)().

syntax_option_fail(_) ->
   undefined = ?cat_compose_fail(option).

syntax_option_state(_) ->
   12 = ?cat_compose_state(option).

syntax_option_transformer(_) ->
   24 = ?cat_compose_transformer(option).

syntax_option_partial(_) ->
   6 = (?cat_compose_partial(option))(1).

%%
syntax_undefined_expr(_) ->
   undefined = ?cat_compose_expr(undefined).

% syntax_undefined_unit(_) ->
%    undefined = ?cat_compose_unit(undefined).
%    undefined = ?cat_compose_unit_with_case(undefined).

syntax_undefined_fail(_) ->
   3 = ?cat_compose_fail(undefined).

syntax_undefined_state(_) ->
   undefined = ?cat_compose_state(undefined).

syntax_undefined_transformer(_) ->
   undefined = ?cat_compose_transformer(undefined).

syntax_undefined_partial(_) ->
   undefined = (?cat_compose_partial(undefined))(1).


%%
syntax_either_expr(_) ->
   {ok, 6} = ?cat_compose_expr(either).

syntax_either_unit(_) ->
   {ok, 6} = (fun() -> ?cat_compose_unit(either) end)(),
   {ok, 4} = (fun() -> ?cat_compose_unit_with_case(either) end)().

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
   {ok, 6} = (?cat_compose_unit(reader))(2),
   {ok, 4} = (?cat_compose_unit_with_case(reader))(2).

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
   6 = ?cat_compose_unit(m_identity),
   4 = ?cat_compose_unit_with_case(m_identity).

syntax_kleisli_fail(_) ->
   3 = (catch ?cat_compose_fail(m_identity)).

syntax_kleisli_state(_) ->
   12 = ?cat_compose_state(m_identity).

syntax_kleisli_transformer(_) ->
   24 = ?cat_compose_transformer(m_identity).

syntax_kleisli_partial(_) ->
   6 = (?cat_compose_partial(m_identity))(1).

syntax_kleisli_list(_) ->
   Tail  = "c",
   "abc" = [m_identity ||
      A =< "ab" ++ Tail,
      cats:unit(A) 
   ].


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
laws_undefined_left_identity(_) ->
   undefined = (?cat_laws_left_identity(undefined))(1).

laws_undefined_right_identity(_) ->
   undefined = (?cat_laws_right_identity(undefined))(1).

laws_undefined_associativity_1(_) ->
   undefined = (?cat_laws_associativity_1(undefined))(1).

laws_undefined_associativity_2(_) ->
   undefined = (?cat_laws_associativity_2(undefined))(1).


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

-define(unitT(Type), 
   [Type ||
      cats:unit(1), cats:unit(_)
   ]
).

-define(failT(Type), 
   [Type ||
      cats:fail(1), cats:unit(_)
   ]
).

-define(requireT(Type),
   [Type ||
      A <- cats:unit(_), cats:require(A > 0, A, nil)
   ]
).

-define(seqT(Type),
   [Type ||
      cats:sequence(_), cats:unit(_) 
   ]
).

-define(flattenT(Type),
   [Type ||
      A <- cats:unit(1),
      cats:unit([Type || cats:unit(A), cats:unit(_)]),
      cats:flatten(_)
   ]
).

-define(optionT(Type, X),
   [Type ||
      cats:optionT(X),
      cats:unit(_)
   ]
).

-define(eitherT(Type, X),
   [Type ||
      cats:eitherT(X),
      cats:unit(_)
   ]
).

-define(tryT(Type, X), 
   [Type ||
      cats:tryT(X),
      cats:unit(_)
   ]
).

zero() -> 0.

%%
%%
transformer_identity_unit(_) ->
   1 = ?unitT(identity).

transformer_identity_fail(_) ->
   1 = (catch ?failT(identity)).

transformer_identity_require(_) ->
   1 = (?requireT(identity))(1),
   nil = (catch (?requireT(identity))(0)).

transformer_identity_sequence(_) ->
   [1, 2, 3] = (?seqT(identity))([1, 2, 3]).

transformer_identity_flatten(_) ->
   1 = ?flattenT(identity).

transformer_identity_option(_) ->
   1 = ?optionT(identity, 1),
   undefined = ?optionT(identity, undefined).

transformer_identity_either(_) ->
   1 = ?eitherT(identity, {ok, 1}),
   undefined = ?eitherT(identity, {error, badarg}).

transformer_identity_try(_) ->
   1 = ?tryT(identity, 1),
   a = ?tryT(identity, throw(a)),
   ok = try ?tryT(identity, exit(badarg)) catch _:badarg -> ok end,
   ok = try ?tryT(identity, 1 / zero()) catch _:badarith -> ok end.

%%
%%
transformer_option_unit(_) ->
   1 = ?unitT(option).

transformer_option_fail(_) ->
   undefined = ?failT(option).

transformer_option_require(_) ->
   1 = (?requireT(option))(1),
   undefined = (?requireT(option))(0).

transformer_option_sequence(_) ->
   [1, 2, 3] = (?seqT(option))([1, 2, 3]),
   undefined = (?seqT(option))([1, undefined, 3]).

transformer_option_flatten(_) ->
   1 = ?flattenT(option).

transformer_option_option(_) ->
   1 = ?optionT(option, 1),
   undefined = ?optionT(option, undefined).

transformer_option_either(_) ->
   1 = ?eitherT(option, {ok, 1}),
   undefined = ?eitherT(option, {error, badarg}).

transformer_option_try(_) ->
   1 = ?tryT(option, 1),
   undefined = ?tryT(option, undefined),
   a = ?tryT(option, throw(a)),
   undefined = ?tryT(option, exit(badarg)),
   undefined = ?tryT(option, 1 / zero()).



%%
%%
transformer_undefined_unit(_) ->
   undefined = ?unitT(undefined).

transformer_undefined_fail(_) ->
   1 = ?failT(undefined).

transformer_undefined_require(_) ->
   undefined = (?requireT(undefined))(1),
   undefined = (?requireT(undefined))(0).

transformer_undefined_sequence(_) ->
   [1, 2, 3] = (?seqT(undefined))([1, 2, 3]),
   undefined = (?seqT(undefined))([1, undefined, 3]).


transformer_undefined_flatten(_) ->
   undefined = ?flattenT(undefined).

transformer_undefined_option(_) ->
   1 = ?optionT(undefined, 1),
   undefined = ?optionT(undefined, undefined).

transformer_undefined_either(_) ->
   1 = ?eitherT(undefined, {ok, 1}),
   undefined = ?eitherT(undefined, {error, badarg}).

transformer_undefined_try(_) ->
   1 = ?tryT(undefined, 1),
   undefined = ?tryT(undefined, undefined),
   a = ?tryT(undefined, throw(a)),
   undefined = ?tryT(undefined, exit(badarg)),
   undefined = ?tryT(undefined, 1 / zero()).


%%
%%
transformer_either_unit(_) ->
   {ok, 1} = ?unitT(either).

transformer_either_fail(_) ->
   {error, 1} = ?failT(either).

transformer_either_require(_) ->
   {ok, 1} = (?requireT(either))(1),
   {error, nil} = (?requireT(either))(0).

transformer_either_sequence(_) ->
   {ok, [1, 2, 3]} = (?seqT(either))([{ok, 1}, {ok, 2}, {ok, 3}]),
   {error, badarg} = (?seqT(either))([{ok, 1}, {error, badarg}, {ok, 3}]).

transformer_either_flatten(_) ->
   {ok, 1} = ?flattenT(either),
   {ok, 1} = [either ||
      cats:unit({ok, {ok, 1}}),
      cats:flatten(_)
   ],
   {error, 1} = [either ||
      cats:unit({ok, {error, 1}}),
      cats:flatten(_)
   ],
   {error, 1} = [either ||
      cats:unit({error, {error, 1}}),
      cats:flatten(_)
   ].

transformer_either_option(_) ->
   {ok, 1} = ?optionT(either, 1),
   {error, undefined} = ?optionT(either, undefined),
   {error, badarg} = [either ||
      cats:optionT(badarg, undefined),
      unit(_)
   ].

transformer_either_either(_) ->
   {ok, 1} = ?eitherT(either, {ok, 1}),
   {error, badarg} = ?eitherT(either, {error, badarg}).

transformer_either_try(_) ->
   {ok, 1} = ?tryT(either, {ok, 1}),
   {error, badarg} = ?tryT(either, {error, badarg}),
   {ok, a} = ?tryT(either, throw(a)),
   {error, badarg} = ?tryT(either, exit(badarg)),
   {error, badarith} = ?tryT(either, 1 / zero()).


%%
%%
transformer_reader_unit(_) ->
   {ok, 1} = (?unitT(reader))(#{}).

transformer_reader_fail(_) ->
   {error, 1} = (?failT(reader))(#{}).

transformer_reader_require(_) ->
   {ok, 1} = ((?requireT(reader))(1))(#{}),
   {error, nil} = ((?requireT(reader))(0))(#{}).

transformer_reader_sequence(_) ->
   {ok, [1, 2, 3]} = ((?seqT(reader))([{ok, 1}, {ok, 2}, {ok, 3}]))(#{}),
   {error, badarg} = ((?seqT(reader))([{ok, 1}, {error, badarg}, {ok, 3}]))(#{}).

transformer_reader_flatten(_) ->
   % {ok, 1} = (?flattenT(reader))(#{}).
   {ok, 1} = ([reader ||
      cats:unit({ok, {ok, 1}}),
      cats:flatten(_)
   ])(#{}),
   {error, 1} = ([reader ||
      cats:unit({ok, {error, 1}}),
      cats:flatten(_)
   ])(#{}),
   {error, 1} = ([reader ||
      cats:unit({error, {error, 1}}),
      cats:flatten(_)
   ])(#{}).

transformer_reader_option(_) ->
   {ok, 1} = (?optionT(reader, 1))(#{}),
   {error, undefined} = (?optionT(reader, undefined))(#{}),
   {error, badarg} = ([reader ||
      cats:optionT(badarg, undefined),
      unit(_)
   ])(#{}).

transformer_reader_either(_) ->
   {ok, 1} = (?eitherT(reader, {ok, 1}))(#{}),
   {error, badarg} = (?eitherT(reader, {error, badarg}))(#{}).

transformer_reader_try(_) ->
   {ok, 1} = (?tryT(reader, {ok, 1}))(#{}),
   {error, badarg} = (?tryT(reader, {error, badarg}))(#{}),
   {ok, a} = (?tryT(reader, throw(a)))(#{}),
   {error, badarg} = (?tryT(reader, exit(badarg)))(#{}),
   {error, badarith} = (?tryT(reader, 1 / zero()))(#{}).


%%
transformer_kleisli_io(_) ->
   1 = [m_identity ||
      _ > 1,
      _ < 1
   ].

