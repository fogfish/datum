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
   syntax_identity/1,
   syntax_option/1,
   syntax_either/1,
   syntax_pattern/1,
   laws_identity/1,
   laws_option/1,
   laws_either/1,
   laws_pattern/1,
   seq_identity/1,
   seq_option/1,
   seq_either/1
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
     ,{group, sequence}
   ].

groups() ->
   [
      {syntax, [parallel], [
         syntax_identity,
         syntax_option,
         syntax_either,
         syntax_pattern
      ]}

     ,{laws, [parallel], [
         laws_identity,
         laws_option,
         laws_either,
         laws_pattern
      ]}

     ,{sequence, [parallel], [
         seq_identity,
         seq_option,
         seq_either
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
%%% unit(s) 
%%%
%%%----------------------------------------------------------------------------   

-define(cat_compose(Type),
   [Type ||
      fmap(1),
      fmap(2 + _),
      fmap(3 + _)   
   ] 
).

-define(cat_fail(Type), 
   [Type ||
      fmap(1),
      fail(2 + _),
      fmap(3 + _)
   ]
).

-define(cat_arrow(Type),
   [Type ||
      A <- fmap(1),
           fmap(2 + A),
      B <- fmap(3 + _),
      C <- fmap(2),
           fmap(A * B * C)
   ]
).

-define(cat_partial(Type), 
   [Type || 
      fmap(_),
      fmap(2 + _),
      fmap(3 + _)
   ] 
).

syntax_identity(_) ->
   6 = ?cat_compose(identity),
   3 = (catch ?cat_fail(identity)),
   12 = ?cat_arrow(identity),
   6 = (?cat_partial(identity))(1).

syntax_option(_) ->
   6 = ?cat_compose(option),
   undefined = ?cat_fail(option),
   12 = ?cat_arrow(option),
   6 = (?cat_partial(option))(1).

syntax_either(_) ->
   {ok, 6} = ?cat_compose(either),
   {error, 3} = ?cat_fail(either),
   {ok, 12} = ?cat_arrow(either),
   {ok, 6} = (?cat_partial(either))(1).

syntax_pattern(_) ->
   {ok, 6} = (?cat_compose(pattern))(undefined),
   {error, 3} = (?cat_fail(pattern))(undefined),
   {ok, 12} = (?cat_arrow(pattern))(undefined),
   {ok, 6} = ((?cat_partial(pattern))(1))(undefined).


%%
%% Category laws
%%  1. left identity
%%  2. right identity
%%  3. associativity law
%%

-define(cat_laws_lid(Type),
   [Type ||
      fmap(_),  %% identity function
      fmap(1 + _)
   ]
).

-define(cat_laws_rid(Type),
   [Type ||
      fmap(1 + _),
      fmap(_)   %% identity function
   ]
).

-define(cat_laws_associativity_1(Type), 
   [Type ||
      [Type || fmap(_), fmap(2 + _)],
      fmap(3 + _)
   ]
).

-define(cat_laws_associativity_2(Type), 
   [Type ||
      fmap(_),
      [Type || fmap(2 + _), fmap(3 + _)]
   ]
).


laws_identity(_) ->
   2 = (?cat_laws_lid(identity))(1),
   2 = (?cat_laws_rid(identity))(1),
   6 = (?cat_laws_associativity_1(identity))(1),
   6 = (?cat_laws_associativity_2(identity))(1).

laws_option(_) ->
   2 = (?cat_laws_lid(option))(1),
   2 = (?cat_laws_rid(option))(1),
   6 = (?cat_laws_associativity_1(option))(1),
   6 = (?cat_laws_associativity_2(option))(1).

laws_either(_) ->
   {ok, 2} = (?cat_laws_lid(either))(1),
   {ok, 2} = (?cat_laws_rid(either))(1),
   {ok, 6} = (?cat_laws_associativity_1(either))(1),
   {ok, 6} = (?cat_laws_associativity_2(either))(1).

laws_pattern(_) ->
   {ok, 2} = ((?cat_laws_lid(pattern))(1))(undefined),
   {ok, 2} = ((?cat_laws_rid(pattern))(1))(undefined).
   % Associativity do not work for nested patterns.
   % {ok, 6} = ((?cat_laws_associativity_1(pattern))(1))(undefined),
   % {ok, 6} = ((?cat_laws_associativity_2(pattern))(1))(undefined).


%%
%%
seq_identity(_) ->
   [1, 2, 3] = [identity ||
      category:sequence([1, 2, 3]),
      fmap(_)
   ].

seq_option(_) ->
   [1, 2, 3] = [option ||
      category:sequence([1, 2, 3]),
      fmap(_)
   ],

   undefined = [option ||
      category:sequence([1, undefined, 3]),
      fmap(_)
   ].


seq_either(_Config) ->
   {ok, [1, 2, 3]} = [either ||
      category:sequence([{ok, 1}, {ok, 2}, {ok, 3}]),
      fmap(_)
   ],

   {error, badarg} = [either ||
      category:sequence([{ok, 1}, {error, badarg}, {ok, 3}]),
      fmap(_)
   ].
