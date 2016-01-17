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
%%   Well behaved lens satisfies following laws
%%    * GetPut - if we get focused element a() from s() and immediately put a() 
%%               with no modifications back into s(), we must get back exactly s().
%%
%%    * PutGet - if putting a() inside s() yields a new s(), 
%%               then the a() obtained from s is exactly a().
%%
%%    * PutPut - A sequence of two puts is just the effect of the second, 
%%               the first gets completely overwritten. This law is applicable 
%%               to very well behaved lenses.
%%
-module(lens_SUITE).
-include_lib("common_test/include/ct.hrl").

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

%%
%% naive lens interface
-export([naive_type_map/1, naive_type_tuple/1, naive_type_list/1]).

%%
%% pure lens interface
-export([
   hd/1, tl/1, list/1, 
   t1/1, t2/1, t3/1, tuple/1, tuple_pred/1, 
   map/1, map_pred/1
]).

%%
%% compose lenses
-export([c/1, apply2/1, apply3/1, apply4/1, apply5/1, apply6/1, apply7/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, naive}
     ,{group, pure}
     ,{group, compose}
   ].

groups() ->
   [
      {naive, [parallel], 
         [naive_type_map, naive_type_tuple]}

     ,{pure,  [parallel], 
         [hd, tl, list, 
          t1, t2, t3, tuple, tuple_pred,
          map, map_pred]}

     ,{compose, [parallel], 
         [c, apply2, apply3, apply4, apply5, apply6, apply7]}
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
%%% naive lens interface
%%%
%%%----------------------------------------------------------------------------   

%%
-define(naive_lens_laws(Ln, X, Y), 
   begin
      X  = lens:nput(Ln,    lens:nget(Ln, X), X),
      d  = lens:nget(Ln,    lens:nput(Ln, d, X)),
      Y  = lens:nput(Ln, e, lens:nput(Ln, d, X))
   end
).

naive_type_map(_Config) ->
   Ln = lens:naive(b),
   X  = #{a => 1, b => 2, c => 3},
   Y  = #{a => 1, b => e, c => 3},
   ?naive_lens_laws(Ln, X, Y).
   
naive_type_tuple(_Config) ->
   Ln = lens:naive(2),
   X  = {1, 2, 3},
   Y  = {1, e, 3},
   ?naive_lens_laws(Ln, X, Y).

naive_type_list(_Config) ->
   %% @todo: requires fix
   Ln = lens:naive(b),
   X  = [{a, 1}, {b, 2}, {c, 3}],
   Y  = [{a, 1}, {b, e}, {c, 3}],
   ?naive_lens_laws(Ln, X, Y).

%%%----------------------------------------------------------------------------   
%%%
%%% pure lens interface
%%%
%%%----------------------------------------------------------------------------   

%% Ln - lens, X - original data, Y - expected PutPut data set
-define(LAWS(Ln, X, Y),
   begin
      X  = lens:put(Ln,    lens:get(Ln, X), X),
      d  = lens:get(Ln,    lens:put(Ln, d, X)),
      Y  = lens:put(Ln, e, lens:put(Ln, d, X))
   end
).

hd(_Config) ->
   Ln = fun lens:hd/2,
   X  = [1, 2, 3],
   Y  = [e, 2, 3],
   ?LAWS(Ln, X, Y).

tl(_Config) ->
   Ln = fun lens:tl/2,
   X  = [1, 2, 3],
   Y  = [1|e],
   ?LAWS(Ln, X, Y).

list(_Config) ->
   Ln = lens:list(fun erlang:is_atom/1),
   X  = [1, 2, a, 3, 4],
   Y  = [1, 2, e, 3, 4],
   ?LAWS(Ln, X, Y).

t1(_Config) ->
   Ln = fun lens:t1/2,
   X  = {1, 2, 3},
   Y  = {e, 2, 3},
   ?LAWS(Ln, X, Y).

t2(_Config) ->
   Ln = fun lens:t2/2,
   X  = {1, 2, 3},
   Y  = {1, e, 3},
   ?LAWS(Ln, X, Y).

t3(_Config) ->
   Ln = fun lens:t3/2,
   X  = {1, 2, 3},
   Y  = {1, 2, e},
   ?LAWS(Ln, X, Y).

tuple(_Config) ->
   Ln = lens:tuple(4),
   X  = {1, 2, 3, 4},
   Y  = {1, 2, 3, e},
   ?LAWS(Ln, X, Y).

tuple_pred(_Config) ->
   Ln = lens:tuple(fun erlang:is_atom/1),
   X  = {1, 2, a, 3, 4},
   Y  = {1, 2, e, 3, 4},
   ?LAWS(Ln, X, Y).
      
map(_Config) ->
   Ln = lens:map(b),
   X  = #{a => 1, b => 2, c => 3},
   Y  = #{a => 1, b => e, c => 3},
   ?LAWS(Ln, X, Y).

map_pred(_Config) ->
   Ln = lens:map(fun({_, X}) -> erlang:is_atom(X) end),
   X  = #{a => 1, b => a, c => 3},
   Y  = #{a => 1, b => e, c => 3},
   ?LAWS(Ln, X, Y).

%%%----------------------------------------------------------------------------   
%%%
%%% compose lenses
%%%
%%%----------------------------------------------------------------------------   

c(_Config) ->
   Ln = lens:c([lens:keylist(b), lens:tuple(2)]),
   X  = [{a, 1}, {b, 2}, {c, 3}],
   Y  = [{a, 1}, {b, e}, {c, 3}],
   ?LAWS(Ln, X, Y).

apply2(_Config) ->
   Ln1 = fun lens:hd/2,
   Ln2 = fun lens:t1/2,
   X   = [{1,2}],
   Y   = [{e,2}],  
   X = lens:put(Ln1, Ln2,    lens:get(Ln1, Ln2, X), X),
   d = lens:get(Ln1, Ln2,    lens:put(Ln1, Ln2, d, X)),
   Y = lens:put(Ln1, Ln2, e, lens:put(Ln1, Ln2, d, X)).

apply3(_Config) ->
   Ln1 = fun lens:tl/2,
   Ln2 = fun lens:hd/2,
   Ln3 = fun lens:t1/2,
   X   = [head, {1,2}],
   Y   = [head, {e,2}],  
   X = lens:put(Ln1, Ln2, Ln3,    lens:get(Ln1, Ln2, Ln3, X), X),
   d = lens:get(Ln1, Ln2, Ln3,    lens:put(Ln1, Ln2, Ln3, d, X)),
   Y = lens:put(Ln1, Ln2, Ln3, e, lens:put(Ln1, Ln2, Ln3, d, X)).

apply4(_Config) ->
   Ln1 = fun lens:tl/2,
   Ln2 = fun lens:hd/2,
   Ln3 = fun lens:t1/2,
   Ln4 = fun lens:hd/2,
   X   = [a, {[1],2}],
   Y   = [a, {[e],2}],  
   X = lens:put(Ln1, Ln2, Ln3, Ln4,    lens:get(Ln1, Ln2, Ln3, Ln4, X), X),
   d = lens:get(Ln1, Ln2, Ln3, Ln4,    lens:put(Ln1, Ln2, Ln3, Ln4, d, X)),
   Y = lens:put(Ln1, Ln2, Ln3, Ln4, e, lens:put(Ln1, Ln2, Ln3, Ln4, d, X)).

apply5(_Config) ->
   Ln1 = fun lens:tl/2,
   Ln2 = fun lens:hd/2,
   Ln3 = fun lens:t1/2,
   Ln4 = fun lens:hd/2,
   Ln5 = fun lens:t1/2,
   X   = [a, {[{1, 2}],2}],
   Y   = [a, {[{e, 2}],2}],  
   X = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5,    lens:get(Ln1, Ln2, Ln3, Ln4, Ln5, X), X),
   d = lens:get(Ln1, Ln2, Ln3, Ln4, Ln5,    lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, d, X)),
   Y = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, e, lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, d, X)).

apply6(_Config) ->
   Ln1 = fun lens:tl/2,
   Ln2 = fun lens:hd/2,
   Ln3 = fun lens:t1/2,
   Ln4 = fun lens:hd/2,
   Ln5 = fun lens:t1/2,
   Ln6 = fun lens:hd/2,
   X   = [a, {[{[1], 2}],2}],
   Y   = [a, {[{[e], 2}],2}],  
   X = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6,    lens:get(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, X), X),
   d = lens:get(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6,    lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, d, X)),
   Y = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, e, lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, d, X)).

apply7(_Config) ->
   Ln1 = fun lens:tl/2,
   Ln2 = fun lens:hd/2,
   Ln3 = fun lens:t1/2,
   Ln4 = fun lens:hd/2,
   Ln5 = fun lens:t1/2,
   Ln6 = fun lens:hd/2,
   Ln7 = fun lens:t1/2,
   X   = [a, {[{[{1}], 2}],2}],
   Y   = [a, {[{[{e}], 2}],2}],  
   X = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7,    lens:get(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, X), X),
   d = lens:get(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7,    lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, d, X)),
   Y = lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, e, lens:put(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, d, X)).


