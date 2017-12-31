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
%% pure lens interface
-export([
   hd/1,
   hd_om/1,
   tl/1, 
   tl_om/1,
   traverse/1,
   takewith/1, 
   takewith_om/1,

   t1/1,
   t2/1,
   t3/1,
   ti/1,

   at/1,
   at_om/1,

   pair/1,
   pair_om/1,

   compose1/1, 
   compose2/1, 
   compose3/1, 
   compose4/1, 
   compose5/1, 
   compose6/1, 
   compose7/1,
   compose8/1,
   compose9/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, list},
      {group, tuple},
      {group, map},
      {group, pair},
      {group, compose}
   ].

groups() ->
   [
      {list, [parallel],
         [hd, hd_om, tl, tl_om, traverse, takewith, takewith_om]},

      {tuple, [parallel],
         [t1, t2, t3, ti]},

      {map,   [parallel],
         [at, at_om]},

      {pair,   [parallel],
         [pair, pair_om]},

      {compose, [parallel], 
         [compose1, compose2, compose3, compose4, compose5, compose6, compose7, compose8, compose9]}
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
%%% pure lens interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% GetPut - if we get focused element a() from s() and immediately put a() 
%%               with no modifications back into s(), we must get back exactly s().
law_get_put(Lens, Struct) ->
   Struct = lens:put(Lens, lens:get(Lens, Struct), Struct).

%%
%% PutGet - if putting a() inside s() yields a new s(), 
%%          then the a() obtained from s is exactly a().
law_put_get(Lens, Value, Struct) ->
   Value = lens:get(Lens, lens:put(Lens, Value, Struct)).

%%
%% PutPut - A sequence of two puts is just the effect of the second, 
%%          the first gets completely overwritten. This law is applicable 
%%          to very well behaved lenses.
law_put_put(Lens, Value1, Value2, Expect, Struct) ->
   Expect = lens:put(Lens, Value2, lens:put(Lens, Value1, Struct)).


%%%----------------------------------------------------------------------------   
%%%
%%% list lenses 
%%%
%%%----------------------------------------------------------------------------   

hd(_Config) ->
   Lens = lens:hd(),
   List = [1, 2, 3],
   law_get_put(Lens, List),
   law_put_get(Lens, a, List),
   law_put_put(Lens, a, b, [b, 2, 3], List).

hd_om(_Config) ->
   Lens = lens:hd(e),
   List = [1, 2, 3],
   law_get_put(Lens, List),
   law_put_get(Lens, a, List),
   law_put_put(Lens, a, b, [b, 2, 3], List),
   e   = lens:get(Lens, []),
   [x] = lens:put(Lens, x, []).

tl(_Config) ->
   Lens = lens:tl(),
   List = [1, 2, 3],
   law_get_put(Lens, List),
   law_put_get(Lens, [a, b], List),
   law_put_put(Lens, [a, b], [b, c], [1, b, c], List).

tl_om(_Config) ->
   Lens = lens:tl([e]),
   List = [1, 2, 3],
   law_get_put(Lens, List),
   law_put_get(Lens, [a, b], List),
   law_put_put(Lens, [a, b], [b, c], [1, b, c], List),
   [e] = lens:get(Lens, []),
   [x] = lens:put(Lens, [x], []).

traverse(_Config) ->
   %% traverse is not well behaved lens
   Lens = lens:traverse(),
   List = [1, 2, 3],
   List = lens:get(Lens, List),
   [x, x, x] = lens:put(Lens, x, List).

takewith(_Config) ->
   Lens = lens:takewith(fun erlang:is_atom/1),
   List = [1, 2, a, 3, 4],
   law_get_put(Lens, List),
   law_put_get(Lens, b, List),
   law_put_put(Lens, a, b, [1, 2, b, 3, 4], List).

takewith_om(_Config) ->
   Lens = lens:takewith(fun erlang:is_atom/1, b),
   List = [1, 2, a, 3, 4],
   law_get_put(Lens, List),
   law_put_get(Lens, b, List),
   law_put_put(Lens, a, b, [1, 2, b, 3, 4], List),
   b = lens:get(Lens, [1, 2, 3, 4]),
   [1, 2, 3, 4, x] = lens:put(Lens, x, [1, 2, 3, 4]).


%%%----------------------------------------------------------------------------   
%%%
%%% tuple lenses 
%%%
%%%----------------------------------------------------------------------------   

t1(_Config) ->
   Lens = lens:t1(),
   Data = {1, 2, 3, 4},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, {b, 2, 3, 4}, Data).

t2(_Config) ->
   Lens = lens:t2(),
   Data = {1, 2, 3, 4},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, {1, b, 3, 4}, Data).

t3(_Config) ->
   Lens = lens:t3(),
   Data = {1, 2, 3, 4},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, {1, 2, b, 4}, Data).

ti(_Config) ->
   Lens = lens:ti(4),
   Data = {1, 2, 3, 4},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, {1, 2, 3, b}, Data).

%%%----------------------------------------------------------------------------   
%%%
%%% map lenses 
%%%
%%%----------------------------------------------------------------------------   

at(_Config) ->
   Lens = lens:at(b),
   Data = #{a => 1, b => 2, c => 3},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, #{a => 1, b => b, c => 3}, Data).

at_om(_Config) ->
   Lens = lens:at(b, x),
   Data = #{a => 1, b => 2, c => 3},
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, #{a => 1, b => b, c => 3}, Data),
   x = lens:get(Lens, #{}),
   #{b := 1} = lens:put(Lens, 1, #{}).


%%%----------------------------------------------------------------------------   
%%%
%%% pair lenses 
%%%
%%%----------------------------------------------------------------------------   

pair(_Config) ->
   Lens = lens:pair(b),
   List = [{a, 1}, {b, 2}, {c, 3}],
   law_get_put(Lens, List),
   law_put_get(Lens, a, List),
   law_put_put(Lens, a, b, [{a, 1}, {b, b}, {c, 3}], List).

pair_om(_Config) ->
   Lens = lens:pair(b, x),
   List = [{a, 1}, {b, 2}, {c, 3}],
   law_get_put(Lens, List),
   law_put_get(Lens, a, List),
   law_put_put(Lens, a, b, [{a, 1}, {b, b}, {c, 3}], List),
   x = lens:get(Lens, []),
   [{b, 1}] = lens:put(Lens, 1, []).


%%%----------------------------------------------------------------------------   
%%%
%%% compose lenses 
%%%
%%%----------------------------------------------------------------------------   

compose1(_Config) ->
   Lens = lens:c([lens:keylist(b), lens:t2()]),
   Data = [{a, 1}, {b, 2}, {c, 3}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [{a, 1}, {b, b}, {c, 3}], Data).

compose2(_Config) ->
   Ln1  = lens:hd(),
   Ln2  = lens:t1(),
   Lens = lens:c(Ln1, Ln2),
   Data = [{1,2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [{b,2}], Data).

compose3(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Lens = lens:c(Ln1, Ln2, Ln3),
   Data = [head, {1,2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [head, {b,2}], Data).

compose4(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4),
   Data = [a, {[1],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[b],2}], Data).

compose5(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Ln5  = lens:t1(),
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5),
   Data = [a, {[{1, 2}],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[{b, 2}],2}], Data).

compose6(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Ln5  = lens:t1(),
   Ln6  = lens:hd(),
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6),
   Data = [a, {[{[1], 2}],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[{[b], 2}],2}], Data).

compose7(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Ln5  = lens:t1(),
   Ln6  = lens:hd(),
   Ln7  = lens:t1(),
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7),
   Data   = [a, {[{[{1}], 2}],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[{[{b}], 2}],2}], Data).

compose8(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Ln5  = lens:t1(),
   Ln6  = lens:hd(),
   Ln7  = lens:t1(),
   Ln8  = lens:hd(),
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, Ln8),
   Data = [a, {[{[{[1]}], 2}],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[{[{[b]}], 2}],2}], Data).
                           
compose9(_Config) ->
   Ln1  = lens:tl(),
   Ln2  = lens:hd(),
   Ln3  = lens:t1(),
   Ln4  = lens:hd(),
   Ln5  = lens:t1(),
   Ln6  = lens:hd(),
   Ln7  = lens:t1(),
   Ln8  = lens:hd(),
   Ln9  = lens:t1(), 
   Lens = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7, Ln8, Ln9),
   Data   = [a, {[{[{[{1}]}], 2}],2}],
   law_get_put(Lens, Data),
   law_put_get(Lens, a, Data),
   law_put_put(Lens, a, b, [a, {[{[{[{b}]}], 2}],2}], Data).
