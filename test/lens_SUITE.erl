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
   id/1,
   const/1,
   hbits/1,
   tbits/1,
   bits/1,
   hd/1,
   hd_om/1,
   tl/1, 
   tl_om/1,
   traverse/1,
   traverse_empty/1,
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
   keylist/1,
   keylist_om/1,

   require/1,
   defined/1,

   compose1/1, 
   compose2/1, 
   compose3/1, 
   compose4/1, 
   compose5/1, 
   compose6/1, 
   compose7/1,
   compose8/1,
   compose9/1,

   product1/1, 
   product2/1, 
   product3/1, 
   product4/1, 
   product5/1, 
   product6/1, 
   product7/1,
   product8/1,
   product9/1,

   get/1,
   put/1,
   map/1,
   apply/1,
   iso/1,
   iso4/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, basic},
      {group, binary},
      {group, list},
      {group, tuple},
      {group, map},
      {group, pair},
      {group, keylist},
      {group, unittest},
      {group, compose},
      {group, lens_api}
   ].

groups() ->
   [
      {basic, [parallel],
         [id, const]},

      {binary, [parallel],
         [hbits, tbits, bits]},

      {list, [parallel],
         [hd, hd_om, tl, tl_om, traverse, traverse_empty, takewith, takewith_om]},

      {tuple, [parallel],
         [t1, t2, t3, ti]},

      {map,   [parallel],
         [at, at_om]},

      {pair,   [parallel],
         [pair, pair_om]},

      {keylist,  [parallel],
         [keylist, keylist_om]},

      {unittest, [parallel],
         [require, defined]},

      {compose, [parallel], 
         [compose1, compose2, compose3, compose4, compose5, compose6, compose7, compose8, compose9,
          product1, product2, product3, product4, product5, product6, product7, product8, product9]},

      {lens_api, [parallel],
         [get, put, map, apply, iso, iso4]}
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
%%% basic lenses 
%%%
%%%----------------------------------------------------------------------------   

id(_Config) ->
   Lens = lens:id(),
   Value = 1,
   law_get_put(Lens, Value),
   law_put_get(Lens, a, Value),
   law_put_put(Lens, a, b, b, Value).

const(_Config) ->
   Lens = lens:const(a),
   a = lens:get(Lens, 1),
   a = lens:put(Lens, b, 1).

%%%----------------------------------------------------------------------------   
%%%
%%% binary lenses 
%%%
%%%----------------------------------------------------------------------------   

hbits(_Config) ->
   Lens = lens:hbits(8),
   List = <<"abc">>,
   law_get_put(Lens, List),
   law_put_get(Lens, <<"x">>, List),
   law_put_put(Lens, <<"x">>, <<"y">>, <<"ybc">>, List).

tbits(_Config) ->
   Lens = lens:tbits(16),
   List = <<"abc">>,
   law_get_put(Lens, List),
   law_put_get(Lens, <<"x">>, List),
   law_put_put(Lens, <<"x">>, <<"y">>, <<"aby">>, List).

bits(_Config) ->
   Lens = lens:bits(8, 8),
   List = <<"abc">>,
   law_get_put(Lens, List),
   law_put_get(Lens, <<"x">>, List),
   law_put_put(Lens, <<"x">>, <<"y">>, <<"ayc">>, List).


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

traverse_empty(_Config) ->
   Lens = lens:c(lens:traverse(), lens:t1()),
   List = [{1}, {2}, {3}],
   [1, 2, 3] = lens:get(Lens, List),
   [] = lens:get(Lens, []),

   [{1}, {1}, {1}] = lens:put(Lens, 1, List),
   [] = lens:put(Lens, 1, []).

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
%%% listkey lenses 
%%%
%%%----------------------------------------------------------------------------   

keylist(_Config) ->
   Lens = lens:keylist(b),
   List = [{a, 1}, {b, 2}, {c, 3}],
   law_get_put(Lens, List),
   law_put_get(Lens, {b, a}, List),
   law_put_put(Lens, {b, a}, {b, b}, [{a, 1}, {b, b}, {c, 3}], List).

keylist_om(_Config) ->
   Lens = lens:keylist(1, b, x),
   List = [{a, 1}, {b, 2}, {c, 3}],
   law_get_put(Lens, List),
   law_put_get(Lens, {b, a}, List),
   law_put_put(Lens, {b, a}, {b, b}, [{a, 1}, {b, b}, {c, 3}], List),
   x = lens:get(Lens, []),
   [{b, 1}] = lens:put(Lens, {b, 1}, []).


%%%----------------------------------------------------------------------------   
%%%
%%% unittest lenses 
%%%
%%%----------------------------------------------------------------------------   

require(_Config) ->
   {ok, 1} = lens:get(lens:c(lens:hd(), lens:require(1)), [1]),
   {error, {require, 1, 2}} = lens:get(lens:c(lens:hd(), lens:require(1)), [2]).

defined(_Config) ->
   {ok, 1} = lens:get(lens:c(lens:hd(), lens:defined()), [1]),
   {error, undefined} = lens:get(lens:c(lens:hd(), lens:defined()), []).



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


product1(_Config) ->
   Lens = lens:p([lens:at(a), lens:at(b)]),
   Data = #{a => 1, b => 2},
   [1, 2] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b], Data),
   law_put_put(Lens, [a, b], [-1, -2], #{a => -1, b => -2}, Data).

product2(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b)),
   Data = #{a => 1, b => 2},
   [1, 2] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b], Data),
   law_put_put(Lens, [a, b], [-1, -2], #{a => -1, b => -2}, Data).

product3(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c)),
   Data = #{a => 1, b => 2, c => 3},
   [1, 2, 3] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c], Data),
   law_put_put(Lens, [a, b, c], [-1, -2, -3], #{a => -1, b => -2, c => -3}, Data).

product4(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d)),
   Data = #{a => 1, b => 2, c => 3, d => 4},
   [1, 2, 3, 4] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d], Data),
   law_put_put(Lens, [a, b, c, d], [-1, -2, -3, -4], #{a => -1, b => -2, c => -3, d => -4}, Data).

product5(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d), lens:at(e)),
   Data = #{a => 1, b => 2, c => 3, d => 4, e => 5},
   [1, 2, 3, 4, 5] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d, e], Data),
   law_put_put(Lens, [a, b, c, d, e], [-1, -2, -3, -4, -5], #{a => -1, b => -2, c => -3, d => -4, e => -5}, Data).

product6(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d), lens:at(e), lens:at(f)),
   Data = #{a => 1, b => 2, c => 3, d => 4, e => 5, f => 6},
   [1, 2, 3, 4, 5, 6] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d, e, f], Data),
   law_put_put(Lens, [a, b, c, d, e, f], [-1, -2, -3, -4, -5, -6], #{a => -1, b => -2, c => -3, d => -4, e => -5, f => -6}, Data).

product7(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d), lens:at(e), lens:at(f), lens:at(g)),
   Data = #{a => 1, b => 2, c => 3, d => 4, e => 5, f => 6, g => 7},
   [1, 2, 3, 4, 5, 6, 7] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d, e, f, g], Data),
   law_put_put(Lens, [a, b, c, d, e, f, g], [-1, -2, -3, -4, -5, -6, -7], #{a => -1, b => -2, c => -3, d => -4, e => -5, f => -6, g => -7}, Data).

product8(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d), lens:at(e), lens:at(f), lens:at(g), lens:at(h)),
   Data = #{a => 1, b => 2, c => 3, d => 4, e => 5, f => 6, g => 7, h => 8},
   [1, 2, 3, 4, 5, 6, 7, 8] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d, e, f, g, h], Data),
   law_put_put(Lens, [a, b, c, d, e, f, g, h], [-1, -2, -3, -4, -5, -6, -7, -8], #{a => -1, b => -2, c => -3, d => -4, e => -5, f => -6, g => -7, h => -8}, Data).

product9(_Config) ->
   Lens = lens:p(lens:at(a), lens:at(b), lens:at(c), lens:at(d), lens:at(e), lens:at(f), lens:at(g), lens:at(h), lens:at(i)),
   Data = #{a => 1, b => 2, c => 3, d => 4, e => 5, f => 6, g => 7, h => 8, i => 9},
   [1, 2, 3, 4, 5, 6, 7, 8, 9] = lens:get(Lens, Data),
   law_get_put(Lens, Data),
   law_put_get(Lens, [a, b, c, d, e, f, g, h, i], Data),
   law_put_put(Lens, [a, b, c, d, e, f, g, h, i], [-1, -2, -3, -4, -5, -6, -7, -8, -9], #{a => -1, b => -2, c => -3, d => -4, e => -5, f => -6, g => -7, h => -8, i => -9}, Data).


%%
%%
get(_Config) ->
   1 = lens:get(lens:hd(), [1]).

put(_Config) ->
   [a] = lens:put(lens:hd(), a, [1]).

map(_Config) ->
   [10] = lens:map(fun(X) -> X * 10 end, lens:hd(), [1]).

apply(_Config) ->
   [10] = lens:apply(lens:hd(), fun(X) -> X * 10 end, [1]).


%%
%%
-record(address, {street = undefined}).
-record(user,    {name = undefined, address = #address{}}).

iso(_Config) ->
   Iso = lens:iso([
      {
         lens:ti(#user.name), 
         lens:at(name)
      },
      {
         lens:c(lens:ti(#user.address), lens:ti(#address.street)),
         lens:c(lens:at(address, #{}), lens:at(street))
      }
   ]),
   Rec = #user{name = "Verner", address = #address{street = "Blumenstraße"}},
   Map = #{name => "Verner", address => #{street => "Blumenstraße"}},

   Map = lens:isof(Iso, Rec, #{}),
   Rec = lens:isob(Iso, Map, #user{}).

%%
%%
iso_rec_name_street() ->
   lens:p(
      lens:ti(#user.name),
      lens:c(lens:ti(#user.address), lens:ti(#address.street))
   ).

iso_map_name_street() ->
   lens:p(
      lens:at(name),
      lens:c(lens:at(address, #{}), lens:at(street))
   ).

iso4(_Config) ->
   Rec = #user{name = "Verner", address = #address{street = "Blumenstraße"}},
   Map = #{name => "Verner", address => #{street => "Blumenstraße"}},

   Map = lens:iso(iso_rec_name_street(), Rec, iso_map_name_street(), #{}),
   Rec = lens:iso(iso_map_name_street(), Map, iso_rec_name_street(), #user{}).


