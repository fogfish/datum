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
   pair_om/1


   % t1/1, t2/1, t3/1, tuple/1, tuple_pred/1, 
   % map/1, map_pred/1, map_default/1,
   % pair/1, pair_default/1
]).

%%
%% compose lenses
% -export([c/1, apply2/1, apply3/1, apply4/1, apply5/1, apply6/1, apply7/1]).

%%
%% omega lenses
% -export([
%    om_hd/1, om_tl/1, om_takewith/1
% ]).

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
      {group, pair}

     %  {group, pure}
     % ,{group, compose}
     % ,{group, omega}
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
         [pair, pair_om]}


      % {naive, [parallel], 
      %    [naive_type_map, naive_type_tuple]}

     %  {pure,  [parallel], 
     %     [
     %      map, map_default,
     %      % map, map_pred, map_default,
     %      pair, pair_default]}

     % ,{compose, [parallel], 
     %     [c, apply2, apply3, apply4, apply5, apply6, apply7]}

     % ,{omega, [parallel],
     %     [om_hd, om_tl, om_takewith]}
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


% pair(_Config) ->
%    Ln = lens:pair(b),
%    X  = [{a, 1}, {b, 2}, {c, 3}],
%    Y  = [{a, 1}, {b, e}, {c, 3}],
%    ?LAWS(Ln, X, Y).

% pair_default(_Config) ->
%    Ln = lens:pair(b, 2),
%    X  = [{a, 1}, {c, 3}],
%    X1 = [{a, 1}, {c, 3}, {b, 2}],
%    Y  = [{a, 1}, {c, 3}, {b, e}],
%    ?LAWS(Ln, X, X1, Y).

% %% Ln - lens, X - original data, Y - expected PutPut data set
% -define(LAWS(Ln, A, B, C),
%    begin
%       B  = lens:put(Ln,    lens:get(Ln, A), A),
%       d  = lens:get(Ln,    lens:put(Ln, d, A)),
%       C  = 
%    end
% ).
% -define(LAWS(Ln, A, Expect), ?LAWS(Ln, A, A, Expect)).


%%   Well behaved lens satisfies following laws
%%
%%
%%




% tuple_pred(_Config) ->
%    Ln = lens:tuple(fun erlang:is_atom/1),
%    X  = {1, 2, a, 3, 4},
%    Y  = {1, 2, e, 3, 4},
%    ?LAWS(Ln, X, Y).
      

% map_pred(_Config) ->
%    Ln = lens:map(fun({_, X}) -> erlang:is_atom(X) end),
%    X  = #{a => 1, b => a, c => 3},
%    Y  = #{a => 1, b => e, c => 3},
%    ?LAWS(Ln, X, Y).

% map_default(_Config) ->
%    Ln = lens:map(b, 2),
%    X  = #{a => 1, c => 3},
%    X1 = #{a => 1, b => 2, c => 3},
%    Y  = #{a => 1, b => e, c => 3},
%    ?LAWS(Ln, X, X1, Y).   

% % keylist(_Config) ->
% %    Ln = lens:keylist(b),
% %    X  = [{a, 1}, {b, 2}, {c, 3}],
% %    Y  = [{a, 1}, {b, e}, {c, 3}],
% %    ?LAWS(Ln, X, Y).

% % keylist_default(_Config) ->
% %    Ln = lens:keylist(b, 2),
% %    X  = [{a, 1}, {c, 3}],
% %    X1 = [{a, 1}, {c, 3}, {b, 2}],
% %    Y  = [{a, 1}, {c, 3}, {b, e}],
% %    ?LAWS(Ln, X, X1, Y).
   



% %%%----------------------------------------------------------------------------   
% %%%
% %%% compose lenses
% %%%
% %%%----------------------------------------------------------------------------   

% c(_Config) ->
%    Ln = lens:c([lens:keylist(b), lens:tuple(2)]),
%    X  = [{a, 1}, {b, 2}, {c, 3}],
%    Y  = [{a, 1}, {b, e}, {c, 3}],
%    ?LAWS(Ln, X, Y).

% apply2(_Config) ->
%    Ln1 = lens:hd(),
%    Ln2 = lens:t1(),
%    LnC = lens:c(Ln1, Ln2),
%    X   = [{1,2}],
%    Y   = [{e,2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).

% apply3(_Config) ->
%    Ln1 = lens:tl(),
%    Ln2 = lens:hd(),
%    Ln3 = lens:t1(),
%    LnC = lens:c(Ln1, Ln2, Ln3),
%    X   = [head, {1,2}],
%    Y   = [head, {e,2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).

% apply4(_Config) ->
%    Ln1 = lens:tl(),
%    Ln2 = lens:hd(),
%    Ln3 = lens:t1(),
%    Ln4 = lens:hd(),
%    LnC = lens:c(Ln1, Ln2, Ln3, Ln4),
%    X   = [a, {[1],2}],
%    Y   = [a, {[e],2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).

% apply5(_Config) ->
%    Ln1 = lens:tl(),
%    Ln2 = lens:hd(),
%    Ln3 = lens:t1(),
%    Ln4 = lens:hd(),
%    Ln5 = lens:t1(),
%    LnC = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5),
%    X   = [a, {[{1, 2}],2}],
%    Y   = [a, {[{e, 2}],2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).

% apply6(_Config) ->
%    Ln1 = lens:tl(),
%    Ln2 = lens:hd(),
%    Ln3 = lens:t1(),
%    Ln4 = lens:hd(),
%    Ln5 = lens:t1(),
%    Ln6 = lens:hd(),
%    LnC = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6),
%    X   = [a, {[{[1], 2}],2}],
%    Y   = [a, {[{[e], 2}],2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).

% apply7(_Config) ->
%    Ln1 = lens:tl(),
%    Ln2 = lens:hd(),
%    Ln3 = lens:t1(),
%    Ln4 = lens:hd(),
%    Ln5 = lens:t1(),
%    Ln6 = lens:hd(),
%    Ln7 = lens:t1(),
%    LnC = lens:c(Ln1, Ln2, Ln3, Ln4, Ln5, Ln6, Ln7),
%    X   = [a, {[{[{1}], 2}],2}],
%    Y   = [a, {[{[{e}], 2}],2}],  
%    X = lens:put(LnC,    lens:get(LnC, X), X),
%    d = lens:get(LnC,    lens:put(LnC, d, X)),
%    Y = lens:put(LnC, e, lens:put(LnC, d, X)).


% %%%----------------------------------------------------------------------------   
% %%%
% %%% omega lens interface
% %%%
% %%%----------------------------------------------------------------------------   




