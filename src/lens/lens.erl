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
%%   Lenses resembles concept of getters and setters, which you can compose 
%%   using functional concepts. In other words, this is combinator data 
%%   transformation for pure functional data structure.
%%
-module(lens).
-compile({parse_transform, partial}).
-compile({parse_transform, category}).

%%
%% lens primitives
-export([fmap/2, apply/3, map/3, get/2, put/3, iso/2, isof/3, isob/3, iso/4]). 

%%
%% lenses
-export([id/0, const/1]).
-export([hd/0, hd/1, tl/0, tl/1]).
-export([t1/0, t2/0, t3/0, ti/1]).
-export([at/1, at/2]).
-export([keylist/1, keylist/2, keylist/3, pair/1, pair/2]).

%%
%% traverse
-export([traverse/0, takewith/1, takewith/2, require/1, defined/0]).

%%
%% lens utility
-export([c/1, c/2, c/3, c/4, c/5, c/6, c/7, c/8, c/9]).
-export([p/1, p/2, p/3, p/4, p/5, p/6, p/7, p/8, p/9]).

-export_type([lens/0]).

-compile({no_auto_import,[apply/3, hd/1, tl/1]}).
-compile([inline, {inline_size, 128}, inline_list_funcs]).

%%
%% data types
-type s() :: _.
-type a() :: _.  

-type f(F) :: [atom()|F].
-spec fmap( fun((a()) -> _), f(a()) ) -> f(_).     

%% van Laarhoven lens type
-type lens(A, S) :: fun( (fun( (A) -> f(A) ), S) -> f(S) ).
-type lens()     :: lens(a(), s()).


%%
%% identity functor
-spec with_id(a()) -> f(a()).

with_id(X) ->
   [id|X].

%%
%% const functor
-spec with_const(a()) -> f(a()).

with_const(X) ->
   [const|X].

%%
%% functor fmap implementation, see spec above
fmap(Fun, [id|X]) -> 
   with_id( Fun(X) );
fmap(_,   [const|_] = X) -> 
   X.


%%  
%% Returns modified s() by applying the function to focus point of the lens
-spec map(fun( (a()) -> a() ), lens(), s()) -> s().

map(Fun, Ln, S) ->
   erlang:tl( Ln(fun(X) -> fmap(Fun, with_id(X)) end, S) ).


%%
%% Returns value of focus point of the lens
-spec get(lens(), s()) -> a().

get(Ln, S) ->
   erlang:tl( Ln(fun(X) -> fmap(undefined, with_const(X)) end, S) ).


%%
%% Returns modified s() by setting a new value to focus point of the lens
-spec put(lens(), a(), s()) -> s().

put(Ln, A, S) ->
   map(fun(_) -> A end, Ln, S).


%%
%% Isomorphism translates between different data structures.
-spec iso(lens(), _, lens(), _) -> _.

iso(LensA, A, LensB, B) ->
   lens:put(LensB, lens:get(LensA, A), B).


%%%------------------------------------------------------------------
%%%
%%% deprecated, removed at release 5.x.x
%%%
%%%------------------------------------------------------------------

%%
%% Use lens:map/3
-spec apply(lens(), fun( (a()) -> a() ), s()) -> s().

apply(Ln, Fun, S) ->
   erlang:tl( Ln(fun(X) -> fmap(Fun, with_id(X)) end, S) ).


%%
%% Use lens:iso/4
 -spec iso(lens(), lens()) -> {_, _}.

iso(LensesA, LensesB)
 when is_list(LensesA), is_list(LensesB) ->
   iso(lens:p(LensesA), lens:p(LensesB));
iso(LensesA, LensesB) ->
   {morphism(LensesA, LensesB), morphism(LensesB, LensesA)}.

morphism(LensesA, LensesB) ->
   fun(Source, Target) ->
      lens:put(LensesB, lens:get(LensesA, Source), Target)
   end.

%%
%% applies forward isomorphism from A to B
-spec isof({_, _}, _, _) -> _.

isof({Iso, _}, A, B) ->
   Iso(A, B).

%%
%% applies backward isomorphism from B to A
-spec isob({_, _}, _, _) -> _.

isob({_, Iso}, A, B) ->
   Iso(A, B).

%%%------------------------------------------------------------------
%%%
%%% lenses 
%%%
%%%------------------------------------------------------------------

%%
%%
-spec id() -> lens(_, _).

id() ->
   fun(Fun, Focus) ->
      lens:fmap(fun(X) -> X end, Fun(Focus))
   end.

%%
%%
-spec const(_) -> lens(_, _).

const(X) ->
   fun(Fun, _) ->
      lens:fmap(fun(_) -> X end, Fun(X))
   end.

%%%------------------------------------------------------------------
%%%
%%% list lenses 
%%%
%%%------------------------------------------------------------------

%%
%% focus head of list
-spec hd() -> lens(_, list()).
-spec hd(_) -> lens(_, list()).

hd() ->
   hd(undefined).

hd(Om) ->
   fun
   (Fun, [H|T]) ->
      fmap(fun(X) -> [X|T] end, Fun(H));
   (Fun,    []) ->
      fmap(fun(X) -> [X] end, Fun(Om))
   end.


%%
%% focus tail of list
-spec tl() -> lens(list(), list()).
-spec tl(_) -> lens(list(), list()).

tl() ->
   fun(Fun, [H|T]) ->
      fmap(fun(X) -> [H|X] end, Fun(T))
   end.

tl(Om) ->
   fun
   (Fun, [H|T]) ->
      fmap(fun(X) -> [H|X] end, Fun(T));
   (Fun,    []) ->
      fmap(fun(X) -> X end, Fun(Om))
   end.

%%%------------------------------------------------------------------
%%%
%%% tuple lenses 
%%%
%%%------------------------------------------------------------------

%%
%% focus fist tuple element
-spec t1() -> lens(_, tuple()).

t1() ->
   fun(Fun, Term) ->
      fmap(erlang:setelement(1, Term, _), Fun(erlang:element(1, Term)))
   end.

%%
%% focus second tuple element
-spec t2() -> lens(_, tuple()).

t2() ->
   fun(Fun, Term) ->
      fmap(erlang:setelement(2, Term, _), Fun(erlang:element(2, Term)))
   end.

%%
%% focus third tuple element
-spec t3() -> lens(_, tuple()).

t3() ->
   fun(Fun, Term) ->
      fmap(erlang:setelement(3, Term, _), Fun(erlang:element(3, Term)))
   end.
   
%%
%% focuses tuple element using index
-spec ti(integer()) -> lens(_, tuple()).

ti(I)
 when is_integer(I) -> 
   fun(Fun, Term) ->
      fmap(erlang:setelement(I, Term, _), Fun(erlang:element(I, Term)))
   end.

%%%------------------------------------------------------------------
%%%
%%% map lenses 
%%%
%%%------------------------------------------------------------------

%%
%% focuses map element using key.
-spec at(_) -> lens(_, map()).
-spec at(_, _) -> lens(_, map()).

at(Key) ->
   at(Key, undefined).

at(Key, Om) ->
   fun(Fun, Map) ->
      fmap(maps:put(Key, _, Map), Fun(maps:get(Key, Map, Om)))
   end.

%%%------------------------------------------------------------------
%%%
%%% keylist lenses 
%%%
%%%------------------------------------------------------------------

%%
%% focuses tuple in keylist.
-spec keylist(_) -> lens(_, [tuple()]).
-spec keylist(_, _) -> lens(_, [tuple()]).
-spec keylist(_, _, _) -> lens(_, [tuple()]).

keylist(Key) ->
   keylist(1, Key).

keylist(N, Key) ->
   keylist(N, Key, undefined).

keylist(N, Key, Om) ->
   fun(Fun, List) ->
      H = case lists:keyfind(Key, N, List) of
         false -> Om;
         Value -> Value
      end,
      fmap(lists:keystore(Key, N, List, _), Fun(H))
   end.

%%
%% focuses pair value
-spec pair(_) -> lens(_, [{_, _}]).
-spec pair(_, _) -> lens(_, [{_, _}]).

pair(Key) ->
   pair(Key, undefined).

pair(Key, Om) ->
  fun(Fun, List) ->
      H = case lists:keyfind(Key, 1, List) of
         false -> Om;
         {_, Value} -> Value
      end,
      fmap(fun(X) -> lists:keystore(Key, 1, List, {Key, X}) end, Fun(H))
   end.



%%%------------------------------------------------------------------
%%%
%%% traverse
%%%
%%%------------------------------------------------------------------

%%
%% The lens focuses on each element of the list
%% e.g
%%   lens:get(lens:c(lens:traverse(), lens:t1()), [{1},{2}]).
-spec traverse() -> lens(_, list()). 

traverse() ->
   fun(Fun, List) ->
      lists:foldr(
         fun(X, Acc) ->
            '++'(fmap(fun(Y) -> Y end, Fun(X)), Acc)
         end,
         [],
         List
      )
   end.

'++'([F|X], [])    -> [F|[X]];
'++'([F|H], [F|T]) -> [F|[H|T]].


%%
%% The lens takes a predicate and focuses the leftmost element 
%% of the structure matching the predicate
-spec takewith(fun((_) -> true | false)) -> lens(_, list()).
-spec takewith(fun((_) -> true | false), _) -> lens(_, list()).

takewith(Pred) ->
   fun(Fun, List) ->
      {H, [I|T]} = lists:splitwith(fun(X) -> not Pred(X) end, List),
      fmap(fun(X) -> H ++ [X|T] end, Fun(I))
   end.

takewith(Pred, Om) ->
   fun(Fun, List) ->
      {Head, [El|Tail]} = case      
         lists:splitwith(fun(X) -> not Pred(X) end, List)
      of
         {H, []} -> {H, [Om]};
         Value   -> Value
      end,
      fmap(fun(X) -> Head ++ [X|Tail] end, Fun(El))
   end.

%%
%% The lens implements either semantic, returns {ok, _} if focused element 
%% matches required value, error otherwise 
-spec require(_) -> lens(_, datum:either(_)).

require(Value) ->
   fun(Fun, X) ->
      case X of
         Value ->
            lens:fmap(fun(_) -> Value end, Fun({ok, Value}));
         _    ->
            lens:fmap(fun(_) -> Value end, Fun({error, {require, Value, X}}))
      end
   end.

%%
%% The lens implements either semantic, returns {ok, _} if focused element is defined
defined() ->
   fun(Fun, undefined) ->
         lens:fmap(fun(X) -> X end, Fun({error, undefined}));
      (Fun, Value) ->
         lens:fmap(fun(X) -> X end, Fun({ok, Value}))
   end.


%%%------------------------------------------------------------------
%%%
%%% lens utility
%%%
%%%------------------------------------------------------------------

%%
%% The lens composition is powerful concept to produce complex lenses.
-spec c([lens()]) -> lens().

c(Lenses) ->
   fun(Fun, S) ->
      dot(lists:reverse(Lenses), Fun, S)
   end.

dot([Ln], Fun, S) ->
   Ln(Fun, S);
dot([Ln | Lenses], Fun, S) ->
   dot(Lenses, fun(X) -> Ln(Fun, X) end, S).

%%
%% The list composition function is not efficient from performance perspective, 
%% list-based folding is expensive. The efficiency of lens can be improved by 40%
%% using inline variants of combinator  `apply`, `get` and `put`.  
%%

-spec c(lens(), lens()) -> lens().
-spec c(lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec c(lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().

c(Ln2, Ln1) ->
   fun(Fun, S) -> 
      Ln2(Ln1(Fun, _), S) 
   end.

c(Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln3(Ln2(Ln1(Fun, _), _), S)
   end.

c(Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), S)
   end.

c(Ln5, Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln5(Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), _), S)
   end.

c(Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln6(Ln5(Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), _), _), S)
   end.

c(Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln7(Ln6(Ln5(Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), _), _), _), S)
   end.

c(Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln8(Ln7(Ln6(Ln5(Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), _), _), _), _), S)
   end.

c(Ln9, Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   fun(Fun, S) ->
      Ln9(Ln8(Ln7(Ln6(Ln5(Ln4(Ln3(Ln2(Ln1(Fun, _), _), _), _), _), _), _), _), S)
   end.

%%
%% The product lens composes lenses to spawn multiple fields at once
-spec p([lens()]) -> lens().

p(Lenses)
 when is_list(Lenses) ->
   fun(Fun, Struct) ->
      fmap(put_lens_product(Lenses, _, Struct), Fun(get_lens_product(Lenses, Struct)))
   end.

get_lens_product(Lenses, Struct) ->
   [lens:get(LnX, Struct) || LnX <- Lenses].

put_lens_product([Lens | Lenses], [X | View], Struct) ->
   put_lens_product(Lenses, View, lens:put(Lens, X, Struct));
put_lens_product([], [], Struct) ->
   Struct.


%%
%% Inline variants of lens product combinator.  
%%

-spec p(lens(), lens()) -> lens().
-spec p(lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().
-spec p(lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens(), lens()) -> lens().

p(Ln2, Ln1) ->
   p([Ln2, Ln1]).

p(Ln3, Ln2, Ln1) ->
   p([Ln3, Ln2, Ln1]).

p(Ln4, Ln3, Ln2, Ln1) ->
   p([Ln4, Ln3, Ln2, Ln1]).

p(Ln5, Ln4, Ln3, Ln2, Ln1) ->
   p([Ln5, Ln4, Ln3, Ln2, Ln1]).

p(Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   p([Ln6, Ln5, Ln4, Ln3, Ln2, Ln1]).

p(Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   p([Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1]).

p(Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   p([Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1]).

p(Ln9, Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1) ->
   p([Ln9, Ln8, Ln7, Ln6, Ln5, Ln4, Ln3, Ln2, Ln1]).

