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
%%   Lenses resembles concept of getters and setters, which you can compose 
%%   using functional concepts. In other words, this is combinator data 
%%   transformation for pure functional data structure.
%%
%% @see
%% 
%%   The lens implementation is partially base on Haskell lens library, and 
%%   references
%%   * Combinators for Bi-Directional Tree Transformations: 
%%     A Linguistic Approach to the View Update Problem by J. Nathan Foster et al.
%%     http://repository.upenn.edu/cgi/viewcontent.cgi?article=1044&context=cis_reports
%%   * Lens tutorial by Jakub Arnold
%%     http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html
%%
%%   There are other approaches to implement lens for Erlang
%%   * https://github.com/jlouis/erl-lenses by Jesper Louis Andersen
%%   * http://www.cs.otago.ac.nz/staffpriv/ok/lens.erl by Richard A. O'Keefe
%%
-module(lens).

%%
%% naive lens interface
-export([nget/2, nput/3, napply/3, naive/1]).

%%
%% lens primitives
-export([fmap/2, apply/3, get/2, put/3]). 

%%
%% lenses  
-export([hd/2, tl/2 ]).
-export([t1/2, t2/2, t3/2, tuple/1]).
-export([map/1]).
-export([keylist/1]).

%%
%% lens utility
-export([c/1]).
-export([apply/4, apply/5, apply/6, apply/7, apply/8, apply/9]).
-export([get/3, get/4, get/5, get/6, get/7, get/8]).
-export([put/4, put/5, put/6, put/7, put/8, put/9]).


-compile({no_auto_import,[apply/3]}).
-compile([inline, {inline_size, 128}, inline_list_funcs]).

%%%------------------------------------------------------------------
%%%
%%% naive lens interface
%%%
%%%------------------------------------------------------------------

%%
%% Lens types are defined as ... They are following the convention of Haskell lens library.

%% type of object
-type s() :: _.

%% type of focused element (focus type)  
-type a() :: _.  

%% originally lenses are defined using `get` and `put` primitives. 
%% The third primitive `over` (or `apply`) allows to enhance lens behavior using 
%% function. The `put` is `over` using `const` function.   

-type naive() :: 
   #{
      get   => fun( (s()) -> a() ),
      apply => fun( (fun( (a()) -> a() ), s()) -> s() )
   }.

%%
%% naive lens interface
%%

-spec nget(naive(), s()) -> a().

nget(#{get := Ln}, S) -> 
   Ln(S).


-spec nput(naive(), a(), s()) -> s().

nput(Ln, Val, S) ->
   napply(Ln, fun(_) -> Val end, S).

-spec napply(naive(), fun( (a()) -> a() ), s()) -> s().

napply(#{apply := Ln}, Fun, S) ->
   Ln(Fun, S).
   

%%
%% the simple lens implementation to support built-in types: tuples, maps and key-val lists
%%    
%%    Stock  = {stock, "BZNT", 50}.
%%    Ticker = lens:naive(2).
%%    Price  = lens:naive(3).
%% 
%%    lens:put(Ticker, "NTXX", Stock). 
%%    lens:apply(Price, fun(X) -> X + 15 end, Stock).
%%    lens:get(Ticker, Stock).
%%

-spec naive(_) -> naive().

naive(Key) ->
   #{
      get   => fun(X) -> naive_get(Key, X) end, 
      apply => fun(Fun, X) -> naive_apply(Key, Fun, X) end
   }.

%%
%%
naive_get(Key, X)
 when is_map(X) ->
   maps:get(Key, X);

naive_get(Key, X)
 when is_tuple(X) ->
   erlang:element(Key, X);

naive_get(Key, X)
 when is_list(X) ->
   erlang:element(2, lists:keyfind(Key, 1, X)).

%%
%%
naive_apply(Key, Fun, X)
 when is_map(X) ->
   maps:put(Key, Fun(maps:get(Key, X)), X);

naive_apply(Key, Fun, X)
 when is_tuple(X) ->
   erlang:setelement(Key, X, Fun(erlang:element(Key, X)));

naive_apply(Key, Fun, X)
 when is_list(X) ->
   {value, Val, List} = lists:keytake(Key, 1, X),
   [{Key, Fun(Val)} | List].

%%%------------------------------------------------------------------
%%%
%%% lens primitives
%%%
%%%------------------------------------------------------------------

%%
%% The used lens structure is not scalable when you need to expends with new primitives or
%% support new data types. You either grow it by implementing various flavors of getters 
%% and setters or extend module to support new data types.
%%
%% van Laarhoven lens generalization solves the problem, the proposal to use functor
%% to implement `get`, `put`, `apply`, etc. The lens is defined as 
%%
%%    type Lens s a = Functor f => (a -> f a) -> s -> f s
%%
%% Note: there is a good tutorial about type classes and functors  
%% http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
%% http://scalaz.github.io/scalaz/scalaz-2.9.1-6.0.2/doc.sxr/scalaz/Functor.scala.html
%% 
%% Functors do not exists in Erlang, Let's define one with minimal (no) runtime overhead.
%% Let's skip all details on the design decision about the function definition below. 
%% In the nutshell, various Erlang native containers (tuple, function, etc) are evaluated.
%% The list shown best performance. There is not any intent to generalize application wide
%% functor concept, it is made only to support lens implementation.
-type f(F) :: [atom()|F].
-spec fmap( fun((a()) -> _), f(a()) ) -> f(_).     

%% van Laarhoven lens type
-type lens() :: fun( (fun( (a()) -> f(a()) ), s() ) -> f(s()) ).


%% Implementation of lenses requires two type of functors
%%  * `apply` / 'over' is built with `identity`
%%  * `get` is built with `const`

%%
%% identity functor
-spec id(a()) -> f(a()).

id(X) ->
   [id|X].

%%
%% const functor
-spec const(a()) -> f(a()).

const(X) ->
   [const|X].

%%
%% functor fmap implementation, see spec above
fmap(Fun, [id|X]) -> 
   id( Fun(X) );
fmap(_,   [const|_] = X) -> 
   X.
   
%%
%% The `apply` is defined as 
%% Given a lens() that focuses on a() inside of s(), and
%% a function from a() to a() and instance of object s().
%% It returns modified s() by applying the function 
%% to focus point of the lens, e.g. Haskell use following notation 
%%  over :: Lens s a -> (a -> a) -> s -> s
%%  
-spec apply(lens(), fun( (a()) -> a() ), s()) -> s().

apply(Ln, Fun, S) ->
   % @todo: The failing lenses are not handled properly if focus is not exists
   %        They can return or handle `Omega` term. It is not clear how to 
   %        a new container from nothing. The further investigation is required.
   tl( Ln(fun(X) -> fmap(Fun, id(X)) end, S) ).


%%
%% The `get` is defined as
%% Given a lens() that focuses on a() inside of s(), and
%% instance of object s(). It returns value of focus point a(). 
%% e.g. Haskell use following notation
%%  view :: Lens s a -> s -> a
%%
-spec get(lens(), s()) -> a().

get(Ln, S) ->
   tl( Ln(fun(X) -> fmap(undefined, const(X)) end, S) ).

%%
%% The `put` is `over` using `const` function.
-spec put(lens(), a(), s()) -> s().

put(Ln, A, S) ->
   apply(Ln, fun(_) -> A end, S).


%%%------------------------------------------------------------------
%%%
%%% lenses 
%%%
%%%------------------------------------------------------------------

%% 
%% Lens primitives above requires actual lens implementation. The lens
%% definition is library agnostic. The function of lens() type is required.
%%
%% in Haskell
%%    Functor f => (a -> f a) -> s -> f s
%%
%% in Erlang
%% -type lens() :: fun( (fun( (a()) -> f(a()) ), s() ) -> f(s()) ).
%%
%% Let's define a lens that focuses on head of list lens:hd/2 (see definition below)
%%
%% The lens usage is straight forward:
%%
%%    lens:get(fun lens:hd/2, [1,2]).  
%%    lens:put(fun lens:hd/2, 5, [1, 2]).
%%    lens:apply(fun lens:hd/2, fun(X) -> X + 1 end, [1, 2]).
%%
%% Well behaved lens satisfies following laws
%%  * GetPut - if we get focused element a() from s() and immediately put a() 
%%             with no modifications back into s(), we must get back exactly s().
%%
%%    [a] = lens:put(fun lens:hd/2, lens:get(fun lens:hd/2, [a]), [a]).
%%
%%  * PutGet - if putting a() inside s() yields a new s(), 
%%             then the a() obtained from s is exactly a().
%%
%%    b = lens:get(fun lens:hd/2, lens:put(fun lens:hd/2, b, [a])).
%%
%%  * PutPut - A sequence of two puts is just the effect of the second, 
%%             the first gets completely overwritten. This law is applicable 
%%             to very well behaved lenses.
%%
%%    [c] = lens:put(fun lens:hd/2, c, lens:put(fun lens:hd/2, b, [a])).
%%
%%
%% The utility section covers aspects of lens composition and building a complex
%% applications.
%%

%%
%% @todo: How to use predicate function as a selector?
%%        How to select 'all' (Is this feasible)?

%%
%% 
-spec hd(fun( (_) -> f(_) ), list() ) -> f(list()).

hd(Fun, [H|T]) ->
   fmap(fun(X) -> [X|T] end, Fun(H)).

%%
%%
-spec tl(fun( (list()) -> f(list()) ), list() ) -> f(list()).

tl(Fun, [H|T]) ->
   fmap(fun(X) -> [H|X] end, Fun(T)).

% list_get_fun(Pred, [H|T]) ->
%    case Pred(H) of
%       true  -> H;
%       false -> list_get_fun(Pred, T)
%    end.

% list_apply_fun(Pred, Fun, [H|T]) ->
%    case Pred(H) of
%       true  -> [Fun(H)|T];
%       false -> [H|list_apply_fun(Pred, Fun, T)]
%    end.

%%
%%
-spec t1(fun( (_) -> f(_) ), tuple() ) -> f(tuple()).

t1(Fun, Term) ->
   fmap(fun(X) -> erlang:setelement(1, Term, X) end, Fun(erlang:element(1, Term))).

%%
%%
-spec t2(fun( (_) -> f(_) ), tuple() ) -> f(tuple()).

t2(Fun, Term) ->
   fmap(fun(X) -> erlang:setelement(2, Term, X) end, Fun(erlang:element(2, Term))).

%%
%%
-spec t3(fun( (_) -> f(_) ), tuple() ) -> f(tuple()).

t3(Fun, Term) ->
   fmap(fun(X) -> erlang:setelement(3, Term, X) end, Fun(erlang:element(3, Term))).
   
%%
%%
-spec tuple(integer()) -> fun( (fun( (_) -> f(_) ), tuple() ) -> f(tuple()) ).

tuple(I) -> 
   fun(Fun, Term) ->
      fmap(fun(X) -> erlang:setelement(I, Term, X) end, Fun(erlang:element(I, Term)))
   end.

% tuple_get_fun(Pred, I, X) ->
%    H = erlang:element(I, X),
%    case Pred(H) of
%       true  -> H;
%       false -> tuple_get_fun(Pred, I + 1, X)
%    end.  

% tuple_apply_fun(Pred, Fun, I, X) ->
%    H = erlang:element(I, X),
%    case Pred(H) of
%       true  -> erlang:setelement(I, X, Fun(H));
%       false -> tuple_put_fun(Pred, Fun, I + 1, X)
%    end.

%%
%%
-spec map(_) -> fun( (fun( (_) -> f(_) ), map() ) -> f(map()) ).

map(Key) ->
   fun(Fun, Map) ->
      fmap(fun(X) -> maps:put(Key, X, Map) end, Fun(maps:get(Key, Map)))
   end.

% map_get_fun(Pred, X) ->
%    list_get_fun(Pred, maps:to_list(X)).
%
% map_apply_fun(Pred, Fun, X) ->
%    maps:from_list(list_apply_fun(Pred, Fun, maps:to_list(X))).

%%
%%
-spec keylist(_) -> fun( (fun( (_) -> f(_) ), list() ) -> f(list()) ).

keylist({N, Key}) -> 
   fun(Fun, List) ->
      {value, H, _} = lists:keytake(Key, N, List),
      fmap(fun(X) -> lists:keystore(Key, N, List, X) end, Fun(H))
   end;

keylist(Key) ->
   keylist({1, Key}).

% keylist_get_key(N, Key, X) ->
%    case lists:keyfind(Key, N, X) of
%       false -> exit(badarg);
%       H     -> H
%    end.

% keylist_apply_key(Key, Fun, X) ->
%    {value, H, T} = ,
%    [Fun(H) | T].


%%%------------------------------------------------------------------
%%%
%%% lens utility
%%%
%%%------------------------------------------------------------------

%%
%% The lens composition is powerful concept to produce complex lenses.
%% The lens type is a functor but functor composition is not natively 
%% supported by Erlang.
%%
%% E.g. there is list of tuple [{1,2,3}], the composition of 
%% fun lens:hd/2, fun lens:t2/2 allows to focus on second element on tuple.
%% the composition is fundamental approach to deal with nested types.
%% 
%%   lens:get(lens:c([fun lens:hd/2, fun lens:t2/2]), [{1,2,3}]).
%%   lens:put(lens:c([fun lens:hd/2, fun lens:t2/2]), 6, [{1,2,3}]).
%%
-spec c([lens()]) -> lens().

c(Lenses) ->
   fun(Fun, S) ->
      c(lists:reverse(Lenses), Fun, S)
   end.

c([Ln], Fun, S) ->
   Ln(Fun, S);
c([Ln | Lenses], Fun, S) ->
   c(Lenses, fun(X) -> Ln(Fun, X) end, S).

%%
%% The composition function is not efficient from performance perspective, 
%% list-based folding is expensive. The efficiency of lens can be improved by 40%
%% using inline variants of `apply`, `get` and `put`.  
%%

%%
%% helper macros to facilitate inline composition
-define(ALnZ, 
   fun(Xy) -> 
      LnZ(fun(Xz) -> fmap(Fun, id(Xz)) end, Xy) 
   end
).

-define(GLnZ, 
   fun(Xy) -> 
      LnZ(fun(Xz) -> fmap(undefined, const(Xz)) end, Xy)
   end
).

-define(C(X, Id, In), 
   fun(X) ->
      Id(In, X)
   end
).


%%
%%
-spec apply(lens(), lens(), fun( (a()) -> a() ), s()) -> s().
-spec apply(lens(), lens(), lens(), fun( (a()) -> a() ), s()) -> s().
-spec apply(lens(), lens(), lens(), lens(), fun( (a()) -> a() ), s()) -> s().
-spec apply(lens(), lens(), lens(), lens(), lens(), fun( (a()) -> a() ), s()) -> s().
-spec apply(lens(), lens(), lens(), lens(), lens(), lens(), fun( (a()) -> a() ), s()) -> s().
-spec apply(lens(), lens(), lens(), lens(), lens(), lens(), lens(), fun( (a()) -> a() ), s()) -> s().


apply(LnY, LnZ, Fun, S) ->
   tl( LnY(?ALnZ, S) ).

apply(LnX, LnY, LnZ, Fun, S) ->
   tl( LnX(?C(Xx, LnY, ?ALnZ), S) ).

apply(LnW, LnX, LnY, LnZ, Fun, S) ->
   tl( LnW(?C(Xw, LnX, ?C(Xx, LnY, ?ALnZ)), S) ).

apply(LnV, LnW, LnX, LnY, LnZ, Fun, S) ->
   tl( LnV(?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?ALnZ))), S) ).

apply(LnU, LnV, LnW, LnX, LnY, LnZ, Fun, S) ->
   tl( LnU(?C(Xu, LnV, ?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?ALnZ)))), S) ).

apply(LnT, LnU, LnV, LnW, LnX, LnY, LnZ, Fun, S) ->
   tl( LnT(?C(Xt, LnU, ?C(Xu, LnV, ?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?ALnZ))))), S) ).

%%
%%
-spec get(lens(), lens(), s()) -> a().
-spec get(lens(), lens(), lens(), s()) -> a().
-spec get(lens(), lens(), lens(), lens(), s()) -> a().
-spec get(lens(), lens(), lens(), lens(), lens(), s()) -> a().
-spec get(lens(), lens(), lens(), lens(), lens(), lens(), s()) -> a().
-spec get(lens(), lens(), lens(), lens(), lens(), lens(), lens(), s()) -> a().

get(LnY, LnZ, S) ->
   tl( LnY(?GLnZ, S) ).

get(LnX, LnY, LnZ, S) ->
   tl( LnX(?C(Xx, LnY, ?GLnZ), S) ).

get(LnW, LnX, LnY, LnZ, S) ->
   tl( LnW(?C(Xw, LnX, ?C(Xx, LnY, ?GLnZ)), S) ).

get(LnV, LnW, LnX, LnY, LnZ, S) ->
   tl( LnV(?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?GLnZ))), S) ).

get(LnU, LnV, LnW, LnX, LnY, LnZ, S) ->
   tl( LnU(?C(Xu, LnV, ?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?GLnZ)))), S) ).

get(LnT, LnU, LnV, LnW, LnX, LnY, LnZ, S) ->
   tl( LnT(?C(Xt, LnU, ?C(Xu, LnV, ?C(Xv, LnW, ?C(Xw, LnX, ?C(Xx, LnY, ?GLnZ))))), S) ).

%%
%%
-spec put(lens(), lens(), a(), s()) -> s().
-spec put(lens(), lens(), lens(), a(), s()) -> s().
-spec put(lens(), lens(), lens(), lens(), a(), s()) -> s().
-spec put(lens(), lens(), lens(), lens(), lens(), a(), s()) -> s().
-spec put(lens(), lens(), lens(), lens(), lens(), lens(), a(), s()) -> s().
-spec put(lens(), lens(), lens(), lens(), lens(), lens(), lens(), a(), s()) -> s().

put(LnY, LnZ, A, S) ->
   apply(LnY, LnZ, fun(_) -> A end, S).

put(LnX, LnY, LnZ, A, S) ->
   apply(LnX, LnY, LnZ, fun(_) -> A end, S).

put(LnW, LnX, LnY, LnZ, A, S) ->
   apply(LnW, LnX, LnY, LnZ, fun(_) -> A end, S).

put(LnV, LnW, LnX, LnY, LnZ, A, S) ->
   apply(LnV, LnW, LnX, LnY, LnZ, fun(_) -> A end, S).

put(LnU, LnV, LnW, LnX, LnY, LnZ, A, S) ->
   apply(LnU, LnV, LnW, LnX, LnY, LnZ, fun(_) -> A end, S).

put(LnT, LnU, LnV, LnW, LnX, LnY, LnZ, A, S) ->
   apply(LnT, LnU, LnV, LnW, LnX, LnY, LnZ, fun(_) -> A end, S).

