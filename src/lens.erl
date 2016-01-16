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
%% Lenses resembles concept of getters and setters, which you can compose 
%% using functional concepts   
%%
-module(lens).

%% naive lens interface
-export([nget/2, nput/3, napply/3, naive/1]).
%% lens primitives
-export([fmap/2, apply/3, get/2, put/3]). 
%% lenses  
-export([hd/2, tl/2]).
-export([t1/2, t2/2, t3/2, tuple/1]).
%% lens utility

-compile({no_auto_import,[apply/3]}).
-compile([export_all]).

-compile(inline).
-compile({inline_size, 128}).
-compile(inline_list_funcs).


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
   apply(Ln, fun(_) -> Val end, S).

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
   [Fun(Val) | List].

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
%%
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

apply(Ln, F, S) 
 when is_function(Ln), is_function(F) ->
   tl( Ln(fun(X) -> fmap(F, id(X)) end, S) ).

% helper function
%
% over(Ln1, Ln2, Ln3, F, S) ->
%    L = fun(X) -> fmap(F, id(X)) end,
%    tl( Ln1(fun(K) -> Ln2(fun(Z) -> Ln3(L, Z) end, K) end, S) ).


%%
%% The `get` is defined as
%% Given a lens() that focuses on a() inside of s(), and
%% instance of object s(). It returns value of focus point a(). 
%% e.g. Haskell use following notation
%%  view :: Lens s a -> s -> a
%%
-spec get(lens(), s()) -> a().

get(Ln, S) ->
   tl( Ln(fun(X) -> fmap(a, const(X)) end, S) ).

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
%%
-spec hd(fun( (_) -> f(_) ), list() ) -> f(list()).

hd(Fun, [H|T]) ->
   fmap(fun(X) -> [X|T] end, Fun(H)).

%%
%%
-spec tl(fun( (list()) -> f(list()) ), list() ) -> f(list()).

tl(Fun, [H|T]) ->
   fmap(fun(X) -> [H|X] end, Fun(T)).

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



% In order to use lenses we actually need to have some lenses. 
% As said earlier, we do not need the lens library to define a new lens, 
% we only need a function with the type of . Let’s make one!

% We’ll start by implementing the _1 lens, which focuses on a first element of a pair. The type will be Lens (a,b) a or specifically Functor f => (a -> f a) -> (a,b) -> f (a,b), in another words Given a pair of (a,b) the lens focuses on the first element of the pair, which is a.


% _1 :: Functor f => (a -> f a) -> (a,b) -> f (a,b)
% _1 f (x,y) = fmap (\a -> (a, y)) (f x)

% % works
% t1(F, {X, Y}) ->
%    fmap(fun(A) -> {A, Y} end, F(X)). 


%%%------------------------------------------------------------------
%%%
%%% lens utility
%%%
%%%------------------------------------------------------------------

%%
%% The lens composition is powerful concept to produce complex lenses.
%% The lens type is a functor but functor composition is not natively 
%% supported by Erlang.

c(Lenses) ->
   fun(F, S) ->
      c(Lenses, F, S)
   end.

c([Ln], F, S) ->
   Ln(F, S);
c([Ln | Lenses], F, S) ->
   c(Lenses, fun(X) -> Ln(F, X) end, S).




% alternative (use curring) 
% t1(F) ->
%    fun({X, Y}) ->
%       fmap(fun(A) -> {A, Y} end, F(X))
%    end.

% % experemental
% t1f(F, {X, Y}) ->
%    Functor = F(X),
%    Fmap    = id(Functor),
%    Fmap(fun(A) -> {A, Y} end).


%% how to compose functors ?
%% fmap(a, f * g) = fmap( fmap(g, a), f )

%% :t fmap . fmap
%% (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)

% compose_apply({l, _, _, A}, {l, _, _, B}) ->
%    fun(Fun, X) -> 
%       A(fun(Y) -> B(Fun, Y) end, X) 
%    end.

t11(F, X) ->
   t1(fun(Z) -> t1(F, Z) end, X). %% <- this is right composition how to use list?

t111(F, X) ->
   t1(fun(K) -> t1(fun(Z) -> t1(F, Z) end, K) end, X).

c1(F1, F2, F3) ->
   fun(F, X) ->
      F1(fun(K) -> F2(fun(Z) -> F3(F, Z) end, K) end, X)     
   end.





%%
%% stress

      


% composable example
% t11t(F) ->
%    t1(fun(Z) -> t1(F, Z) end).



%lens:over(fun lens:t11/2, fun(X) -> 4 end, {{5,1},2}).




% %% @doc
% %%   combinator data transformation for pure functional data structure.
% %%   The module implementation is derived from prior art by  
% %%      
% %% @see
% %%   Combinators for Bi-Directional Tree Transformations: 
% %%   A Linguistic Approach to the View Update Problem by J. Nathan Foster et al.
% %%   http://repository.upenn.edu/cgi/viewcontent.cgi?article=1044&context=cis_reports
% %%
% %%   Jakub Arnold
% %%   http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html
% %%
% %%   Erlang Library by Jesper Louis Andersen
% %%   https://github.com/jlouis/erl-lenses
% %%
% %%   Richard A. O'Keefe
% %%   http://www.cs.otago.ac.nz/staffpriv/ok/lens.erl


% -export([
%    new/1,
%    new/2,
%    get/2,
%    put/3,
%    apply/3,
%    compose/2,
%    compose/1,
%    t/1
% ]).

% %% The thing we are missing is "failing" lenses where they return
% %% something of term Omega. If we had this, we could handle the case
% %% by building up the structure from "nothing". But as it currently
% %% stands, we can't. ==> may-be update can provide Omega

% %%
% %% lens:tuple(1) <- as factory instead of new()
% %%
% %% it can be extended to supply initial value
% %% lens:map(<<"key">>, #{})
% %%
% %% composition might know (next type to automate recovery Omega space)
% %% lense:compose([ lens:map(...), lens:keylist(...) ])



% %%
% %% create new lens for given selector
% %% @todo: how to handle developer friendly composition
% -spec(new/1 :: (any()) -> datum:lens()).

% new(L) ->
%    make_any(L).

% new(tuple, L) ->
%    make_tuple(L);
% new(list,  L) ->
%    make_list(L);
% new(keylist, L) ->
%    make_keylist(L);
% new(map,   L) ->
%    make_map(L).

% %%
% %% 
% get({l, Fun, _, _}, X) ->
%    Fun(X).

% %% rename to set ?
% %% pull down (set ?) -> what if set is identity of apply
% %% put(Val, X) -> apply(.., fun(_) -> X end)
% %% need to benchmark !?
% put({l, _, Fun, _}, Val, X) ->
%    Fun(Val, X).

% %%
% %% @todo: apply(Fun, Key, T) ->
% apply({l, _, _, Fun}, Fxx, X) ->
%    Fun(Fxx, X).

% %% 
% %% lens composition
% -spec(compose/2 :: (datum:lens(), datum:lens()) -> datum:lens()).
% -spec(compose/1 :: ([datum:lens()]) -> datum:lens()).

% compose(A, B) ->
%    {l, compose_get(A, B), compose_put(A, B), compose_apply(A, B)}.

% compose_get({l, A, _, _}, {l, B, _, _}) ->
%    fun(X) -> 
%       B( A(X) ) 
%    end.

% compose_put({l, G, A, _}, {l, _, B, _}) ->
%    fun(Val, X) -> 
%       A( B(Val, G(X)), X) 
%    end.

% compose_apply({l, _, _, A}, {l, _, _, B}) ->
%    fun(Fun, X) -> 
%       A(fun(Y) -> B(Fun, Y) end, X) 
%    end.

% compose([A,B|T]) ->
%    compose([compose(A,B)|T]);
% compose([T]) ->
%    T.      


% %%%------------------------------------------------------------------
% %%%
% %%% tuple 
% %%%
% %%%------------------------------------------------------------------
% make_tuple(L) ->
%    {l, tuple_get(L), tuple_put(L), tuple_apply(L)}.

% %%
% tuple_get(L)
%  when is_integer(L) ->
%    fun(X) -> tuple_get_int(L, X) end;
% tuple_get(L)
%  when is_function(L) ->
%    fun(X) -> tuple_get_fun(L, 1, X) end.

% tuple_get_int(L, X) ->
%    erlang:element(L, X).

% tuple_get_fun(Pred, I, X) ->
%    H = erlang:element(I, X),
%    case Pred(H) of
%       true  -> H;
%       false -> tuple_get_fun(Pred, I + 1, X)
%    end.  

% %%
% tuple_put(L)
%  when is_integer(L) ->
%    fun(Val, X) -> tuple_put_int(L, Val, X) end;
% tuple_put(L)
%  when is_function(L) ->
%    fun(Val, X) -> tuple_put_fun(L, Val, 1, X) end.

% tuple_put_int(L, Val, X) ->
%    erlang:setelement(L, X, Val).

% tuple_put_fun(Pred, Val, I, X) ->
%    case Pred(erlang:element(I, X)) of
%       true  -> erlang:setelement(I, X, Val);
%       false -> tuple_put_fun(Pred, Val, I + 1, X)
%    end.

% %%
% tuple_apply(L)
%  when is_integer(L) ->
%    fun(Fun, X) -> tuple_apply_int(L, Fun, X) end;
% tuple_apply(L)
%  when is_function(L) ->
%    fun(Fun, X) -> tuple_apply_fun(L, Fun, 1, X) end.


% tuple_apply_int(L, Fun, X) ->
%    erlang:setelement(L, X, Fun(erlang:element(L, X))).

% tuple_apply_fun(Pred, Fun, I, X) ->
%    H = erlang:element(I, X),
%    case Pred(H) of
%       true  -> erlang:setelement(I, X, Fun(H));
%       false -> tuple_put_fun(Pred, Fun, I + 1, X)
%    end.

% %%%------------------------------------------------------------------
% %%%
% %%% map
% %%%
% %%%------------------------------------------------------------------
% make_map(L) ->
%    {l, map_get(L), map_put(L), map_apply(L)}.

% %%
% map_get(L)
%  when not is_function(L) ->
%    fun(X) -> map_get_key(L, X) end;
% map_get(L)
%  when is_function(L) ->
%    fun(X) -> map_get_fun(L, X) end.

% map_get_key(L, X) ->
%    maps:get(L, X, #{}). %% how to handle not found in lens (may be xfork) ?
%    % maps:get(L, X).

% map_get_fun(Pred, X) ->
%    list_get_fun(Pred, maps:to_list(X)).

% %%
% map_put(L)
%  when not is_function(L) ->
%    fun(Val, X) -> map_put_key(L, Val, X) end;
% map_put(L)
%  when is_function(L) ->
%    fun(Val, X) -> map_put_fun(L, Val, X) end.

% map_put_key(L, Val, X) ->
%    maps:put(L, Val, X).

% map_put_fun(Pred, Val, X) ->
%    maps:from_list(list_put_fun(Pred, Val, maps:to_list(X))).


% %%
% map_apply(L)
%  when not is_function(L) ->
%    fun(Fun, X) -> map_apply_key(L, Fun, X) end;
% map_apply(L)
%  when is_function(L) ->
%    fun(Fun, X) -> map_apply_fun(L, Fun, X) end.


% map_apply_key(L, Fun, X) ->
%    maps:put(L, Fun(maps:get(L, X)), X).

% map_apply_fun(Pred, Fun, X) ->
%    maps:from_list(list_apply_fun(Pred, Fun, maps:to_list(X))).

% %%%------------------------------------------------------------------
% %%%
% %%% list
% %%%
% %%%------------------------------------------------------------------
% make_list(L) ->
%    {l, list_get(L), list_put(L), list_apply(L)}.

% %%
% list_get(L)
%  when is_integer(L) ->
%    fun(X) -> list_get_int(L, X) end;
% list_get(L)
%  when is_function(L) ->
%    fun(X) -> list_get_fun(L, X) end.
   
% list_get_int(1, [H|_]) -> 
%    H;
% list_get_int(L, [_|T])
%  when L > 1 ->
%    list_get_int(L - 1, T).

% list_get_fun(Pred, [H|T]) ->
%    case Pred(H) of
%       true  -> H;
%       false -> list_get_fun(Pred, T)
%    end.

% %%
% list_put(L)
%  when is_integer(L) ->
%    fun(Val, X) -> list_put_int(L, Val, X) end;
% list_put(L)
%  when is_function(L) ->
%    fun(Val, X) -> list_put_fun(L, Val, X) end.

% list_put_int(1, Val, [_|T]) -> 
%    [Val|T];
% list_put_int(N, Val, [H|T])
%  when N > 1 ->
%    [H | list_put_int(N - 1, Val, T)].

% list_put_fun(Pred, Val, [H|T]) ->
%    case Pred(H) of
%       true  -> [Val|T];
%       false -> [H|list_put_fun(Pred, Val, T)]
%    end.

% %%
% list_apply(L)
%  when is_integer(L) ->
%    fun(Fun, X) -> list_apply_int(L, Fun, X) end;
% list_apply(L)
%  when is_function(L) ->
%    fun(Fun, X) -> list_apply_fun(L, Fun, X) end.

% list_apply_int(1, Fun, [H|T]) -> 
%    [Fun(H)|T];
% list_apply_int(N, Fun, [H|T])
%  when N > 1 ->
%    [H | list_apply_int(N - 1, Fun, T)].

% list_apply_fun(Pred, Fun, [H|T]) ->
%    case Pred(H) of
%       true  -> [Fun(H)|T];
%       false -> [H|list_apply_fun(Pred, Fun, T)]
%    end.

% %%%------------------------------------------------------------------
% %%%
% %%% keylist
% %%%
% %%%------------------------------------------------------------------
% make_keylist(L) ->
%    {l, keylist_get(L), keylist_put(L), keylist_apply(L)}.

% %%
% keylist_get(L)
%  when not is_function(L) ->
%    fun(X) -> keylist_get_key(L, X) end;
% keylist_get(L)
%  when is_function(L) ->
%    fun(X) -> list_get_fun(L, X) end.
   
% keylist_get_key({N, Key}, X) ->
%    keylist_get_key(N, Key, X);
% keylist_get_key(Key, X) ->
%    keylist_get_key(1, Key, X).

% keylist_get_key(N, Key, X) ->
%    case lists:keyfind(Key, N, X) of
%       false -> exit(badarg);
%       H     -> H
%    end.

% %%
% keylist_put(L)
%  when not is_function(L) ->
%    fun(Val, X) -> keylist_put_key(L, Val, X) end;
% keylist_put(L)
%  when is_function(L) ->
%    fun(Val, X) -> list_put_fun(L, Val, X) end.

% keylist_put_key({N, Key}, Val, X) ->
%    lists:keystore(Key, N, X, Val);
% keylist_put_key(Key, Val, X) ->
%    lists:keystore(Key, 1, X, Val).


% %%
% keylist_apply(L)
%  when not is_function(L) ->
%    fun(Fun, X) -> keylist_apply_key(L, Fun, X) end;
% keylist_apply(L)
%  when is_function(L) ->
%    fun(Fun, X) -> list_apply_fun(L, Fun, X) end.

% keylist_apply_key({N, Key}, Fun, X) ->
%    {value, H, T} = lists:keytake(Key, N, X),
%    [Fun(H) | T];
% keylist_apply_key(Key, Fun, X) ->
%    {value, H, T} = lists:keytake(Key, 1, X),
%    [Fun(H) | T].

% %%%------------------------------------------------------------------
% %%%
% %%% any 
% %%%
% %%%------------------------------------------------------------------
% make_any(L) ->
%    {l, any_get(L), any_put(L), any_apply(L)}.

% %%
% any_get(L)
%  when not is_function(L) ->
%    fun
%       (X) when is_map(X) -> map_get_key(L, X);  
%       (X) when is_tuple(X) -> tuple_get_int(L, X);
%       (X) when is_list(X) -> list_get_int(L, X)  
%    end;

% any_get(L) ->
%    fun
%       (X) when is_map(X) -> map_get_fun(L, X);
%       (X) when is_tuple(X) -> tuple_get_fun(L, 1, X);
%       (X) when is_list(X)  -> list_get_fun(L, X)  
%    end.

% %%
% any_put(L)
%  when not is_function(L) ->
%    fun
%       (Val, X) when is_map(X) -> map_put_key(L, Val, X);
%       (Val, X) when is_tuple(X) -> tuple_put_int(L, X, Val);
%       (Val, X) when is_list(X) -> list_put_int(L, Val, X) 
%    end;
% any_put(L) ->
%    fun
%       (Val, X) when is_map(X) -> map_put_fun(L, Val, X);
%       (Val, X) when is_tuple(X) -> tuple_put_fun(L, X, 1, Val);
%       (Val, X) when is_list(X) -> list_put_fun(L, Val, X) 
%    end.

% %%
% any_apply(L)
%  when not is_function(L) ->
%    fun
%       (Fun, X) when is_map(X) -> map_apply_key(L, Fun, X);
%       (Fun, X) when is_tuple(X) -> tuple_apply_int(L, Fun, X);
%       (Fun, X) when is_list(X) -> list_apply_fun(L, Fun, X)
%    end;
% any_apply(L) ->
%    fun
%       (Fun, X) when is_map(X) -> map_apply_fun(L, Fun, X);
%       (Fun, X) when is_tuple(X) -> tuple_apply_fun(L, Fun, 1, X);
%       (Fun, X) when is_list(X) -> list_apply_fun(L, Fun, X)
%    end.


% t(List) ->
%    lists:foldl(
%       fun(X, Acc) ->
%          L = lens:compose([lens:new(map, X) 
%             || X <- binary:split(erlang:list_to_binary(X), <<$->>, [global])
%          ]),
%          lens:put(L, #{}, Acc)
%       end,
%       #{},
%       List
%    ).
