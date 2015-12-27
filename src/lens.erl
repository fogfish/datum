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
%%   combinator data transformation for pure functional data structure.
%%   The module implementation is derived from prior art by  
%%      
%% @see
%%   Combinators for Bi-Directional Tree Transformations: 
%%   A Linguistic Approach to the View Update Problem by J. Nathan Foster et al.
%%   http://repository.upenn.edu/cgi/viewcontent.cgi?article=1044&context=cis_reports
%%
%%   Erlang Library by Jesper Louis Andersen
%%   https://github.com/jlouis/erl-lenses
%%
%%   Richard A. O'Keefe
%%   http://www.cs.otago.ac.nz/staffpriv/ok/lens.erl
-module(lens).
-compile(inline).
-compile({inline_size, 128}).
-compile(inline_list_funcs).

-export([
   new/1,
   new/2,
   get/2,
   put/3,
   apply/3,
   compose/2,
   compose/1
]).

%%
%% create new lens for given selector
%% @todo: how to handle developer friendly composition
-spec(new/1 :: (any()) -> datum:lens()).

new(L) ->
   make_any(L).

new(tuple, L) ->
   make_tuple(L);
new(list,  L) ->
   make_list(L);
new(keylist, L) ->
   make_keylist(L);
new(map,   L) ->
   make_map(L).

%%
%% 
get({l, Fun, _, _}, X) ->
   Fun(X).

%%
%% pull down
put({l, _, Fun, _}, Val, X) ->
   Fun(Val, X).

%%
%% @todo: apply(Fun, Key, T) ->
apply({l, _, _, Fun}, Fxx, X) ->
   Fun(Fxx, X).

%% 
%% lens composition
-spec(compose/2 :: (datum:lens(), datum:lens()) -> datum:lens()).
-spec(compose/1 :: ([datum:lens()]) -> datum:lens()).

compose(A, B) ->
   {l, compose_get(A, B), compose_put(A, B), compose_apply(A, B)}.

compose_get({l, A, _, _}, {l, B, _, _}) ->
   fun(X) -> 
      B( A(X) ) 
   end.

compose_put({l, G, A, _}, {l, _, B, _}) ->
   fun(Val, X) -> 
      A( B(Val, G(X)), X) 
   end.

compose_apply({l, _, _, A}, {l, _, _, B}) ->
   fun(Fun, X) -> 
      A(fun(Y) -> B(Fun, Y) end, X) 
   end.

compose([A,B|T]) ->
   compose([compose(A,B)|T]);
compose([T]) ->
   T.      


%%%------------------------------------------------------------------
%%%
%%% tuple 
%%%
%%%------------------------------------------------------------------
make_tuple(L) ->
   {l, tuple_get(L), tuple_put(L), tuple_apply(L)}.

%%
tuple_get(L)
 when is_integer(L) ->
   fun(X) -> tuple_get_int(L, X) end;
tuple_get(L)
 when is_function(L) ->
   fun(X) -> tuple_get_fun(L, 1, X) end.

tuple_get_int(L, X) ->
   erlang:element(L, X).

tuple_get_fun(Pred, I, X) ->
   H = erlang:element(I, X),
   case Pred(H) of
      true  -> H;
      false -> tuple_get_fun(Pred, I + 1, X)
   end.  

%%
tuple_put(L)
 when is_integer(L) ->
   fun(Val, X) -> tuple_put_int(L, Val, X) end;
tuple_put(L)
 when is_function(L) ->
   fun(Val, X) -> tuple_put_fun(L, Val, 1, X) end.

tuple_put_int(L, Val, X) ->
   erlang:setelement(L, X, Val).

tuple_put_fun(Pred, Val, I, X) ->
   case Pred(erlang:element(I, X)) of
      true  -> erlang:setelement(I, X, Val);
      false -> tuple_put_fun(Pred, Val, I + 1, X)
   end.

%%
tuple_apply(L)
 when is_integer(L) ->
   fun(Fun, X) -> tuple_apply_int(L, Fun, X) end;
tuple_apply(L)
 when is_function(L) ->
   fun(Fun, X) -> tuple_apply_fun(L, Fun, 1, X) end.


tuple_apply_int(L, Fun, X) ->
   erlang:setelement(L, X, Fun(erlang:element(L, X))).

tuple_apply_fun(Pred, Fun, I, X) ->
   H = erlang:element(I, X),
   case Pred(H) of
      true  -> erlang:setelement(I, X, Fun(H));
      false -> tuple_put_fun(Pred, Fun, I + 1, X)
   end.

%%%------------------------------------------------------------------
%%%
%%% map
%%%
%%%------------------------------------------------------------------
make_map(L) ->
   {l, map_get(L), map_put(L), map_apply(L)}.

%%
map_get(L)
 when not is_function(L) ->
   fun(X) -> map_get_key(L, X) end;
map_get(L)
 when is_function(L) ->
   fun(X) -> map_get_fun(L, X) end.

map_get_key(L, X) ->
   maps:get(L, X).

map_get_fun(Pred, X) ->
   list_get_fun(Pred, maps:to_list(X)).

%%
map_put(L)
 when not is_function(L) ->
   fun(Val, X) -> map_put_key(L, Val, X) end;
map_put(L)
 when is_function(L) ->
   fun(Val, X) -> map_put_fun(L, Val, X) end.

map_put_key(L, Val, X) ->
   maps:put(L, Val, X).

map_put_fun(Pred, Val, X) ->
   maps:from_list(list_put_fun(Pred, Val, maps:to_list(X))).


%%
map_apply(L)
 when not is_function(L) ->
   fun(Fun, X) -> map_apply_key(L, Fun, X) end;
map_apply(L)
 when is_function(L) ->
   fun(Fun, X) -> map_apply_fun(L, Fun, X) end.


map_apply_key(L, Fun, X) ->
   maps:put(L, Fun(maps:get(L, X)), X).

map_apply_fun(Pred, Fun, X) ->
   maps:from_list(list_apply_fun(Pred, Fun, maps:to_list(X))).

%%%------------------------------------------------------------------
%%%
%%% list
%%%
%%%------------------------------------------------------------------
make_list(L) ->
   {l, list_get(L), list_put(L), list_apply(L)}.

%%
list_get(L)
 when is_integer(L) ->
   fun(X) -> list_get_int(L, X) end;
list_get(L)
 when is_function(L) ->
   fun(X) -> list_get_fun(L, X) end.
   
list_get_int(1, [H|_]) -> 
   H;
list_get_int(L, [_|T])
 when L > 1 ->
   list_get_int(L - 1, T).

list_get_fun(Pred, [H|T]) ->
   case Pred(H) of
      true  -> H;
      false -> list_get_fun(Pred, T)
   end.

%%
list_put(L)
 when is_integer(L) ->
   fun(Val, X) -> list_put_int(L, Val, X) end;
list_put(L)
 when is_function(L) ->
   fun(Val, X) -> list_put_fun(L, Val, X) end.

list_put_int(1, Val, [_|T]) -> 
   [Val|T];
list_put_int(N, Val, [H|T])
 when N > 1 ->
   [H | list_put_int(N - 1, Val, T)].

list_put_fun(Pred, Val, [H|T]) ->
   case Pred(H) of
      true  -> [Val|T];
      false -> [H|list_put_fun(Pred, Val, T)]
   end.

%%
list_apply(L)
 when is_integer(L) ->
   fun(Fun, X) -> list_apply_int(L, Fun, X) end;
list_apply(L)
 when is_function(L) ->
   fun(Fun, X) -> list_apply_fun(L, Fun, X) end.

list_apply_int(1, Fun, [H|T]) -> 
   [Fun(H)|T];
list_apply_int(N, Fun, [H|T])
 when N > 1 ->
   [H | list_apply_int(N - 1, Fun, T)].

list_apply_fun(Pred, Fun, [H|T]) ->
   case Pred(H) of
      true  -> [Fun(H)|T];
      false -> [H|list_apply_fun(Pred, Fun, T)]
   end.

%%%------------------------------------------------------------------
%%%
%%% keylist
%%%
%%%------------------------------------------------------------------
make_keylist(L) ->
   {l, keylist_get(L), keylist_put(L), keylist_apply(L)}.

%%
keylist_get(L)
 when not is_function(L) ->
   fun(X) -> keylist_get_key(L, X) end;
keylist_get(L)
 when is_function(L) ->
   fun(X) -> list_get_fun(L, X) end.
   
keylist_get_key({N, Key}, X) ->
   keylist_get_key(N, Key, X);
keylist_get_key(Key, X) ->
   keylist_get_key(1, Key, X).

keylist_get_key(N, Key, X) ->
   case lists:keyfind(Key, N, X) of
      false -> exit(badarg);
      H     -> H
   end.

%%
keylist_put(L)
 when not is_function(L) ->
   fun(Val, X) -> keylist_put_key(L, Val, X) end;
keylist_put(L)
 when is_function(L) ->
   fun(Val, X) -> list_put_fun(L, Val, X) end.

keylist_put_key({N, Key}, Val, X) ->
   lists:keystore(Key, N, X, Val);
keylist_put_key(Key, Val, X) ->
   lists:keystore(Key, 1, X, Val).


%%
keylist_apply(L)
 when not is_function(L) ->
   fun(Fun, X) -> keylist_apply_key(L, Fun, X) end;
keylist_apply(L)
 when is_function(L) ->
   fun(Fun, X) -> list_apply_fun(L, Fun, X) end.

keylist_apply_key({N, Key}, Fun, X) ->
   {value, H, T} = lists:keytake(Key, N, X),
   [Fun(H) | T];
keylist_apply_key(Key, Fun, X) ->
   {value, H, T} = lists:keytake(Key, 1, X),
   [Fun(H) | T].

%%%------------------------------------------------------------------
%%%
%%% any 
%%%
%%%------------------------------------------------------------------
make_any(L) ->
   {l, any_get(L), any_put(L), any_apply(L)}.

%%
any_get(L)
 when not is_function(L) ->
   fun
      (X) when is_map(X) -> map_get_key(L, X);  
      (X) when is_tuple(X) -> tuple_get_int(L, X);
      (X) when is_list(X) -> list_get_int(L, X)  
   end;

any_get(L) ->
   fun
      (X) when is_map(X) -> map_get_fun(L, X);
      (X) when is_tuple(X) -> tuple_get_fun(L, 1, X);
      (X) when is_list(X)  -> list_get_fun(L, X)  
   end.

%%
any_put(L)
 when not is_function(L) ->
   fun
      (Val, X) when is_map(X) -> map_put_key(L, Val, X);
      (Val, X) when is_tuple(X) -> tuple_put_int(L, X, Val);
      (Val, X) when is_list(X) -> list_put_int(L, Val, X) 
   end;
any_put(L) ->
   fun
      (Val, X) when is_map(X) -> map_put_fun(L, Val, X);
      (Val, X) when is_tuple(X) -> tuple_put_fun(L, X, 1, Val);
      (Val, X) when is_list(X) -> list_put_fun(L, Val, X) 
   end.

%%
any_apply(L)
 when not is_function(L) ->
   fun
      (Fun, X) when is_map(X) -> map_apply_key(L, Fun, X);
      (Fun, X) when is_tuple(X) -> tuple_apply_int(L, Fun, X);
      (Fun, X) when is_list(X) -> list_apply_fun(L, Fun, X)
   end;
any_apply(L) ->
   fun
      (Fun, X) when is_map(X) -> map_apply_fun(L, Fun, X);
      (Fun, X) when is_tuple(X) -> tuple_apply_fun(L, Fun, 1, X);
      (Fun, X) when is_list(X) -> list_apply_fun(L, Fun, X)
   end.
