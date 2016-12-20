%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
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
%%   functional programming macro(s)
-module(datum_macro).

-export([
   is_category/1, category/2
]).


%%%------------------------------------------------------------------
%%%
%%% category pattern
%%%
%%%------------------------------------------------------------------   

%%
%% check if code is category pattern
is_category({char, _, $.}) ->
   function;
is_category(_) ->
   false.

%%
%% apply category pattern
category(function, Expr0) ->
   Expr1 = category_function(Expr0),
   case is_category_function_partial(Expr1) of
      true ->
         category_function_partial_compose(Expr1);
      false ->
         category_function_compose(Expr1)
   end.

%%
%% built-in functional composition f . g
category_function([{call, _, _, _} = H | T]) ->
   [H | category_function(T)];
category_function([{'fun', Line, {function, Id, _}} | T]) ->
   [{call, Line, {atom, Line, Id}, [{var, Line, '_'}]} | category_function(T)];
category_function([{'fun', Line, {function, Mod, Fun, _}}| T]) ->
   [{call, Line, {remote, Line, Mod, Fun}, [{var, Line, '_'}]} | category_function(T)];
category_function([H | _]) ->
   Msg = io_lib:format("The category pattern do not support~n~p~n", [H]),
   exit(Msg);
category_function([]) ->
   [].

%%
category_function_compose([{call, _, _, _} = F, {call, Line, Gf0, Ga0} | T]) ->
   Ga1 = set_blank_variable(F, Ga0),
   G   = {call, Line, Gf0, Ga1},
   category_function_compose([G|T]);
category_function_compose([Expr]) ->
   Expr.

%%
category_function_partial_compose([{call, Line, Ff0, Fa0}|T]) ->
   Fa1 = set_blank_variable({var, Line, 'VCatX'}, Fa0),
   G   = {call, Line, Ff0, Fa1},
   {'fun', Line,
      {clauses, [
         {clause, Line,
            [{var, Line, 'VCatX'}],
            [],
            [category_function_compose([G|T])]
         }
      ]}
   }.

%%
is_category_function_partial([{call, _, _, Fa0} | _]) ->
   length( lists:filter(fun({var, _, '_'}) -> true; (_) -> false end, Fa0) ) > 0.





%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%%
set_blank_variable(X, [{var, _, '_'}|T]) ->
   [X|set_blank_variable(X, T)];
set_blank_variable(X, [H|T]) ->
   [H|set_blank_variable(X, T)];
set_blank_variable(_, []) ->
   [].  
