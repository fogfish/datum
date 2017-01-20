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
%% check if code is category pattern (functional comprehension)
is_category({char, _, $.}) ->
   function;
is_category({char, _, $?}) ->
   maybe;
is_category({char, _, $^}) ->
   'xor';
is_category(_) ->
   false.

%%
%% apply category composition operator 
category_compose_with(Fun, [F, {call, _, _, _} = G | T]) ->
   category_compose_with(Fun, [Fun(F, G)|T]);
category_compose_with(_, [Expr]) ->
   Expr.

%%
%%
category_compose_partial_with(Fun, [{call, Line, Ff0, Fa0}|T]) ->
   Var = uuid(),
   Fa1 = set_blank_variable({var, Line, Var}, Fa0),
   G   = {call, Line, Ff0, Fa1},
   {'fun', Line,
      {clauses, [
         {clause, Line,
            [{var, Line, Var}],
            [],
            [category_compose_with(Fun, [G|T])]
         }
      ]}
   }.

%%
%%
is_category_partial([{call, _, _, Fa0} | _]) ->
   length( lists:filter(fun({var, _, '_'}) -> true; (_) -> false end, Fa0) ) > 0.


%%
%% apply category pattern
category(function, Expr0) ->
   Expr1 = function_composition(Expr0),
   case is_category_partial(Expr1) of
      true ->
         category_compose_partial_with(fun compose_function/2, Expr1);
      false ->
         category_compose_with(fun compose_function/2, Expr1)
   end;

category(maybe, Expr0) ->
   Expr1 = function_composition(Expr0),
   case is_category_partial(Expr1) of
      true ->
         Line = erlang:element(2, hd(Expr0)),
         {Var, Expr2} = category_compose_with(fun compose_maybe/2, lists:reverse(Expr1)),
         {'fun', Line,
            {clauses, [
               {clause, Line,
                  [{var, Line, Var}],
                  [],
                  [Expr2]
               }
            ]}
         };
      false ->
         {_, Expr2} = category_compose_with(fun compose_maybe/2, lists:reverse(Expr1)),
         Expr2
   end;

category('xor', Expr0) ->
   Expr1 = function_composition(Expr0),
   case is_category_partial(Expr1) of
      true ->
         Line = erlang:element(2, hd(Expr0)),
         {Var, Expr2} = category_compose_with(fun compose_xor/2, lists:reverse(Expr1)),
         {'fun', Line,
            {clauses, [
               {clause, Line,
                  [{var, Line, Var}],
                  [],
                  [Expr2]
               }
            ]}
         };
      false ->
         {_, Expr2} = category_compose_with(fun compose_xor/2, lists:reverse(Expr1)),
         Expr2
   end.

%%
%% compose operator in function category 
compose_function({call, _, _, _} = F, {call, Line, Gf0, Ga0}) ->
   %% rewrite f(_) . g(_) to g(f(_)) without runtime overhead 
   {call, Line, Gf0, set_blank_variable(F, Ga0)}.

%%
%% compose operator in maybe category
compose_maybe({call, Line, Ff0, Fa0}, {call, _, _, _} = G) ->
   Var = uuid(),
   compose_maybe({Var, {call, Line, Ff0, set_blank_variable({var, Line, Var}, Fa0)}}, G);

compose_maybe({V0, F}, {call, Line, Gf0, Ga0}) ->
   Var = uuid(),
   I = {'case', Line, {call, Line, Gf0, set_blank_variable({var, Line, Var}, Ga0)}, [
      {clause, Line,
         [{atom, Line, undefined}],
         [],
         [{atom, Line, undefined}]
      },
      {clause, Line, 
         [{var, Line, V0}],
         [],
         [F]
      }
   ]},
   {Var, I}.

%%
%% compose operator in xor category
compose_xor({call, Line, Ff0, Fa0}, {call, _, _, _} = G) ->
   Var = uuid(),
   compose_xor({Var, {call, Line, Ff0, set_blank_variable({var, Line, Var}, Fa0)}}, G);

compose_xor({V0, F}, {call, Line, Gf0, Ga0}) ->
   Var = uuid(),
   Err = uuid(),
   I = {'case', Line, {call, Line, Gf0, set_blank_variable({var, Line, Var}, Ga0)}, [
      {clause, Line, 
         [{tuple, Line, [{atom, Line, ok},{var, Line, V0}]}],
         [],
         [F]
      },
      {clause, Line,
         [{match, Line, {tuple,Line, [{atom, Line, error},{var, Line, '_'}]}, {var, Line, Err}}],
         [],
         [{var, Line, Err}]
      }
   ]},
   {Var, I}.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% unique variable
uuid() ->
   list_to_atom("Var" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).   


%%
%% set blank variable to 
set_blank_variable(X, [{var, _, '_'}|T]) ->
   [X|set_blank_variable(X, T)];

set_blank_variable(X, [H|T]) ->
   [H|set_blank_variable(X, T)];

set_blank_variable(_, []) ->
   [].  


%%
%% convert expression to function composition (f . g . h ...)
function_composition([{call, _, _, _} = H | T]) ->
   [H | function_composition(T)];

%% function reference 
function_composition([{'fun', Line, {function, Id, _}} | T]) ->
   [{call, Line, {atom, Line, Id}, [{var, Line, '_'}]} | function_composition(T)];

function_composition([{'fun', Line, {function, Mod, Fun, _}} | T]) ->
   [{call, Line, {remote, Line, Mod, Fun}, [{var, Line, '_'}]} | function_composition(T)];

%% inline function
function_composition([{'fun', Line, {clauses, _}} = H | T]) ->
   [{call, Line, H, [{var, Line, '_'}]} | function_composition(T)];

%% function reference within variable
function_composition([{var, Line, _} = H | T]) ->
   [{call, Line, H, [{var, Line, '_'}]} | function_composition(T)];

function_composition([H | _]) ->
   exit( lists:flatten(io_lib:format("Function composition do not support the expression:~n~p~n", [H])) );

function_composition([]) ->
   [].


