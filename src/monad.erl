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
%%   monad parse transform (do-notation syntax)
-module(monad).

-export([parse_transform/2]).


%%%------------------------------------------------------------------
%%%
%%% monad compiler (parse transform)
%%%
%%%------------------------------------------------------------------

%%
%% built-in pattern for abstract syntax tree
-define(FUN(Clauses), 
   {function, Label, Name, Arity, Clauses}).

%  
% -define(MONAD(Monad),  
%    {lc, _, {tuple, _, [{atom, _, do}, Monad]}, _}).
%
% -define(SEQUENCE(Seq),  
%    {lc, _, {tuple, _, [{atom, _, do}, _]}, Seq}).

-define(DO(Monad),  
   {call, _, {atom, _, do}, [{lc, _, Monad, _}]}).

-define(SEQUENCE(Seq),  
   {call, _, {atom, _, do}, [{lc, _, _, Seq}]}).


%%
%% entry-point
parse_transform(AST, _Options) -> 
   pt_function(AST, []).

%%
%% monad syntax requires us only function transformation
pt_function([?FUN(Clauses) | Tail], Acc) ->
   pt_function(Tail, [?FUN(pt_clauses(Clauses)) | Acc]);

pt_function([Head | Tail], Acc) ->
   pt_function(Tail, [Head | Acc]);

pt_function([], Acc) ->
   lists:reverse(Acc).

%%
%% only function body defines do syntax
pt_clauses([{clause, CLine, A1, A2, Code} | Clauses]) ->
   [{clause, CLine, A1, A2, pt_monad(Code)} | pt_clauses(Clauses)];
pt_clauses([]) -> 
   [].

%%
%%
pt_monad([?DO(Monad) = Head | Tail]) -> 
   [cc_do_lc(Monad, Head) | pt_monad(Tail)];

pt_monad(List) ->
   List.

%%
%% compile list comprehension to monad
cc_do_lc(Monad, ?SEQUENCE(Seq)) ->
   cc_do_return(Monad, lists:reverse( pp_var('_', 0, pp_do(Seq)) )).


%%
%% pre-process do sequence to replace 
%% '=<' - return( ... )
%% '>=' - fail(...)
%% '/=' - monad additional function
pp_do([{op, Ln, '=<', Ma, Expr} | Tail]) ->
   [{generate, Ln, Ma, {call, Ln, {atom, Ln, return}, [Expr]}} | pp_do(Tail)];

pp_do([{op, Ln, '>=', Ma, Expr} | Tail]) ->
   [{generate, Ln, Ma, {call, Ln, {atom, Ln, fail}, [Expr]}} | pp_do(Tail)];

pp_do([{op, Ln, '/=', Ma, {call, _, Fun, Expr}} | Tail]) ->
   [{generate, Ln, Ma, {call, Ln, {remote, Ln, '/=', Fun}, Expr}} | pp_do(Tail)];

pp_do([Head | Tail]) ->
   [Head | pp_do(Tail)];

pp_do([]) ->
   [].

%%
%% pre-process do sequence to replace unbound variables _ (cuts)
pp_var(Mv, I, [{generate, Ln, {var, _, '_'}, Expr} | Tail]) ->
   Vx = erlang:list_to_atom("_Vx" ++ erlang:integer_to_list(I)),
   [{generate, Ln, {var, Ln, Vx}, cc_bind_var(Mv, Expr)} | pp_var(Vx, I + 1, Tail)];

pp_var(Mv, I, [{generate, Ln, Head, Expr} | Tail]) ->
   [{generate, Ln, Head, cc_bind_var(Mv, Expr)} | pp_var(Mv, I, Tail)];

pp_var(Mv, I, [Expr | Tail]) ->
   [cc_bind_var(Mv, Expr) | pp_var(Mv, I, Tail)];

pp_var(_, _, []) ->
   [].
      
%%
%% 
cc_do_return(_, [{generate, _, _, _}|_]) ->
   exit("The last statement in a 'do' must be an expression");

cc_do_return(M, [Expr, {generate, _, Ma, _} | _] = Seq) ->
   cc_do_seq(M, lambda(Ma, erlang:element(2, Expr), return(M, Expr)), tl(Seq)).   

cc_do_seq(M, Lambda, [{generate, Ln, _, Expr}, {generate, _, Ma, _} | _] = Seq) ->
   cc_do_seq(M, lambda(Ma, Ln, '>>='(M, Ln, return(M, Expr), Lambda)), tl(Seq));

cc_do_seq(M, Lambda, [{generate, Ln, _, Expr}]) ->
   '>>='(M, Ln, return(M, Expr), Lambda).

%%
%% lift expression to lambda function
lambda(Ma, Ln, Expr) ->
   {'fun', Ln,
      {clauses, 
         [
            {clause, Ln, [Ma], [], [Expr]}
         ]
      }
   }.

%%
%% bind monad
'>>='(Monad, Ln, Expr, Lambda) ->
   {call, Ln, 
      {remote, Ln, Monad, {atom, Ln, '>>='}},
      [Expr, Lambda]
   }.

%%
%% return monad
return(Monad, {call, Ln, {atom, _, return}, Expr}) ->
   {call, Ln,
      {remote, Ln, Monad, {atom, Ln, return}},
      Expr
   };
return(Monad, {call, Ln, {atom, _, fail}, Expr}) ->
   {call, Ln,
      {remote, Ln, Monad, {atom, Ln, fail}},
      Expr
   };

return(Monad, {call, Ln, {remote, _, '/=', Fun}, Expr}) ->
   {call, Ln,
      {remote, Ln, Monad, Fun},
      Expr
   };

return(_, Expr) ->
   Expr.


%%
%%
cc_bind_var(Vx, X)
 when is_tuple(X) ->
   erlang:list_to_tuple(
      cc_bind_var(Vx, erlang:tuple_to_list(X))
   );

cc_bind_var(Vx, [{var, Ln, '_'} | T]) ->
   [{var, Ln, Vx} | cc_bind_var(Vx, T)];
cc_bind_var(Vx, [H | T]) ->
   [cc_bind_var(Vx, H) | cc_bind_var(Vx, T)];
cc_bind_var(_, []) ->
   [];

cc_bind_var(_, X) ->
   X.
   
   


