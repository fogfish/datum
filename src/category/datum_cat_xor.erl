%% @doc
%%   category pattern: xor category
-module(datum_cat_xor).

-export(['.'/2, expr/1, partial/1]).

%%
%% compose function(s) using AST notation
%%
%% case f(_) of {error, _} = Err -> Err ; {ok, X} -> g(X) end
%%
'.'({'xor', VarX, G}, {call, Ln, Ff0, Fa0}) ->
   VarN = uuid(),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, set_blank_variable({var, Ln, VarN}, Fa0)}, G),
   {'xor', VarN, Expr};

'.'({call, Ln, Ff0, Fa0}, {call, _, _, _} = G) ->
   VarN = uuid(),
   Expr = {call, Ln, Ff0, set_blank_variable({var, Ln, VarN}, Fa0)},
   '.'({'xor', VarN, Expr}, G).

%%
%% 
dot_expr(Ln, VarX, F, G) ->
   Err = uuid(),
   {'case', Ln, F, [
      {clause, Ln,
         [{tuple, Ln, [{atom, Ln, ok},{var, Ln, VarX}]}],
         [],
         [G]
      },
      {clause, Ln, 
         [{match, Ln, {tuple, Ln, [{atom, Ln, error}, {var, Ln, '_'}]}, {var, Ln, Err}}],
         [],
         [{var, Ln, Err}]
      }
   ]}.

%%
%% map compose to expression 
%% 
expr({'xor', _, Expr}) -> 
   Expr.

%%
%% map compose to partial expression
%%
partial({'xor', VarX, {'case', Ln, _, _} = Expr}) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, VarX}],
            [],
            [Expr]
         }
      ]}
   }.

%%
%% unique variable
uuid() ->
   list_to_atom("_Vx" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).

%%
%% set blank variable to 
set_blank_variable(X, [{var, _, '_'}|T]) ->
   [X|set_blank_variable(X, T)];

set_blank_variable(X, [H|T]) ->
   [H|set_blank_variable(X, T)];

set_blank_variable(_, []) ->
   [].  
