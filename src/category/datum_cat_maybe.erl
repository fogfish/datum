%% @doc
%%   category pattern: maybe category
-module(datum_cat_maybe).

-export(['.'/2, expr/1, partial/1]).


%%
%% compose function(s) using AST notation
%%
%% case f(_) of undefined -> undefined ; X -> g(X) end
%%
'.'({maybe, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   VarN = uuid(),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)}, G),
   {maybe, VarN, Expr};

'.'({call, Ln, Ff0, Fa0}, {call, _, _, _} = G) ->
   VarN = uuid(),
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)},
   '.'({maybe, VarN, Expr}, G).

%%
%% 
dot_expr(Ln, VarX, F, G) ->
   {'case', Ln, F, [
      {clause, Ln,
         [{atom, Ln, undefined}],
         [],
         [{atom, Ln, undefined}]
      },
      {clause, Ln, 
         [{var, Ln, VarX}],
         [],
         [G]
      }
   ]}.

%%
%% map compose to expression 
%% 
expr({maybe, _, Expr}) -> 
   Expr.

%%
%% map compose to partial expression
%%
partial({maybe, VarX, {'case', Ln, _, _} = Expr}) ->
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
