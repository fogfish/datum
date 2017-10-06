%% @doc
%%   category pattern: option category
-module(datum_cat_option).

-export(['.'/2, fmap/1, expr/1, partial/1]).


%%
%% compose function(s) using AST notation
%%
%% case f(_) of undefined -> undefined ; X -> g(X) end
%%
'.'({option, VarX, G}, {call, Ln, Ff0, Fa0} = F) ->
   VarN = uuid(),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)}, G),
   {option, VarN, Expr};

'.'({option, VarX, G}, {generate, Ln, {var, _, VarN}, F}) ->
   Expr = dot_expr(Ln, VarN, datum_cat:cc_bind_var({var, Ln, VarX}, F), G),
   {option, VarX, Expr};

'.'({call, Ln, Ff0, Fa0}, G) ->
   VarN = uuid(),
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)},
   '.'({option, VarN, Expr}, G).


% '.'({option, VarX, G}, {call, Ln, Ff0, Fa0}) ->
%    VarN = uuid(),
%    Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)}, G),
%    {option, VarN, Expr};

% '.'({call, Ln, Ff0, Fa0}, {call, _, _, _} = G) ->
%    VarN = uuid(),
%    Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)},
%    '.'({option, VarN, Expr}, G).

%%
%%
fmap(X) ->
   X.   


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
expr({option, _, Expr}) -> 
   Expr.

%%
%% map compose to partial expression
%%
partial({option, VarX, {'case', Ln, _, _} = Expr}) ->
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
