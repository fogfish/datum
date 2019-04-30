%% @doc
%%   category pattern: the category of monadic functions
-module(datum_cat_kleisli).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%%
%%
'/='(Arrow) ->
   Arrow.


%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> m_state:'>>='(f(), fun(X) -> m_state:'>>='(g(X), ...) end)
'.'(Monad, {monad, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   VarN = {var, Ln, datum_cat:uuid()},
   Expr = dot_expr(Monad, Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var(VarN, Fa0)}, G),
   {monad, VarN, Expr};

'.'(Monad, {monad, VarX, G}, {generate, Ln, Pattern, F}) ->
   Expr = dot_expr(Monad, Ln, Pattern, datum_cat:cc_bind_var(VarX, F), G),
   {monad, VarX, Expr};

'.'(Monad, {call, Ln, _, _} = F, G) ->
   VarN = {var, Ln, datum_cat:uuid()},
   Expr = {call, Ln, {remote, Ln, {atom, Ln, Monad}, {atom, Ln, unit}}, [VarN]},
   '.'(Monad, '.'(Monad, {monad, VarN, Expr}, F), G);

'.'(Cat, {generate, _Ln, _Var, F}, G) ->
   %% ignore tail arrow
   '.'(Cat, F, G).

%%
%% 
dot_expr(Monad, Ln, Pattern, F, G) ->
   {call, Ln, 
      {remote, Ln, {atom, Ln, Monad}, {atom, Ln, '>>='}},
      [
         F, 
         {'fun', Ln,
            {clauses, 
               [
                  {clause, Ln, [Pattern], [], [G]}
               ]
            }
         }
      ]
   }.


%%
%% return dot-composition chain. 
chain({monad, _, Expr}) -> 
   Expr.

%%
%% curry do-composition chain into partial application
curry({monad, VarX, {_, Ln, _, _} = Expr}) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [VarX],
            [],
            [Expr]
         }
      ]}
   }.
