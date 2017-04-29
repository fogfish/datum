%% @doc
%%   category pattern: either
-module(datum_cat_either).

-export(['.'/2, fmap/1, fmap/2, expr/1, partial/1]).

%%
%% compose function(s) using AST notation
%%
%% case f(_) of {error, _} = Err -> Err ; {ok, X} -> g(X) end
%%
'.'({either, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, Fa1}, G),
   {either, VarN, Expr};

'.'({call, Ln, Ff0, Fa0}, {call, _, _, _} = G) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   '.'({either, VarN, {call, Ln, Ff0, Fa1}}, G).

%%
%%
dot_expr(Ln, [], F, G) ->
   Err = uuid(),
   {'case', Ln, F, [
      {clause, Ln, 
         [{match, Ln, {tuple, Ln, [{atom, Ln, error}, {var, Ln, '_'}]}, {var, Ln, Err}}],
         [],
         [{var, Ln, Err}]
      },
      {clause, Ln,
         [{var, Ln, '_'}],
         [],
         [G]
      }
   ]};
dot_expr(Ln, VarX, F, G) ->
   Err = uuid(),
   Pat = [{var, Ln, X} || X <- VarX],
   {'case', Ln, F, [
      {clause, Ln,
         [{tuple, Ln, [{atom, Ln, ok}|Pat]}],
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
%%
fmap(X) 
 when is_tuple(X) ->
   case element(1, X) of
      ok    -> X;
      error -> X;
      _     -> {ok, X}
   end;
fmap(X) ->
   {ok, X}.

fmap(A, X)
 when is_tuple(X) ->
   case element(1, X) of
      ok    -> X;
      error -> X;
      _     -> {ok, A, X}
   end;
fmap(A, X) ->
   {ok, A, X}.


%%
%% map compose to expression 
%% 
expr({either, _, Expr}) -> 
   Expr.

%%
%% map compose to partial expression
%%
partial({either, VarX, {'case', Ln, _, _} = Expr}) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, X} || X <- VarX],
            [],
            [Expr]
         }
      ]}
   }.

%%
%% unique variable
uuid() ->
   list_to_atom("_Vx" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).
