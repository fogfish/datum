%% @doc
%%   category pattern: function category
-module(datum_cat_f).

-export(['.'/2, fmap/1, expr/1, partial/1]).

%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> g(f(_))
%%
'.'({f, G}, {call, _, _, _} = F) ->
   {f, [F|G]};

'.'({call, _, _, _} = G, {call, _, _, _} = F) ->
   '.'({f, [G]}, F).

%%
%%
fmap(X) ->
   X.

%%
%% map compose to expression 
%% 
expr({f, Expr}) -> 
   join(Expr).

join([{call, _, _, _} = F, {call, Ln, Gf0, Ga0}|T]) ->
   Expr = {call, Ln, Gf0, datum_cat:cc_bind_var(F, Ga0)},
   join([Expr|T]);

join([{call, _, _, _} = F]) ->
   F.   

%%
%% map compose to partial expression
%%
partial({f, [{call, Ln, Ff0, Fa0} | T]}) ->
   VarX = uuid(),
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarX}, Fa0)},
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, VarX}],
            [],
            [join( [Expr | T] )]
         }
      ]}
   }.

%%
%% unique variable
uuid() ->
   list_to_atom("_Vx" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).
