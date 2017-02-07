%% @doc
%%   category pattern: function category
-module(datum_cat_f).

-export(['.'/2, expr/1, partial/1]).

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
%% map compose to expression 
%% 
expr({f, Expr}) -> 
   join(Expr).

join([{call, _, _, _} = F, {call, Ln, Gf0, Ga0}|T]) ->
   Expr = {call, Ln, Gf0, set_blank_variable(F, Ga0)},
   join([Expr|T]);

join([{call, _, _, _} = F]) ->
   F.   

%%
%% map compose to partial expression
%%
partial({f, [{call, Ln, Ff0, Fa0} | T]}) ->
   VarX = uuid(),
   Expr = {call, Ln, Ff0, set_blank_variable({var, Ln, VarX}, Fa0)},
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
   list_to_atom("Var" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).

%%
%% set blank variable to 
set_blank_variable(X, [{var, _, '_'}|T]) ->
   [X|set_blank_variable(X, T)];

set_blank_variable(X, [H|T]) ->
   [H|set_blank_variable(X, T)];

set_blank_variable(_, []) ->
   [].  
