%% @doc
%%   category pattern: function category
-module(datum_cat_f).

-export([
   '.'/2, 
   fmap/1, 
   expr/1, 
   partial/1
]).


%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> g(f(_))
%%
'.'({call, Ln, Gf0, Ga0}, {call, _, _, _} = F) ->
   {call, Ln, Gf0, datum_cat:cc_bind_var(F, Ga0)};

'.'({call, _, _, _} = G, {generate, Ln, VarS, F}) ->
   dot_arrow_state(Ln, VarS, F, G);

'.'(G, {call, _, _, _} = F) ->
   datum_cat:cc_bind_var(F, G);

'.'(G, {generate, Ln, VarS, F}) ->
   dot_arrow_state(Ln, VarS, F, G).

   % io:format("=[ g ] => ~p~n", [G]),
   % io:format("=[ f ] => ~p~n", [F]),
   % exit(ffff).


% '.'(G, {call, Ln, Ff0, Fa0}) ->
%    {call, Ln, Gf0, datum_cat:cc_bind_var(G, Fa0)}.

%    dot_arrow_state(Ln, VarS, F, G);


% '.'({f, G}, {generate, Ln, VarS, F}) ->
%    Expr = dot_arrow_state(Ln, VarS, F, G),   
%    {f, Expr};

% '.'({f, G}, {call, Ln, Ff0, Fa0} = F) ->
%    io:format("=[ derv ]=> ~p~n", [datum_cat:cc_derive(Fa0, [])]),
%    io:format("=[ bind ]=> ~p ~p~n", [G, F]),
%    Expr = {call, Ln, Ff0, datum_cat:cc_bind_var(G, Fa0)},
%    {f, Expr};

% '.'({call, _, _, _} = G, F) ->
%    '.'({f, G}, F).

%%
%%
dot_arrow_state(Ln, VarX, F, G) ->
   {'case', Ln, F, [
      {clause, Ln,
         [VarX],
         [],
         [G]
      }
   ]}.

%%
%%
fmap(X) ->
   X.

%%
%% map compose to expression 
%% 
% expr({f, Expr}) -> 
expr(Expr) -> 
   Expr.
%    join(Expr).

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
