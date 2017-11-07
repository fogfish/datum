%% @doc
%%   category pattern: option category
-module(datum_cat_option).

%% (.) operation
-export(['.'/2, chain/1, curry/1]).

%% category utility
-export([fmap/1, fail/1, sequence/1, eitherT/1]).


%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> case f(_) of undefined -> undefined ; X -> g(X) end
%%
'.'({option, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   VarN = datum_cat:uuid(),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)}, G),
   {option, VarN, Expr};

'.'({option, VarX, G}, {generate, Ln, {var, _, VarN}, F}) ->
   Expr = dot_expr(Ln, VarN, datum_cat:cc_bind_var({var, Ln, VarX}, F), G),
   {option, VarX, Expr};

'.'({call, Ln, Ff0, Fa0}, G) ->
   VarN = datum_cat:uuid(),
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)},
   '.'({option, VarN, Expr}, G).

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
%% return dot-composition chain. 
chain({option, _, Expr}) -> 
   Expr.

%%
%% curry do-composition chain into partial application
curry({option, VarX, {'case', Ln, _, _} = Expr}) ->
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
%%
fmap(X) ->
   X.   

%%
%%
fail(_) ->
   undefined.

%%
%% 
-spec sequence( [datum:option(_)] ) -> datum:option([_]).

sequence([undefined | _]) ->
   undefined;

sequence([Head | Seq]) ->
   case sequence(Seq) of
      undefined ->
         undefined;
      Tail ->
         [Head | Tail]
   end;

sequence([]) ->
   [].

%%
%%
-spec eitherT( datum:either(_) ) -> datum:option(_).

eitherT({ok, X}) ->
   X;
eitherT({error, _}) ->
   undefined.


