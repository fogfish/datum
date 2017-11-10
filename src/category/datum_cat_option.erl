%% @doc
%%   category pattern: option category
-module(datum_cat_option).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%% transformers
-export([unit/1, fail/1, sequence/1, eitherT/1]).

%%
%%
'/='(Arrow) ->
   Arrow.

%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> case f(_) of undefined -> undefined ; X -> g(X) end
%%
'.'(_, {option, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   VarN = datum_cat:uuid(),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)}, G),
   {option, VarN, Expr};

'.'(_, {option, VarX, G}, {generate, Ln, {var, _, VarN}, F}) ->
   Expr = dot_expr(Ln, VarN, datum_cat:cc_bind_var({var, Ln, VarX}, F), G),
   {option, VarX, Expr};

'.'(Cat, {call, Ln, Ff0, Fa0}, G) ->
   VarN = datum_cat:uuid(),
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var({var, Ln, VarN}, Fa0)},
   '.'(Cat, {option, VarN, Expr}, G);

'.'(Cat, {generate, _Ln, _Var, F}, G) ->
   %% ignore tail arrow
   '.'(Cat, F, G).

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
unit(X) ->
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


