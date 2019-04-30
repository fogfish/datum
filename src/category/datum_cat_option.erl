%% @doc
%%   category pattern: option category
-module(datum_cat_option).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%% category transformers
-export([unit/1, fail/1, require/3, sequence/1, flatten/1, optionT/1, eitherT/1, tryT/1]).

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
   VarN = {var, Ln, datum_cat:uuid()},
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, datum_cat:cc_bind_var(VarN, Fa0)}, G),
   {option, VarN, Expr};

'.'(_, {option, VarX, G}, {generate, Ln, Pattern, F}) ->
   Expr = dot_expr(Ln, Pattern, datum_cat:cc_bind_var(VarX, F), G),
   {option, VarX, Expr};

'.'(Cat, {call, Ln, Ff0, Fa0}, G) ->
   VarN = {var, Ln, datum_cat:uuid()},
   Expr = {call, Ln, Ff0, datum_cat:cc_bind_var(VarN, Fa0)},
   '.'(Cat, {option, VarN, Expr}, G);

'.'(Cat, {generate, _Ln, _Var, F}, G) ->
   %% ignore tail arrow
   '.'(Cat, F, G).

%%
%% 
dot_expr(Ln, Pattern, F, G) ->
   {'case', Ln, F, [
      {clause, Ln,
         [{atom, Ln, undefined}],
         [],
         [{atom, Ln, undefined}]
      },
      {clause, Ln, 
         [Pattern],
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


%%%------------------------------------------------------------------
%%%
%%% transformers
%%%
%%%------------------------------------------------------------------   

%%
%% lifts a value to object of category
-spec unit(_) -> datum:option(_).

unit(X) ->
   X.

%%
%% lifts a failure to error object of category
-spec fail(_) -> datum:option(_).

fail(_) ->
   undefined.

%%
%% conditionally lifts a value to object or error of category 
-spec require(boolean(), _, _) -> datum:option().

require(true,  X, _) ->
   X;
require(false, _, _) ->
   undefined.

%%
%% transforms sequence of objects into object of category.
-spec sequence([datum:option(_)]) -> datum:option([_]).

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
%% transforms nested objects into object of category
-spec flatten(_) -> _.

flatten(X) ->
   X.

%%
%% transforms option category to identity
-spec optionT( datum:option() ) -> datum:option(_).

optionT(X) ->
   X.

%%
%%
-spec eitherT( datum:either(_) ) -> datum:option(_).

eitherT({ok, X}) ->
   X;
eitherT({error, _}) ->
   undefined.

%%
%%
-spec tryT( _ ) -> datum:option(_).

tryT({'EXIT', {_Reason, _Stack}}) ->
   undefined;
tryT({'EXIT', _Reason}) ->
   undefined;
tryT(Result) ->
   Result.

