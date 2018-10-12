%% @doc
%%   category pattern: either
-module(datum_cat_either).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%% category transformers
-export([unit/1, unit/2, fail/1, require/3, sequence/1, flatten/1, optionT/1, optionT/2, eitherT/1, tryT/1]).

%%
%%
'/='(Arrow) ->
   Arrow.


%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> case f(_) of {error, _} = Err -> Err ; {ok, X} -> g(X) end
%%
'.'(_, {either, VarX, G}, {call, Ln, Ff0, Fa0}) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, Fa1}, G),
   {either, VarN, Expr};

'.'(_, {either, _VarX, G}, {generate, Ln, {var, _, VarN}, F}) ->
   {Fa1, VarZ} = datum_cat:cc_derive(F, []),
   Expr = dot_expr(Ln, [VarN], Fa1, G),
   {either, VarZ, Expr};

'.'(Cat, {call, Ln, Ff0, Fa0}, G) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   '.'(Cat, {either, VarN, {call, Ln, Ff0, Fa1}}, G);

'.'(Cat, {generate, _Ln, _Var, F}, G) ->
   %% ignore tail arrow
   '.'(Cat, F, G).


%%
%%
dot_expr(Ln, [], F, G) ->
   Err = datum_cat:uuid(),
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
   Err = datum_cat:uuid(),
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
%% map compose to expression 
%% 
chain({either, _, Expr}) -> 
   Expr.

%%
%% map compose to partial expression
%%
curry({either, VarX, {'case', Ln, _, _} = Expr}) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, X} || X <- VarX],
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
-spec unit(_) -> datum:either(_).

unit(X) ->
   {ok, X}.

unit(A, X) ->
   {ok, A, X}.

%%
%% lifts a failure to error object of category
-spec fail(_) -> datum:either(_).

fail(X) ->
   {error, X}.

%%
%% conditionally lifts a value to object or error of category 
-spec require(boolean(), _, _) -> datum:either().

require(true,  X, _) ->
   {ok, X};
require(false, _, X) ->
   {error, X}.

%%
%% transforms sequence of objects into object of category.
-spec sequence([datum:either(_)]) -> datum:either([_]).

sequence([{ok, Head} | Seq]) ->
   case sequence(Seq) of
      {ok, Tail} ->
         {ok, [Head|Tail]};
      {error, _} = Error ->
         Error
   end;

sequence([{error, _} = Error | _]) ->
   Error;

sequence([]) ->
   {ok, []}.


%%
%% transforms nested objects into object of category
-spec flatten(datum:either(datum:either(_))) -> datum:either(_).

flatten({ok, {ok, _} = X}) ->
   flatten(X);
flatten({ok, {error, _} = X}) ->
   flatten(X);
flatten({error, {ok, _} = X}) ->
   flatten(X);
flatten({error, {error, _} = X}) ->
   flatten(X);
flatten({ok, _} = X) ->
   X;
flatten({error, _} = X) ->
   X.


%%
%% transforms option category to identity
-spec optionT( datum:option() ) -> datum:either(_).

optionT(undefined) ->
   {error, undefined};
optionT(X) ->
   {ok, X}.

optionT(Reason, undefined) ->
   {error, Reason};
optionT(_, X) ->
   {ok, X}.

%%
%%
-spec eitherT( datum:either(_) ) -> datum:either(_).

eitherT({ok, _} = X) ->
   X;
eitherT({error, _} = X) ->
   X.

%%
%%
-spec tryT( _ ) -> datum:either(_).

tryT({'EXIT', {Reason, _Stack}}) ->
   {error, Reason};
tryT({'EXIT', Reason}) ->
   {error, Reason};
tryT({ok, _} = Result) ->
   Result;
tryT({error, _} = Result) ->
   Result;
tryT(Result) ->
   {ok, Result}.
