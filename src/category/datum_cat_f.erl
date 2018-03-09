%% @doc
%%   category pattern: the category of ordinal functions
-module(datum_cat_f).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%% category transformers
-export([unit/1, fail/1, require/3, sequence/1, flatten/1, optionT/1, eitherT/1]).

%%
%%
'/='(Arrow) ->
   Arrow.

%%
%% compose function(s) using AST notation
%%
%% f(_) . g(_) -> g(f(_))
%%
'.'(_, G, {call, _, _, _} = F) ->
   datum_cat:cc_bind_var(F, G);

'.'(_, G, {generate, Ln, VarS, F}) ->
   dot_arrow_state(Ln, VarS, F, G).


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
%% return dot-composition chain. 
chain(Expr) -> 
   Expr.

%%
%% curry do-composition chain into partial application
curry({_, Ln, _, _} = Expr) ->
   VarX = datum_cat:uuid(),
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, VarX}],
            [],
            [datum_cat:cc_bind_var({var, Ln, VarX}, Expr)]
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
-spec unit(_) -> _.

unit(X) ->
   X.

%%
%% lifts a failure to error object of category
-spec fail(_) -> _.

fail(X) ->
   throw(X).

%%
%% conditionally lifts a value to object or error of category 
-spec require(boolean(), _, _) -> _.

require(true,  X, _) ->
   X;
require(false, _, X) ->
   throw(X).

%%
%% transforms sequence of objects into object of category.
-spec sequence([_]) -> [_].

sequence(Seq) ->
   Seq.

%%
%% transforms nested objects into object of category
-spec flatten(_) -> _.

flatten(X) ->
   X.

%%
%% transforms option category to identity
-spec optionT( datum:option() ) -> _.

optionT(X) ->
   X.

%%
%%
-spec eitherT( datum:either(_) ) -> _.

eitherT({ok, X}) ->
   X;
eitherT({error, _}) ->
   undefined.
