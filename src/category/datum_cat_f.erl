%% @doc
%%   category pattern: the category of ordinal functions
-module(datum_cat_f).

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


%%
%%
unit(X) ->
   X.

%%
%%
fail(X) ->
   throw(X).

%%
%%
sequence(Seq) ->
   Seq.

%%
%%
-spec eitherT( datum:either(_) ) -> datum:option(_).

eitherT({ok, X}) ->
   X;
eitherT({error, _}) ->
   undefined.
