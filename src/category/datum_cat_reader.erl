%% @doc
%%   category pattern: pattern match
-module(datum_cat_reader).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/3, chain/1, curry/1]).

%% category utility
-export([unit/1, fail/1, sequence/2, optionT/3, flatten/2]).


'/='({call, Ln, Ff0, Fa0}) ->
   Fa1 = Fa0 ++ [{var, Ln, '_PatternGlobalEnvironment'}],
   {call, Ln, Ff0, Fa1};

'/='(Arrow) ->
   exit( lists:flatten(io_lib:format("Pattern category composition do not support the arrow of type: ~p", [Arrow])) ).

%%
%% compose function(s) using AST notation
%%
%% case f(_) of {error, _} = Err -> Err ; {ok, X} -> g(X) end
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
chain({either, _, {'case', Ln, _, _} = Expr}) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, '_PatternGlobalEnvironment'}],
            [],
            [Expr]
         }
      ]}
   }.

%%
%% map compose to partial expression
%%
curry({either, VarX, {'case', Ln, _, _}} = Either) ->
   {'fun', Ln,
      {clauses, [
         {clause, Ln,
            [{var, Ln, X} || X <- VarX],
            [],
            [chain(Either)]
         }
      ]}
   }.

%%
%%
unit(X) ->
   {ok, X}.


%%
%%
fail(X) ->
   {error, X}.

%%
%% 
-spec sequence( [datum:either(_)], _ ) -> datum:either([_]).

sequence([{ok, Head} | Seq], Env) ->
   case sequence(Seq, Env) of
      {ok, Tail} ->
         {ok, [Head|Tail]};
      {error, _} = Error ->
         Error
   end;

sequence([{error, _} = Error | _], _Env) ->
   Error;

sequence([], _Env) ->
   {ok, []}.



%%
%%
-spec optionT(_, datum:option(_), _) -> datum:either(_). 

optionT(Reason, undefined, _Env) ->
   {error, Reason};
optionT(_, X, _Env) ->
   {ok, X}.


%%
%%
-spec flatten(_, _) ->  datum:either(_).

flatten({ok, {ok, _} = X}, Env) ->
   flatten(X, Env);
flatten({ok, {error, _} = X}, Env) ->
   flatten(X, Env);
flatten({error, {ok, _} = X}, Env) ->
   flatten(X, Env);
flatten({error, {error, _} = X}, Env) ->
   flatten(X, Env);
flatten({ok, _} = X, _Env) ->
   X;
flatten({error, _} = X, _Env) ->
   X.


