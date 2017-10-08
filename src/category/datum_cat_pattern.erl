%% @doc
%%   category pattern: pattern match
-module(datum_cat_pattern).

%% (/=)
-export(['/='/1]).

%% (.) operation
-export(['.'/2, chain/1, curry/1]).

%% category utility
-export([fmap/1, fmap/2, fail/1, sequence/1, optionT/2, maybeT/2]).

%% {call,8,{atom,8,b},[]}
%% {call,8,{remote,8,{atom,8,x},{atom,8,b}},[]}

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
'.'({either, VarX, G}, {call, Ln, Ff0, Fa0} = F) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   Expr = dot_expr(Ln, VarX, {call, Ln, Ff0, Fa1}, G),
   {either, VarN, Expr};

'.'({either, VarX, G}, {generate, Ln, {var, _, VarN}, F}) ->
   {Fa1, VarZ} = datum_cat:cc_derive(F, []),
   Expr = dot_expr(Ln, [VarN], Fa1, G),
   % Expr = dot_expr(Ln, VarN, datum_cat:cc_bind_var({var, Ln, VarX}, F), G),
   {either, VarZ, Expr};

'.'({call, Ln, Ff0, Fa0}, G) ->
   {Fa1, VarN} = datum_cat:cc_derive(Fa0, []),
   '.'({either, VarN, {call, Ln, Ff0, Fa1}}, G).

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
curry({either, VarX, {'case', Ln, _, _} = Expr} = Either) ->
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
fmap(X) 
 when is_tuple(X) ->
   case element(1, X) of
      ok    -> X;
      error -> X;
      _     -> {ok, X}
   end;
fmap(X) ->
   {ok, X}.

fmap(A, X)
 when is_tuple(X) ->
   case element(1, X) of
      ok    -> X;
      error -> X;
      _     -> {ok, A, X}
   end;
fmap(A, X) ->
   {ok, A, X}.

%%
%%
fail(X) ->
   {error, X}.

%%
%% 
-spec sequence( [datum:either(_)] ) -> datum:either([_]).

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
%%
-spec optionT(_, datum:option(_) ) -> datum:either(_). 

optionT(Reason, undefined) ->
   {error, Reason};
optionT(_, X) ->
   {ok, X}.

%% deprecated
maybeT(Reason, X) ->
   optionT(Reason, X).

