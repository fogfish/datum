%% @doc
%%   monad do-notation compiler
-module(monad).

-export([
   'id.return'/1, 'id.>>='/2,
   'io.return'/1, 'io.>>='/2
]).
-export([parse_transform/2]).


%%%------------------------------------------------------------------
%%%
%%% build-in monads
%%%
%%%------------------------------------------------------------------

%%
%% identity monad
'id.return'(X) ->
   X. 

'id.>>='(X, Fun) ->
   Fun(X).

%%
%% i/o monad
'io.return'(X)
 when is_function(X) ->
   X;
'io.return'(X) ->
   fun(_) -> X end.

'io.>>='(X, Fun) ->
   fun(Context) ->
      case Fun( X(Context) ) of
         Fun1 when is_function(Fun1) ->
            Fun1(Context);
         Scalar ->
            Scalar
      end
   end.


%%%------------------------------------------------------------------
%%%
%%% monad compiler (parse transform)
%%%
%%%------------------------------------------------------------------

%%
%% built-in pattern for abstract syntax tree
-define(FUN(Clauses), 
   {function, Label, Name, Arity, Clauses}).
 
-define(MONAD(Lib, Monad),  
   {lc, _, {tuple, _, [{atom, _, Lib}, {atom, _, Monad}]}, _}).


%%
%% entry-point
parse_transform(AST, _Options) -> 
   pt_function(AST, []).

%%
%% monad syntax requires us only function transformation
pt_function([?FUN(Clauses) | Tail], Acc) ->
   pt_function(Tail, [?FUN(pt_clauses(Clauses)) | Acc]);

pt_function([Head | Tail], Acc) ->
   pt_function(Tail, [Head | Acc]);

pt_function([], Acc) ->
   lists:reverse(Acc).

%%
%% only function body defines do syntax
pt_clauses([{clause, CLine, A1, A2, Code} | Clauses]) ->
   [{clause, CLine, A1, A2, pt_monad(Code)} | pt_clauses(Clauses)];
pt_clauses([]) -> 
   [].

%%
%%
pt_monad([?MONAD(Lib, Monad) = Head | Tail]) -> 
   [compile({Lib, Monad}, Head) | pt_monad(Tail)];

pt_monad(List) ->
   List.

%%
%% compile comprehension to monad
compile(Monad, {lc, Ln, _, List} = LC) ->
   case compile(List, Monad, []) of
      {Var, Expr} ->
         {call,Ln, {atom,Ln,hd}, [{lc, Ln, {var, Ln, Var}, Expr}]};
      _ ->
         LC
   end.

compile([{generate, _, {var, _, Ma}, _} = Expr | Tail], Monad, Acc) ->
   compile(Tail, Ma, Monad, [return(Monad, Expr) | Acc]);

compile([Head | Tail], Monad, Acc) ->
   compile(Tail, Monad, [Head | Acc]);

compile([], _, Acc) ->
   lists:reverse(Acc).   

compile([{generate, _, {var, _, Mb}, _} = Expr | Tail], Ma, Monad, Acc) ->
   compile(Tail, Mb, Monad, ['>>='(Monad, Ma, Expr) | Acc]);

compile([Head | Tail], Var, Monad, Acc) ->
   compile(Tail, Var, Monad, [Head | Acc]);

compile([], Var, _, Acc) ->
   {Var, lists:reverse(Acc)}.   

%%
%% lift expression to list-comprehension
lc(Ln, Expr) ->
   {'case', Ln,
      Expr,
      [
         {clause, Ln, 
            [{var, Ln, 'X'}],
            [[{call, Ln, {atom, Ln, is_list}, [{var, Ln, 'X'}]}]],
            [{var, Ln, 'X'}]
         },
         {clause, Ln,
            [{var, Ln, 'X'}],
            [],
            [{cons, Ln, {var, Ln, 'X'}, {nil, Ln}}]
         }
      ]
   }.

%% 
%% lift expression to monadic type (including list comprehension)
return({Lib, Monad}, Ln, Expr) ->
   Return = list_to_atom(atom_to_list(Monad) ++ ".return"),
   {call, Ln, 
      {remote, Ln, {atom, Ln, Lib}, {atom, Ln, Return}},
      [Expr]
   }.

return(Monad, {generate, Ln, Head, Expr}) ->
   {generate, Ln, Head, lc(Ln, return(Monad, Ln, Expr))}.

%%
%% bind monad
'>>='({Lib, Monad}, Ma, Ln, Expr) ->
   Bind = list_to_atom(atom_to_list(Monad) ++ ".>>="),
   {call, Ln, 
      {remote, Ln, {atom, Ln, Lib}, {atom, Ln, Bind}},
      [{var, Ln, Ma}, lambda(Monad, Ma, Expr)]
   }.

'>>='(Monad, Ma, {generate, Ln, {var, _, Mb}, Expr}) ->
   {generate, Ln, {var, Ln, Mb}, lc(Ln, return(Monad, Ln, '>>='(Monad, Ma, Ln, Expr)))}.

%%
%% lift expression to lambda function
lambda(_Monad, Ma, {call, Ln, Fun, Args}) ->
   {Local, Largs} = largs(Args, Ma, '_', []),
   {'fun', Ln,
      {clauses, 
         [
            {clause, Ln, [{var, Ln, Local}], [], [{call, Ln, Fun, Largs}]}
         ]
      }
   };
   
lambda(_, _, _) ->
   exit("only function calls are supported as generators.").

%% 
%% rewrite lambda arguments to support 
largs([{var, Ln, Ma} | Tail], Ma, _, Acc) ->
   La = list_to_atom("LocalMonadVar_" ++ atom_to_list(Ma)),
   largs(Tail, Ma, La, [{var, Ln, La} | Acc]);

largs([Head | Tail], Ma, La, Acc) ->
   largs(Tail, Ma, La, [Head | Acc]);

largs([], _, La, Acc) ->
   {La, lists:reverse(Acc)}.
