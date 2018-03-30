%% @doc
%%   category pattern / functional composition
%%   [<compose operator> || <function 0> ... <function n>]
-module(datum_cat).

-export([is_category/1, category/3, cc_bind_var/2, cc_derive/2, uuid/0]).

%%
%%
-spec is_category({char, _, _} | _) -> atom() | false.

is_category({char, _, $.}) ->
   {datum_cat_f, datum_cat_f};
is_category({atom, _, identity}) ->
   {datum_cat_f, datum_cat_f};
is_category({char, _, $?}) ->
   {datum_cat_option, datum_cat_option};
is_category({atom, _, option}) ->
   {datum_cat_option, datum_cat_option};
is_category({atom, _, undefined}) ->
   {datum_cat_undefined, datum_cat_undefined};
is_category({char, _, $^}) ->
   {datum_cat_either, datum_cat_either};
is_category({atom, _, either}) ->
   {datum_cat_either, datum_cat_either};
is_category({atom, _, reader}) ->
   {datum_cat_reader, datum_cat_reader};
is_category({atom, _, Category}) ->
   case erlang:atom_to_list(Category) of
      "m_" ++ _ ->
         {datum_cat_kleisli, Category};
      _ ->
         false
   end;
is_category(_) ->
   false.

%%
%% check if category expression is partial function
is_partial([Head | _]) ->
   is_partial(Head);

is_partial({generate, _, _, Arrow}) ->
   is_partial(Arrow);

is_partial({op, _, _, _, Arrow}) ->
   is_partial(Arrow);

is_partial({call, _, _, Fa0}) ->
   length( erlang:element(2, cc_derive(Fa0, [])) ) > 0;

is_partial({'fun', _, _}) ->
   true;

is_partial({var, _, _}) ->
   true;

is_partial(_) ->
   false.


%%
%%
-spec category(atom(), atom(), erl_parse:abstract_expr()) -> erl_parse:abstract_expr().

category(Cat, Mod, Expr0) ->
   Expr1 = compile(Cat, Mod, Expr0),
   Expr2 = join(Mod, fun Cat:'.'/3, Expr1),
   cats(is_partial(Expr0), Cat, Expr2).

cats(false, Cat, Expr) ->
   Cat:chain(Expr);

cats(true, Cat, Expr) ->
   Cat:curry(Expr).


%%
%% helper function to bind blank variable with expression
%% flat (inject call to the position)
-spec cc_bind_var(erl_parse:abstract_expr(), erl_parse:abstract_expr()) -> erl_parse:abstract_expr().

cc_bind_var(Vx, X)
 when is_tuple(X) ->
   erlang:list_to_tuple(
      cc_bind_var(Vx, erlang:tuple_to_list(X))
   );

cc_bind_var(Vx, [{lc, _, Type, _} = H | T]) ->
   case is_category(Type) of
      %% enable variable binding for comprehension
      false ->
         [erlang:list_to_tuple(cc_bind_var(Vx, erlang:tuple_to_list(H))) | cc_bind_var(Vx, T)];

      % skip binding for nested category
      true ->
         [H | cc_bind_var(Vx, T)]
   end;
cc_bind_var(Vx, [{var, _, '_'} | T]) ->
   [Vx | cc_bind_var(Vx, T)];
cc_bind_var(Vx, [H | T]) ->
   [cc_bind_var(Vx, H) | cc_bind_var(Vx, T)];
cc_bind_var(_, []) ->
   [];

cc_bind_var(_, X) ->
   X.

%%
%% helper function to derive variables from expression with blank variables
-spec cc_derive(erl_parse:abstract_expr(), [_]) -> {[_], erl_parse:abstract_expr()}.

cc_derive({lc, _, Type, _} = Expr, Acc) ->
   case is_category(Type) of

      %% enable variable binding for comprehension
      false ->
         {Code, Var} = lists:mapfoldr(fun cc_derive/2, Acc, erlang:tuple_to_list(Expr)),
         {erlang:list_to_tuple(Code), Var};   

      %% skip variable binding for nested categories
      true  ->
         {Expr, Acc}
   end;

cc_derive({var, Ln, '_'}, Acc) ->
   Uuid = uuid(),
   {{var, Ln, Uuid}, [Uuid|Acc]};

cc_derive(Expr, Acc)
 when is_tuple(Expr) ->
   {Code, Var} = lists:mapfoldr(fun cc_derive/2, Acc, erlang:tuple_to_list(Expr)),
   {erlang:list_to_tuple(Code), Var};

cc_derive(Expr, Acc)
 when is_list(Expr) ->
   lists:mapfoldr(fun cc_derive/2, Acc, Expr);

cc_derive(Expr, Acc) ->
   {Expr, Acc}.

%%
%% unique variable
uuid() ->
   list_to_atom("_Vx" ++ integer_to_list(erlang:unique_integer([monotonic, positive]))).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% compile expression to functional composition (f . g . h ...)
compile(Cat, Mod, List) ->
   lists:reverse([c(Cat, Mod, X) || X <- List]).

c(Cat, Mod, {generate, Line, VarS, Arrow}) ->
   {generate, Line, VarS, c_cats(Mod, c_arrow(Cat, Arrow))};

c(Cat, Mod, Arrow) ->
   c_cats(Mod, c_arrow(Cat, Arrow)).


%%
%% An arrow is the term used in category theory as an abstract notion of 
%% thing that behaves like a function. It represents [A] process that takes as 
%% input something of type [B] and outputs something of type [C].
%%
c_arrow(_Cat, {call, Ln, {atom, _, unit} = Fn, Fa}) ->
   {call, Ln, {remote, Ln, {atom, Ln, cats}, Fn}, Fa};

c_arrow(_Cat, {call, Ln, {atom, _, fail} = Fn, Fa}) ->
   {call, Ln, {remote, Ln, {atom, Ln, cats}, Fn}, Fa};

c_arrow(_Cat, {call, _, {remote, _, {atom, _, cats}, {atom, _, unit}}, _} = Expr) ->
   Expr;

c_arrow(_Cat, {call, _, {remote, _, {atom, _, cats}, {atom, _, fail}}, _} = Expr) ->
   Expr;

c_arrow(Cat, {call, Ln, {remote, Ln, {atom, _, cats}, {atom, _, _} = Fn}, Fa}) ->
   Cat:'/='({call, Ln, {remote, Ln, {atom, Ln, cats}, Fn}, Fa});

c_arrow(_Cat, {call, _, _, _} = H) ->
   % explicit call: f(...)
   H;

c_arrow(_Cat, {'fun', Line, {function, Id, _}}) ->
   % reference to function: fun f/n 
   {call, Line, {atom, Line, Id}, [{var, Line, '_'}]};

c_arrow(_Cat, {'fun', Line, {function, Mod, Fun, _}}) ->
   % reference to function: fun mod:f/n
   {call, Line, {remote, Line, Mod, Fun}, [{var, Line, '_'}]};

c_arrow(_Cat, {'fun', Line, {clauses, _}} = H) ->
   % inline function: fun(_) -> ... end
   {call, Line, H, [{var, Line, '_'}]};

c_arrow(_Cat, {var, Line, _} = H) ->
   % function reference within variable: X = ... 
   {call, Line, H, [{var, Line, '_'}]};

c_arrow(_Cat, {op, Ln, '=<', VarS, Arrow}) ->
   {generate, Ln, VarS, 
      {call, Ln, {remote, Ln, {atom, Ln, cats}, {atom, Ln, unit}}, [Arrow]}};

c_arrow(_Cat, {op, Ln, '>', VarS, Arrow}) ->
   {generate, Ln, VarS, 
      {call, Ln, {remote, Ln, {atom, Ln, cats}, {atom, Ln, putT}}, [Arrow]}};

c_arrow(_Cat, {op, Ln, '<', VarS, Arrow}) ->
   {generate, Ln, VarS, 
      {call, Ln, {remote, Ln, {atom, Ln, cats}, {atom, Ln, getT}}, [Arrow]}};

c_arrow(Cat, {op, Ln, '/=', VarS, {call, Ln, {remote, Ln, {atom, _, cats}, {atom, _, _} = Fn}, Fa}}) ->
   {generate, Ln, VarS, Cat:'/='({call, Ln, {remote, Ln, {atom, Ln, cats}, Fn}, Fa})};

%
% it would be nice to have a short syntax for transformer
%   _ /= x() 
% 
% but it limits ability for local transformer definition which is essential feature
%
% c_arrow(Cat, Mod, {op, Ln, '/=', VarS, {call, Ln, {atom, _, _} = Fn, Fa}}) ->
%    {generate, Ln, VarS, Cat:'/='({call, Ln, {remote, Ln, {atom, Ln, Mod}, Fn}, Fa})};
%
% c_arrow(Cat, Mod, {op, Ln, '/=', VarS, {atom, _, _} = Fn}) ->
%    {generate, Ln, VarS, Cat:'/='({call, Ln, {remote, Ln, {atom, Ln, Mod}, Fn}, []})};

c_arrow(Cat, {op, Ln, '/=', VarS, Arrow}) ->
   {generate, Ln, VarS, Cat:'/='(Arrow)};

c_arrow(_, H) ->
   exit( lists:flatten(io_lib:format("Category composition do not support: ~p", [H])) ).

%%
%%
c_cats(Mod, Expr)
 when is_tuple(Expr) ->
   erlang:list_to_tuple(
      c_cats(Mod, erlang:tuple_to_list(Expr))
   );

c_cats(Mod, [{atom, Ln, cats} | T]) ->
   [{atom, Ln, Mod} | c_cats(Mod, T)];

c_cats(Mod, [H | T]) ->
   [c_cats(Mod, H) | c_cats(Mod, T)];

c_cats(_, []) ->
   [];

c_cats(_, Expr) ->
   Expr.


%%
%% join
join(Mod, Fun, [F, G | T]) ->
   join(Mod, Fun, [Fun(Mod, F, G)|T]);
join(_, _, [Expr]) ->
   Expr.
