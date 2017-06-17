%% @doc
%%   category pattern / functional composition
%%   [<compose operator> || <function 0> ... <function n>]
-module(datum_cat).

-export([is_category/1, category/2, cc_bind_var/2, cc_derive/2, uuid/0]).

%%
%%
-spec is_category({char, _, _} | _) -> atom() | false.

is_category({char, _, $.}) ->
   datum_cat_f;
is_category({atom, _, identity}) ->
   datum_cat_f;
is_category({char, _, $?}) ->
   datum_cat_option;
is_category({atom, _, option}) ->
   datum_cat_option;
is_category({char, _, $^}) ->
   datum_cat_either;
is_category({atom, _, either}) ->
   datum_cat_either;
is_category(_) ->
   false.

%%
%%
is_partial([{call, _, _, Fa0} | _]) ->
   length( lists:filter(fun({var, _, '_'}) -> true; (_) -> false end, Fa0) ) > 0;

is_partial([{'fun', _, _} | _]) ->
   true;

is_partial([{var, _, _} | _]) ->
   true.

%%
%%
-spec category(atom(), erl_parse:abstract_expr()) -> erl_parse:abstract_expr().

category(Cat, Expr0) ->
   Expr1 = compile(Cat, Expr0),
   category(is_partial(Expr0), Cat, Expr1).

category(false, Cat, Expr) ->
   Cat:expr(join(fun Cat:'.'/2, Expr));

category(true, Cat, Expr) ->
   Cat:partial(join(fun Cat:'.'/2, Expr)).


%%
%% helper function to bind blank variable with expression
-spec cc_bind_var(erl_parse:abstract_expr(), erl_parse:abstract_expr()) -> erl_parse:abstract_expr().

cc_bind_var(Vx, X)
 when is_tuple(X) ->
   erlang:list_to_tuple(
      cc_bind_var(Vx, erlang:tuple_to_list(X))
   );

cc_bind_var(Vx, [{lc, _, _, _} = H | T]) ->
   % skip binding for nested objects
   [H | cc_bind_var(Vx, T)];
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

cc_derive({lc, _, _, _} = Expr, Acc) ->
   % skip binding for nested objects
   {Expr, Acc};

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
compile(Cat, List) ->
   lists:reverse([c(Cat, X) || X <- List]).

c(Cat, {call, Ln, {atom, _, fmap} = Fn, Fa}) ->
   {call, Ln, {remote, Ln, {atom, Ln, Cat}, Fn}, Fa};

c(Cat, {call, Ln, {remote, Ln, {atom, _, category}, {atom, _, _} = Fn}, Fa}) ->
   {call, Ln, {remote, Ln, {atom, Ln, Cat}, Fn}, Fa};

c(_, {call, _, _, _} = H) ->
   % explicit call: f(...)
   H;

c(_, {'fun', Line, {function, Id, _}}) ->
   % reference to function: fun f/n 
   {call, Line, {atom, Line, Id}, [{var, Line, '_'}]};

c(_, {'fun', Line, {function, Mod, Fun, _}}) ->
   % reference to function: fun mod:f/n
   {call, Line, {remote, Line, Mod, Fun}, [{var, Line, '_'}]};

c(_, {'fun', Line, {clauses, _}} = H) ->
   % inline function: fun(_) -> ... end
   {call, Line, H, [{var, Line, '_'}]};

c(_, {var, Line, _} = H) ->
   % function reference within variable: X = ... 
   {call, Line, H, [{var, Line, '_'}]};

c(_, H) ->
   exit( lists:flatten(io_lib:format("Function composition do not support the expression:~n~p~n", [H])) ).


%%
%% join
join(Fun, [F, {call, _, _, _} = G | T]) ->
   join(Fun, [Fun(F, G)|T]);
join(_, [Expr]) ->
   Expr.
