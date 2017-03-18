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
is_category({char, _, $?}) ->
   datum_cat_maybe;
is_category({char, _, $^}) ->
   datum_cat_either;
is_category(_) ->
   false.

%%
%%
is_partial([{call, _, _, Fa0} | _]) ->
   length( lists:filter(fun({var, _, '_'}) -> true; (_) -> false end, Fa0) ) > 0.

%%
%%
-spec category(atom(), erl_parse:abstract_expr()) -> erl_parse:abstract_expr().

category(Cat, Expr0) ->
   Expr1 = compile(Expr0),
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
   {Code, Var} = lists:mapfoldl(fun cc_derive/2, Acc, erlang:tuple_to_list(Expr)),
   {erlang:list_to_tuple(Code), Var};

cc_derive(Expr, Acc)
 when is_list(Expr) ->
   lists:mapfoldl(fun cc_derive/2, Acc, Expr);

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
compile(List) ->
   lists:reverse([c(X) || X <- List]).

c({call, _, _, _} = H) ->
   % explicit call: f(...)
   H;

c({'fun', Line, {function, Id, _}}) ->
   % reference to function: fun f/n 
   {call, Line, {atom, Line, Id}, [{var, Line, '_'}]};

c({'fun', Line, {function, Mod, Fun, _}}) ->
   % reference to function: fun mod:f/n
   {call, Line, {remote, Line, Mod, Fun}, [{var, Line, '_'}]};

c({'fun', Line, {clauses, _}} = H) ->
   % inline function: fun(_) -> ... end
   {call, Line, H, [{var, Line, '_'}]};

c({var, Line, _} = H) ->
   % function reference within variable: X = ... 
   {call, Line, H, [{var, Line, '_'}]};

c(H) ->
   exit( lists:flatten(io_lib:format("Function composition do not support the expression:~n~p~n", [H])) ).


%%
%% join
join(Fun, [F, {call, _, _, _} = G | T]) ->
   join(Fun, [Fun(F, G)|T]);
join(_, [Expr]) ->
   Expr.
