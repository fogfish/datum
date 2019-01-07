# Infix functions

In Erlang, we apply functions using prefix notation - name is followed by its arguments in brackets `plus(1, 2)`. Humans are more used to infix operators than prefix or postfix. If a function takes two arguments, we have the option of using it in infix form, where we place it between its first and second arguments `1 /plus/ 2`.

The `parse_transform` feature implements a syntax sugar for *infix*, which is compiled into valid Erlang code using corresponding function at compile-time. To apply a function using infix notation, we enclose its name in slash characters (/). This allows us to use any arity 2 functions as infix operators.

```erlang
-compile({parse_transform, infix}).

1 /plus/ 1.

F /lists:map/ [1, 2, 3].
```
Infix notation does not change a function's behavior, it is purely a syntactic convenience that help readability in a specific situation.

## Operators

The parse transform allows to define custom operator -- a function of arity 2. Just put remote (mod:fun) or local functions enclosed in slashes. The usage of variables as operators is not supported due to confusion with legitimate Erlang expression.

```erlang
%% infix notation 
1 /plus/ 1
F /lists:map/ [1, 2, 3].

%% transformed to function calls
plus(1, 1).
lists:map(F, [1, 2, 3])/
```

Example of math operator for tuples

```erlang
example() ->
   {1,2,3} /add/ {4,5,6}, %% {5,7,9}
   1 /add/ {4,5,6},       %% {5,6,7}
   {1,2,3} /add/ 1.       %% {2,3,4}

add(X, Y)
 when is_tuple(X), is_tuple(Y) -> 
   list_to_tuple([
      A + B || {A, B} <- lists:zip(tuple_to_list(X), tuple_to_list(Y))
   ]);

add(X, Y)
 when is_integer(X), is_tuple(Y) ->
   list_to_tuple([
      A + X || A <- tuple_to_list(Y)
   ]);

add(X, Y)
 when is_tuple(X), is_integer(Y) ->
   list_to_tuple([
      A + Y || A <- tuple_to_list(X)
   ]).
```

User defined operators do not/cloud not/will not introduce any kind of polymorphism or overloading that Erlang does not already implement.

## Partial application

The infix notation supports a partial application, just replace left/right operand with unbound variable _

```erlang
%% partial application
_ /plus/ 1
1 /plus/ _

%% transformed to
fun(X) -> plus(X, 1) end.
fun(X) -> plus(1, X) end.

%% e.g. infix notation to increment elements in array
1 /erlang:'+'/ _ /lists:map/ [1,2,3,4].
```

## Monoid

The infix parse transform allows to define monoid -- an algebraic structure with a single associative binary operation and an identity element. Erlang module with following "behavior" defines a monoid class. 

```erlang
%% an identity element
-spec empty() -> _.

%% associative binary operation
-spec append(_, _) -> _.
```

Use module name enclosed in stars to refer it.

```erlang
%% infix notation
1 *plus* 1.

%% translated to function call.
plus:append(1, 1).
```

One of the practical application of monoids -- folding over various data structures. So far, everyone is familiar with folds over lists, but lists aren't the only foldable data structure. We can abstract and define fold over almost any data structure with help of monoids.

```erlang
plus /fold/ [1, 2, 3].

fold(Monoid, []) ->
   Monoid:empty();
fold(Monoid, [H | T]) ->
   Monoid:append(H, fold(Monoid, T)).
```

## Known limitations

1. **Import** is not supported yet.

```
-import(erlang, ['+'/2,]).

two() ->
   1 /'+'/ 1.
```

## References

[1] http://erlang.org/pipermail/erlang-questions/2004-March/011929.html
