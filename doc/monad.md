# Monad

The library facilitates a pure functional programming by providing a set of utility functions to work with monads. It also provides the definition of several common monads and defines extension interface using Erlang parse-transform extension so that users can define their own monads. 

This papers and documents provides excessive definition of monads, please refer to it for details:
* [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads) 
* [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)

## "do"-notation

The library implements roughly equivalent of Haskell's "do"-notation (monadic binding form) using parse-transforms and special form of list comprehension (a similar techniques to erlando library). Monads extends the meaning of variable binding within an expression, so called programmable comma. 

```
-compile({parse_transform, monad}).
f() ->
   do([m_id ||
      X <- 10,
      Y <- 11,
      return(X + Y)
   ]).
```

The library transforms `f() -> do([ atom() || ... ]).` syntax construction to monadic binding using `atom()` as current monad. List comprehension generators `X <- ...` are transformed into symbol pattern matching rules. For example the following computation produces a list `[1, 2, 100, 101]`

```
f() ->
   do([m_id ||
      [X, Y] <- [1, 2],
      {A, B} <- {100, 101},
      return([X, Y, A, B])
   ]).
```  

Note the use of return as final "statement" of the do-notation. It is appropriate `return` functions in the specified monad that injects bounds value into the monad (container).

Besides syntax sugar for "do"-notation, the library does not do much until your application start using monads other than identity one.  

Monads are defined in terms of `return`, `bind` and `fail` operations
```
%% 
%% return :: a -> m a
-spec return(A) -> m(A).

%%
%% (>>=) :: m a -> (a -> m b) -> m b
-spec '>>='(m(A), fun((A) -> m(B))) -> m(B).

%%
%% fail :: String -> m a
-spec fail(_) -> m(_).
```

Let's show these operation at "do"-notation 
```
-compile({parse_transform, monad}).
f() ->
   do([m_id ||         %% (Monad m)
      X <- one(),      %% (>>=) :: m a -> (a -> m b) -> m b
      Y <- two(),      %% (>>=) :: m a -> (a -> m b) -> m b
      return(X + Y)    %% return :: (Monad m) => a -> m a
   ]).
```


## return

The operation takes non-monadic value or expression (plain type) and "lifts" it into container using monadic constructor. The library also implements a syntax operand `=<` to lift expression into monad. For instance, next computation produces `{ok, 4}`. 
```
f() ->
   do([m_error ||
      X <- return(2),
      Y =< X * X,
      return(Y)
   ]).
```


## fail

The operation enable failures in a special syntactic construct for monads. The operation is rarely used but allows to escalate failure within computation. The failure is either indicated using `fail` operation from specified monad or `>=` operand.


## anonymous variables

The library defines a special syntax to chain computations and they result through do-notation without explicit definition of binding variables. An anonymous variable `_` hold the result of previous pattern match statement. For example, the computation holds the result `{ok, 10}` 

```
f() ->
   do([m_error ||
      _ <- return(2), %% _ <- 2 
      _ =< _ * _,     %% _ <- 2 * 2 
      _ =< _ + 1,     %% _ <- 4 + 1
      _ =< _ * 2,     %% _ <- 5 * 2
      return(_)       %% 10
   ]).
```


## composition

```
f(File) ->
   do([m_error ||
      X <- file:open(File),
      Y <- read(X),
      _ <- file:close(X),
      return(Y)
   ]).

read(FD) ->
   do([m_error ||
      X <- file:read(FD, 4),
      Y <- file:read(FD, 4),      
      return(X + Y)
   ]).
```


## utility operation

Monads provides utility operation. These function are defined by the monad class and allows to transform monadic values, executed side effect, etc. The library defines operand `/=` to call utility operation from specified monad. For example, following computation holds result 3 and stash it in the state. 

```
f() ->
   do([m_state || 
      A =< 1,
      B =< 2,
      C =< A + B,
      _ /= put(lens:t1(), C),
      return(C)
   ]).
```

## bundled monads

### identity (`m_id`)

tbd

### error (`m_error`)

tbd

### io-monad (`m_io`)

tbd

### state (`m_state`)

tbd



## define new monad

tbd


## References

1. http://stenmans.org/happi_blog/?p=181
2. http://www.rabbitmq.com/blog/2011/05/17/can-you-hear-the-drums-erlando/
3. https://github.com/rabbitmq/erlando