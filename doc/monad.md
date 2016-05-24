# Monad

tbd

## Syntax

```erlang
do() ->
   [{'M', id} ||           %% (Monad m)    
      A <- one(),          %% (Monad m) => m a -> (a -> m b) -> m b

      B <- return(two()),  %% return :: (Monad m) => a -> m a
      C =< two(),          %% syntax sugar for return 
                          
      _ <- fail(invalid)   %% fail :: _ -> m a      
      _ >= invalid         %% syntax sugar for fail

      return(A + B + C)    %% final statement 
   ].
```
