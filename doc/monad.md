# Monad

tbd

## Syntax

```erlang
doM() ->
   do(['Mid' ||            %% (Monad m)    
      A <- one(),          %% (Monad m) => m a -> (a -> m b) -> m b

      B <- return(two()),  %% return :: (Monad m) => a -> m a
      C =< two(),          %% syntax sugar for return 
                          
      _ <- fail(invalid)   %% fail :: _ -> m a      
      _ >= invalid         %% syntax sugar for fail

      return(A + B + C)    %% final statement 
   ]).
```

References:

1. http://stenmans.org/happi_blog/?p=181
2. http://www.rabbitmq.com/blog/2011/05/17/can-you-hear-the-drums-erlando/
3. https://github.com/rabbitmq/erlando