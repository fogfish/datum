# Category

# `$^` either

```erlang
[$^|| a(), b(_)].

case a() of
   {ok, X} -> 
      b(X);
   {error, _} = Error ->
      Error
end.
```

```erlang
[$^|| a(), b(_, _)].

case a() of
   {ok, X, Y} -> 
      b(X, Y);
   {error, _} = Error ->
      Error
end.
```

```erlang
[$^|| a(), b()].

case a() of
   {error, _} = Error ->
      Error
   _ -> 
      b();
end.
```

