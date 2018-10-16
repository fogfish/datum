%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   type cast utilities
-module(typecast).

-export([
   i/1,
   f/1,
   s/1,
   ls/1,
   c/1,
   lc/1,
   a/1,
   atom/1,
   x/1
]).


%%
%% typecast scalar data type to integer or fails
-spec i(_) -> integer().

i(X) when is_binary(X)  -> btoi(X);
i(X) when is_atom(X)    -> atoi(X);
i(X) when is_list(X)    -> ltoi(X);
i(X) when is_integer(X) -> X;
i(X) when is_float(X)   -> ftoi(X).

btoi(X) -> ltoi(btol(X)).
atoi(X) -> ltoi(atol(X)).
ltoi(X) -> list_to_integer(X).
ftoi(X) -> erlang:trunc(X). 

%%
%% typecast scalar data type to double in normal (fixed-point) notation or fails
-spec f(_) -> float().

f(X) when is_binary(X)  -> btof(X);
f(X) when is_atom(X)    -> atof(X);
f(X) when is_list(X)    -> ltof(X);
f(X) when is_integer(X) -> itof(X);
f(X) when is_float(X)   -> X.

btof(X) -> ltof(btol(X)).
atof(X) -> ltof(atol(X)).
ltof(X) -> list_to_float(X).
itof(X) -> X + 0.0.

%%
%% typecast scalar data type to binary string or fails
-spec s(_) -> binary().

s(undefined)            -> <<>>;
s(X) when is_binary(X)  -> btos(X);
s(X) when is_atom(X)    -> atos(X);
s(X) when is_list(X)    -> ltos(X);
s(X) when is_integer(X) -> itos(X);
s(X) when is_float(X)   -> ftos(X).

btos(X) -> X.
atos(X) -> atom_to_binary(X, utf8).
ltos(X) -> iolist_to_binary(X).
itos(X) -> ltos(itol(X)).
ftos(X) -> ltos(io_lib:format("~.9f", [X])).

%%
%% typecast scalar data type to Unicode binary or fails
-spec ls(_) -> binary().

ls(undefined)            -> <<>>;
ls(X) when is_binary(X)  -> utob(X);
ls(X) when is_atom(X)    -> atos(X);
ls(X) when is_list(X)    -> utob(X);
ls(X) when is_integer(X) -> itos(X);
ls(X) when is_float(X)   -> ftos(X).

utob(X) ->
   case unicode:characters_to_binary(X) of
      {incomplete, _} ->
         exit(rought);
      {error,      _} ->
         exit(badarg);
      Y ->
         Y
   end.

%%
%% typecast scalar data type to character list or fails
-spec c(_) -> list().

c(undefined)            -> [];
c(X) when is_binary(X)  -> btol(X);
c(X) when is_atom(X)    -> atol(X);
c(X) when is_list(X)    -> X;
c(X) when is_integer(X) -> itol(X);
c(X) when is_float(X)   -> ftol(X).

btol(X) -> binary_to_list(X).
atol(X) -> atom_to_list(X).
itol(X) -> integer_to_list(X).
ftol(X) -> lists:flatten(io_lib:format("~.9f", [X])).

%%
%% typecast scalar data type to Unicode character list or fails
-spec lc(_) -> list().

lc(undefined)            -> [];
lc(X) when is_binary(X)  -> utoc(X);
lc(X) when is_atom(X)    -> atol(X);
lc(X) when is_list(X)    -> utoc(X);
lc(X) when is_integer(X) -> itol(X);
lc(X) when is_float(X)   -> ftol(X).

utoc(X) ->
   case unicode:characters_to_list(X) of
      {incomplete, _} ->
         exit(rought);
      {error,      _} ->
         exit(badarg);
      Y ->
         Y
   end.

%%
%% typecast scalar data type to existing atom or fails
-spec a(_) -> atom().

a(X) when is_binary(X)  -> btoa(X);
a(X) when is_atom(X)    -> X;
a(X) when is_list(X)    -> ltoa(X);
a(X) when is_integer(X) -> itoa(X);
a(X) when is_float(X)   -> ftoa(X).

btoa(X) -> binary_to_existing_atom(X, utf8).
ltoa(X) -> list_to_existing_atom(X).
itoa(X) -> ltoa(itol(X)).
ftoa(X) -> ltoa(ftol(X)).

%%
%% typecast scalar data type to new atom or fails
-spec atom(_) -> atom().

atom(X) when is_binary(X)  -> btoaa(X);
atom(X) when is_atom(X)    -> X;
atom(X) when is_list(X)    -> ltoaa(X);
atom(X) when is_integer(X) -> itoaa(X);
atom(X) when is_float(X)   -> ftoaa(X).

btoaa(X) -> binary_to_atom(X, utf8).
ltoaa(X) -> list_to_atom(X).
itoaa(X) -> ltoaa(itol(X)).
ftoaa(X) -> ltoaa(ftol(X)).

%%
%% typecast scalar data type to hexadecimal or fails
-spec x(_) -> binary().

x(X) when is_binary(X)  -> btoh(X);
x(X) when is_atom(X)    -> btoh(atos(X));
x(X) when is_list(X)    -> btoh(ltos(X));
x(X) when is_integer(X) -> itoh(X).

btoh(X) ->
   << <<(if A < 10 -> $0 + A; A >= 10 -> $a + (A - 10) end):8>> || <<A:4>> <= X >>.

itoh(X) ->
   << <<(if A < $A -> A; A > $A -> $a + (A - $A) end):8>> || <<A:8>> <= erlang:integer_to_binary(X, 16) >>.
