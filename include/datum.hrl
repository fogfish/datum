%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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

-define(NULL, undefined).

%%
%% 
-define(None,    undefined).
-define(Some(X), X).

%%
%% either category pattern match
-define(EitherR(X),  {ok,    X}).
-define(Right(X),    {ok,    X}).

-define(EitherL(X),  {error, X}).
-define(Left(X),     {error, X}).


%%
%% data pattern match macro  
-define(tree(Ord, T),      {t, Ord, T}).

-define(heap(Ord, H),      {h, Ord, H}).

%%
-record(stream, {
   head = undefined :: _,
   tail = undefined :: datum:option(fun(() -> _))
}).

%%
-record(queue, {
   length = 0  :: integer(),
   head   = [] :: [_],
   tail   = [] :: [_]
}).

% -define(stream(Head, Fun), {s, Head, Fun}).
% -define(stream(),          {s}).

% -define(queue(Length, Head, Tail), {q, Length, Head, Tail}).
