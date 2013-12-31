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
%% @description
%%   pure functional data structures
-module(datum).

-export([
   is_queue/1
]).

%%
%% data types
-type(q()      :: {q, integer(), list(), list()}).
-type(stream() :: {s, any(), function()}).

-export_type([
   q/0
  ,stream/0
]).

%%
%%
-spec(is_queue/1 :: (any()) -> true | false).

is_queue({q, _, _, _}) ->
   true;
is_queue(_) ->
   false.



