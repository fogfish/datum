%%
%%   Copyright (c) 2018, Dmitry Kolesnikov
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
%%   Examples of Generic usage in Erlang
-module(examples_generic_io).
-behaviour(gen_server).

-export([
   send/1
,  recv/1
,  start_link/3
,  init/1
,  terminate/2
,  handle_call/3
,  handle_cast/2
,  handle_info/2
,  code_change/3
]).

-record(state, {encoder, decoder, generic}).

%%
%%
send(Struct) when is_tuple(Struct) ->
   gen_server:call(erlang:element(1, Struct), {send, Struct}).

%%
%%
recv(Struct) when is_tuple(Struct) ->
   gen_server:call(erlang:element(1, Struct), {recv, Struct}).

%%
%%
start_link(Type, Encoder, Decoder) ->
   gen_server:start_link({local, Type}, ?MODULE, [Encoder, Decoder], []).

init([Encoder, Decoder]) ->
   {ok, #state{encoder = Encoder, decoder = Decoder}}.

terminate(_, _) ->
   ok.

handle_call({send, Struct}, _, #state{encoder = Encoder} = State) ->
   {reply, ok,
      State#state{
         generic = Encoder(Struct)
      }
   };

handle_call({recv, _}, _, #state{decoder = Decoder, generic = Generic} = State) ->
   {reply, {ok, Decoder(Generic)}, State}.

handle_cast(_, State) ->
   {noreply, State}.

handle_info(_, State) ->
   {noreply, State}.

code_change(_, State, _) ->
   {ok, State}.

