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
-module(examples_generic).
-compile({parse_transform, generic}).

-compile(export_all).

-record(address, {
   street  = undefined :: string(),
   city    = undefined :: string(),
   zipcode = undefined :: string() 
}).

-record(person, {
   name    = undefined :: string(),
   age     = undefined :: integer(),
   address = undefined :: string()
}).

-record(employee, {
   name    = undefined :: string(),
   age     = undefined :: integer(),
   address = undefined :: string()
}).

-record(estate, {
   location = undefined :: string() | #address{},
   owner    = undefined :: string() | #person{}
}).

person() ->
   #person{
      name    = "Verner Pleishner",
      age     = 64,
      address = "Blumenstrasse 14, Berne, 3013"
   }.

address() ->
   #address{
      street  = "Blumenstrasse 14",
      city    = "Berne",
      zipcode = "3013"
   }.

%%
%%
semi_auto() ->
   Person = person(),
   Generic = generic_of:person(Person),
   Person = generic_to:person(Generic).

semi_auto_morphism() ->
   Person = person(),
   Generic = generic_of:person(Person),
   _Employee = generic_to:employee(Generic).

semi_auto_recursive_type_list() ->
   Persons = [person() || _ <- lists:seq(1, 10)],
   Generic = generic_of:person(Persons),
   _Employees = generic_to:employee(Generic).

%%
%%
assisted_morphism() ->
   Person = person(),
   Generic = generic_of:person([name, age, address], Person),
   _Employee = generic_to:estate([address, name], Generic).

%%
%%
partial_application() ->
   Encoder = generic:encode(#person{}),
   Decoder = generic:decode(#person{}),

   Person = person(),
   Generic = Encoder(Person),
   Person = Decoder(Generic).

generic_process() ->
   Encoder = generic:encode(#person{}),
   Decoder = generic:decode(#person{}),
   {ok, _} = examples_generic_io:start_link(person, Encoder, Decoder),

   Person = person(),
   ok = examples_generic_io:send(Person),
   {ok, Person} = examples_generic_io:recv(#person{}).

%%
%%
custom_codecs() ->
   Person  = person(),
   Address = address(),
   Generic = generic_of:estate(#estate{
      location = generic_of:address(Address),
      owner    = generic_of:person(Person)
   }),
   Lens = lens:p(#estate{
      location = lens:c(lens:at(location), generic:lens(#address{})),
      owner    = lens:c(lens:at(owner), generic:lens(#person{}))
   }),
   _Estate = lens:get(Lens, Generic).

