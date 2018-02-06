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
%%   Examples of Lenses usage in Erlang
-module(examples_lens).

-compile(export_all).

%%
%% Ordinary data structures
-record(address, {
   street  = undefined :: string(),
   city    = undefined :: string(),
   zipcode = undefined :: string() 
}).

-record(person, {
   name    = undefined :: string(),
   age     = undefined :: integer(),
   address = undefined :: #address{}
}).


%%
%% Define lenses over data structures
lens_name()    -> lens:ti(#person.name).
lens_age()     -> lens:ti(#person.age).
lens_address() -> lens:ti(#person.address).
lens_street()  -> lens:c(lens:ti(#person.address), lens:ti(#address.street)).
lens_city()    -> lens:c(lens:ti(#person.address), lens:ti(#address.city)).
lens_zipcode() -> lens:c(lens:ti(#person.address), lens:ti(#address.zipcode)).


%%
%% A person
person() ->
   #person{
      name    = "Verner Pleishner",
      age     = 64,
      address = #address{street = "Blumenstrasse", city = "Berne", zipcode = "3013"}
   }.


%%
%% Use lenses as data setters/getters
read_field() ->
   64 = lens:get(lens_age(), person()).

update_field() ->
   Person = lens:put(lens_age(), 70, person()),
   #person{age = 70} = Person.

transform_field() ->
   Person = lens:apply(lens_age(), fun(X) -> X + 1 end, person()),
   #person{age = 65} = Person.



%%
%% Use lenses with nested structures
read_nested_field() ->
   "Blumenstrasse" = lens:get(lens_street(), person()).

update_nested_field() ->
   Person = lens:put(lens_street(), "Blumenstraße", person()),
   #person{address = #address{street = "Blumenstraße"}} = Person.

transform_nested_field() ->
   Person = lens:apply(lens_street(), fun(X) -> X ++ " 14" end, person()),
   #person{address = #address{street = "Blumenstrasse 14"}} = Person.



%%
%% Create a product lens, spawning multiple fields
lens_name_age_city() ->
   lens:p(lens_name(), lens_age(), lens_city()).


read_product_lens() ->
   ["Verner Pleishner", 64, "Berne"] = lens:get(lens_name_age_city(), person()).

update_product_lens() ->
   Person = lens:put(lens_name_age_city(), ["Pleishner", 65, "Bern"], person()),
   #person{name = "Pleishner", age = 65, address = #address{city = "Bern"}} = Person.


%%
%% Create a lens isomorphism, transforming a data to new format
lmap_name_age_city() ->
   lens:p(lens:at(name), lens:at(age), lens:at(city)).

transform_record_to_map() ->
   Person = lens:iso(lens_name_age_city(), person(), lmap_name_age_city(), #{}),
   #{name := "Verner Pleishner", age := 64, city := "Berne"} = Person.


%%
%% Define a custom lens
lens_custom() ->
   fun(Fun, Struct) ->
      lens:fmap(fun(X) -> lens_custom_put(X, Struct) end, Fun(lens_custom_get(Struct)) )
   end.

lens_custom_get(#{custom := [H|_]}) ->
   H.
lens_custom_put(X, #{custom := [_|T]} = Struct) ->
   Struct#{custom => [X|T]}.

struct() ->
   #{custom => [a, b, c]}.

read_custom_struct() ->
   a = lens:get(lens_custom(), struct()).

update_custom_struct() ->
   Struct = lens:put(lens_custom(), 1, struct()),
   #{custom := [1, b, c]} = Struct.



run() ->
   read_field(),
   update_field(),
   transform_field(),

   read_nested_field(),
   update_nested_field(),
   transform_nested_field(),

   read_product_lens(),
   update_product_lens(),

   transform_record_to_map(),

   update_custom_struct(),
   ok.


