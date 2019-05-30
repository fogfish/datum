# Generic representation

> Generic programming is a style of computer programming in which algorithms are written in terms of types to-be-specified-later that are then instantiated when needed for specific types provided as parameters. -- said by [Wikipedia](https://en.wikipedia.org/wiki/Generic_programming).

Erlang permits writing a generic code. It is known to be dynamic, strong typing language, the article [Types (or lack thereof)](https://learnyousomeerlang.com/types-or-lack-thereof) describes it in excellent manner. The in-depth study about type systems have been presented at [Point Of No Local Return: The Continuing Story Of Erlang Type Systems](http://www.erlang-factory.com/static/upload/media/1465548492405302zeeshanlakhanipointofnolocalreturn.pdf). In this section, we would not discuss type systems. Instead we look on other aspect closely related to types.

Type theory and a functional programming operates with [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type). They are known as a composition of other types. The theory defines two classes of compositions: product types (tuples, records) and co-product types (sum, enumeration or variant types). Product types are strongly expressed by [records](http://erlang.org/doc/reference_manual/records.html) in Erlang; co-products are loosely defined (we skip them at current library release). 

```erlang
-type fullname() :: binary().
-type address()  :: binary().
-type city()     :: binary().

-record(person, {
   name    :: fullname(),
   address :: address(), 
   city    :: city()
}).
```

The beauty of Erlang records (product type) is that they definitions are only available at compile time. The compiler has complete knowledge of defined "algebra" and catches misuse errors. The usage of records in your code benefits to write correct, maintainable code and support refactoring. Use them to define your domain models!

```erlang
#person{birthday = "18810509"}.

%% Compiling src/person.erl failed
%% src/person.erl:18: field birthday undefined in record person
```

## Generic programming with records

Erlang offers few features that helps with generic programming:

1. record expressions are translated to tuple by compiler, use `element/2` and `setelement/3` for generic access to tuple elements.
2. functions `tuple_to_list/1` and `list_to_tuple/1` are transformers to another an alternative generic representation.
3. pseudo function `record_info/2` to obtain record structure.

As an example, a typical macro here to cast record type to the map:

```erlang
-define(encode(Type, Struct),
   maps:from_list(
      [{Key, Value} ||
         {Key, Value} <- lists:zip(
            record_info(fields, Type),
            tl(tuple_to_list(Struct))
         ),
         Value /= undefined,
         Value /= null
      ]
   )
).
```

Unfortunately, usage of records have a couple of disadvantages that makes they usage questionable:

* external serialization of domain models often requires knowledge of internals about records: types, attributes names, cardinality, etc. This information is not implicitly available unless you import `.hrl` file with records definition. It will be available for you only at compile time. 
* record transformations to other record or any external format requires boilerplate code.
* generic programming with records requires macros.
* new built-in data type `map()` obsoletes usage of records for casual use-cases.

For these reasons, we would like to improve records runtime flexibility through `parse_transform` while keeping its compile-time benefits for domain modeling.

The latest release of library aims on most common tasks in software engineering: representation switching or data transformation.


## Switching representations

`datum` provides a `parse_transform` called `generic` that allows us to switch back and forth between a concrete records (ADT) and its generic representation without boilerplate. Please not that the generic representation is opaque structure for your code. Please follow examples of generic [here](../examples/src/examples_generic.erl).

<!--
Let's take a closer look at generic semantic:

```erlang
-type generic() :: map().
-type t()       :: tuple().

-spec generic_of:t( t() ) -> generic().
-spec generic_to:t( generic() ) -> t().
```
-->


### Semi-auto derivation

It is convenient to have just encoder/decoder. Semi-auto codec makes a magic of switching representation between different ADTs.

```erlang
-compile({parse_transform, generic}).

-record(person, {name, age, address}).

person() ->
   #person{
      name    = "Verner Pleishner",
      age     = 64,
      address = "Blumenstrasse 14, Berne, 3013"
   }.

semi_auto() ->
   Person = person(),
   Generic = generic_of:person(Person),
   Person = generic_to:person(Generic).   
```

The generic representation carries-on instances of records fields and associated metadata, the current implementation uses `map()` but this is subject to change in future release of the library. If two ADTs have the similar representation we can convert back and forth between them using their generics representation:

```erlang
-compile({parse_transform, generic}).

-record(person, {name, age, address}).
-record(employee, {name, age, address}).

Generic = generic_of:person(#person{ ... }).
Employee = generic_to:employee(Generic).
```

Please note that "similar" representation means -- subset of common fields.

```erlang
-compile({parse_transform, generic}).

-record(person, {name, age, address}).
-record(visitor, {name}).
-record(location, {address}).

Generic = generic_of:person(#person{ ... }).
Visitor = generic_to:visitor(Generic).
Location = generic_to:location(Generic).
```

Semi-auto derivation works with recursive types but current version supports only lists.

```erlang
-spec fetch_list_of_persons() -> [#person{}].

Generic = generic_of:person( fetch_list_of_persions() ).
Employee = generic_to:employee(Generic).
```


### Assisted derivation

It is also possible to customize semi-auto encoders/decoders for records. The feature supports morphism if two ADTs do not share similar representation.

```erlang
-compile({parse_transform, generic}).

-record(person, {name, age, address}).
-record(estate, {location, owner}).

person() ->
   #person{
      name    = "Verner Pleishner",
      age     = 64,
      address = "Blumenstrasse 14, Berne, 3013"
   }.

assisted_morphism() ->
   Person = person(),
   Generic = generic_of:person([name, age, address], Person),
   Employee = generic_to:estate([address, name], Generic).
```


### Partial application

Partial application is a tool to make a generic processing for your data domain (e.g. generic input/output to database). The generic implements helper utilities that returns encoder/decoder as a function.

```erlang
-compile({parse_transform, generic}).

-record(person, {name, age, address}).

Encoder = generic:encode(#person{}).
Decoder = generic:decode(#person{}).
```

Use this functions to switch representation of your records at runtime.

```erlang
person() ->
   #person{
      name    = "Verner Pleishner",
      age     = 64,
      address = "Blumenstrasse 14, Berne, 3013"
   }.

Person = person().
Generic = Encoder(Person).
Person = Decoder(Generic).
```

You can pass these codec functions to any processes named after the records type. Then the implementation of these process needs to deal only with aspects of generic data processing. See its [example](../examples/src/examples_generic_io.erl).

```erlang
{ok, _} = examples_generic_io:start_link(person, Encoder, Decoder).

examples_generic_io:send(Person).
examples_generic_io:recv(#person{}).
```


### Custom encoders/decoders

Only flat data structures are supported at the moment. You still need to deal with custom derivation. There are few approaches here

You can write encoder from scratch. In many cases semi-auto and assisted derivation helps you here -- you only writes a custom code for container types but you benefit of derived codec for atomic one.

```erlang
-compile({parse_transform, generic}).

-record(address, {street, city, zipcode}).
-record(person, {name, age, address}).
-record(estate, {location, owner}).

custom_codec() ->
   Person  = person(),
   Address = address(),
   Generic = generic_of:estate(#estate{
      location = generic_of:address(Address),
      owner    = generic_of:person(Person)
   }).      
```

The custom decoder requires traversal through generic data structures. This task do not differs from traditional approach, which involves a portion of boilerplate. The library offers only a [lens abstraction](lens.md) to solve the problem in a relatively boilerplate-free way.

```erlang

custom_decoder() ->
   ...
   Lens = lens:p(#estate{
      location = lens:c(lens:at(location), generic:lens(#address{})),
      owner    = lens:c(lens:at(owner), generic:lens(#person{}))
   }),
   Estate = lens:get(Lens, Generic).

```

Please note that lens isomorphism is an alternative abstraction that facilitates variety of transformation use-cases. You can implement custom codec just with lenses but you might observe that your code becomes verbose. 


## Conclusion

Generic programming is widely known technique, which is well supported by Erlang. This effort makes a convenient to convert specific types into generic ones that we can manipulate with common code. The defined technique has been used to implement RESTfull API and AWS DynamoDB serialization logic, while keeping domain model as Erlang records for purpose of maintainability and catches misuse errors at compile time.
