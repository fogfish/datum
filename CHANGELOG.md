# Changelog

The library uses [semantic versions](http://semver.org) to identify stable releases. 

## Release 4.4.0

**Features**
* Implements basic lenses: identity (`lens:id/0`), constant (`lens:const/1`)
* Implements binary lenses: head of binary (`lens:bh/1`), tail of binary (`lens:bt/`), bit focus (`lens:bf/2`)


## Release 4.3.2

**Features**
* [#30](https://github.com/fogfish/datum/issues/30) Define a conditional fail (`require`) that conditionally lifts a value either to object or error of category
* [#29](https://github.com/fogfish/datum/issues/29) Support category transformers as part of nested expressions
* Define a generic operators to modify monad context or execute side-effect

**Improvements**

* Fix various of compilation errors of function compositions
* Improve test coverage of parse transforms
* Support list comprehensions and list operators as part of function composition 
* Export `datum:lens()` data type


## Release 4.3.0

**Features**

* [#20](https://github.com/fogfish/datum/issues/20) New category `undefined`  
* [#25](https://github.com/fogfish/datum/issues/25) Product lens combinator `lens:p(...)` to spawn multiple fields into abstract view
* [#19](https://github.com/fogfish/datum/issues/19) Macros to pattern match empty data structures (see `datum.hrl`)
* [#22](https://github.com/fogfish/datum/issues/22) Define new lens `lens:require/1`, `lens:defined/0` to support development of unit testing (validate nested structures using lenses) 

**Improvements**

* Improve interface semantic, introduce `lens:map/3` function instead of `lens:apply/3`
* Re-implement lens isomorphism feature using product lens
* Update documentations and add examples about lenses
* Use `option` data type to warp lens output



## Release 4.2.x
Data structure isomorphism with lenses

## Release 4.1.x 
Update interface(s) documentation

## Release 4.0.x 
Re-implement monads through Kleisli category

## Release 3.7.x
Introduce category pattern

## Release 3.4.x
Improve monads for usage in production 

## Release 3.3.x
Van Laahorven lenses and monads

## Release 2.7.x
Enhance pure functional data-types with abstract interfaces such Foldable, Collections, etc  

## Reelase 0.9.x
Implement pure functional data-types: trees, queues, streams, etc
