# Datum Cheat sheet

* [Lens](#lens)


## Lens

Focus on element with

Data type | Lens
--- | ---
List | `lens:hd()`, `lens:tl()`
Tuple | `lens:t1()`, `lens:t2()`, `lens:t3()`, `lens:ti(Int)`
Map | `lens:at(Key)`
KeyList | `lens:keylist(Int, Key)`
PropList | `lens:pair(Key)`

Lens support **composition** of lenses with `lens:c(...)` and construction of **product** lens  `lens:p(...)`


Use lenses to

Intent | Interface
--- | ---
Get | `lens:get/2`
Put | `lens:put/3`
Update | `lens:map/3`
Transform structure | `lens:iso/4`



