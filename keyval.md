# `package keyval`

Types and methods for persistent `value store`s and `key value store`s.  

Note: the following `scala` code is simplified for exposition.

### Companion root trait `WithValue`

```scala
trait WithValue {

  type Id = Sha /*: Order */
  type Repr = Map[Id, NonEmptyList[Row]]

  type Value    /*: Eq */
  type Row

  type Shape[_]
  type Spec

  type Model = Shape[Spec]
}
```
Type swag provider for the free type variable `Value`.
Any type `A` we bind to `Value` will have these provided.
`Value` types must have value equality semantics - of course!

#### `Id` computation using `sha`

A cryptographic hash function (secure hash algorithm: `sha`) is the basis of index computation.  

```scala
  /** assume for example - other choices would work here as well */
  type Sha = String Refined IsSha256AsBase58

  /** elides many details about canonical formats and codecs */
  def sha[A](a: A): Sha = { ... }
```
We can use the `sha` to compute an `Id` based only on the `Row`.
```scala

  /** content address for row */
  def rowSha: Row => Id =
    row => sha(row)
```
Or we can use a previous `Id` (computed from some prior `Row`)
to compute a new (`Merkel chain`ed) `Id` for a given `Row`.
```scala

  /** chain `Id`s together into a sequence using `sha` */
  def chain: Id => Id => Id =
    i => j => sha((i, j))

    /** chained address for row */
  def chainedRowSha: Id => Row => Id =
    id => row => chain(id)(sha(row))  
```

An `Id` calculated via `sha` can span multiple `Rows`.  

```scala

   /** content addressed `Id` spanning multiple `Row`s.
     *
     * single `sha` validates single transaction semantics
     */
   def rowsSha: (Row, List[Row]) => Id =
     (row, rows) => rows.foldLeft(rowSha(row))((s, r) => sha)

   /** chained addressed `Id`s spanning multiple `Row`s (single transaction) */
   def chainedRowsSha: Id => (Row, List[Row]) => Id =
     id => (row, rows) => chain(id)(rowsSha(row, rows))

```
Now we have all the types and methods necessary to compute `Id`s.  

### Companion mixin `WithId`
```scala
trait WithId extends WithValue {
  type Row = Value
}
```
For immutable `value store`s, the `Row` is just the `Value`.

#### content address models a `Set`

Id | Value
:--- | ---:
`a = rowSha` | 42.00
`b = rowSha` | 33.33
`a = rowSha` | 42.00

Here we compute the `Id` of the `Row` using a secure hash function
(`sha`).
This simple convention entails `Set` semantics: duplicates are (semantically) discarded;
identical `Row`s hash to the same `Id`.

```scala  
type Shape[_] = Set[_]
type Spec     = Value
// type Model = Set[Value]

val example: Model = Set(33.33, 42.00)
```

Note: duplicate rows are _silently_ elided.

Because the `Id` may be computed directly given the `Value`, or "content", to be referenced,
this type of `store` can be called `content addressed`. The sequential order of committed
`Value`s is not guaranteed to be maintained.

#### chained address models a `List`

Id | Value
:--- | ---:
`a = chainedRowSha(_)` | 42.00
`b = chainedRowSha(a)` | 33.33
`c = chainedRowSha(b)` | 42.00

This is _also_ a pure immutable `value store`, but with `chain`ed `Id`s. No longer content addressable! Duplicate `Row`s will hash to distinct `Id`s due to chaining.
Entails `List` semantics: the order of commitment may be proved by the `Id` sequence.


```scala  
type Shape[_] = List[_]
type Spec     = Value
// type Model = List[Value]

val example: Model = List(42.00, 33.33, 42.00)
```

Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

`Store`s of this shape are used for `model.Transaction`s.

All `Shape` specializations of [WithId](Companion mixin `WithId`) may use
*either* `content address` or `chained address` as `Id`.

#### specialization for `Map`s.

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSha` | USD | 10,000.00
`a`           | XAU |    420.33
`b = rowsSha` | XAU |    397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, V2]`.
`Id`s are computed across all `Row`s belonging
to the same `Map`.

```scala  
type Shape[_] = Set[_] // or List[_] for chained address!
type Spec     = Map[K2, V2]

val example: Model = Set (
  Map (
    USD ->  10000.00,
    XAU ->    420.33
  ),
  Map (
    XAU ->    397.23
  )
)
```

`Store`s of this shape are used e.g. to model `model.Trade`s (with `content address`ing).

#### specialization for `Lists`.

Id |  Value
:--- | ---:
`a = chainedRowSha(_)`  | XAU
`b = chainedRowsSha(a)` | XAU
`b`                     | CHF
`c = chainedRowsSha(b)` | USD
`c`                     | USD

This is a value store of `List[Value]` rows.

Rows that are committed together get the same `Id`.

```scala  
type Shape[_] = Set[_] // or List[_] for chained address!
type Spec     = List[Value]

val example: Model = Set(
  List(XAU),
  List(XAU ,
       CHF),
  List(USD ,
       USD)
)
```

`Store`s of this shape are used in this example to implement a
set of lists of arbitrary currencies.  

**TODO:** make this a set of `Command` or `Event` values of some kind - that shows off the `Shape`.

#### specialization for `Map`s of `Lists`.

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

May use either content address or chained address  for `Id`.
Duplicate rows must be appended within a single call, that computes a single `Id`.

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSha` | USD |10,000.00
`a`           | USD | 5,000.00
`a`           | USD | 5,000.00
`a`           | XAU | 420.33
`b = rowsSha` | XAU | 397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, List[V2]]`.
`Id`s are computed across all `Row`s belonging to the same `Map`.

In the example above, a `Map[K2, V2]` of size 3 is followed by a `Map` of size 1.

```scala  

type Shape[_] = Set[_] // or List[_]  for chained address!
type Spec     = Map[K2, List[V2]]

val example: Model = Set (
  Map (
    USD -> List(10000.00 ,
                5000.00 ,
                5000.00),
    XAU -> List(  420.33)
  ),
  Map (
    XAU -> List(   397.23)
  )
)
```

### Companion mixin `WithKey`

The use of chained address for `Id` is mandatory.

Rational: since we are tracing the dynamic evolution of a value indexed by a key,
multiple instances of the same Row entail positional significance in the data model.
(We can write the same row multiple times, and it matters in what order we write it.)

```scala
  trait WithKey extends WithValue {
    type Key /*: Order */
    type Row = (Key, Value)
  }
```

Id | (Key | Value)
:--- | --- | ---:
`a = chainedRowSha(_)` | USD | 10,000.00
`b = chainedRowSha(_)` | XAU | 420.33
`c = chainedRowSha(b)` | XAU | 397.23
`d = chainedRowSha(a)` | USD | 5,000.00

This is a key value store. The `Value` for a given `Key` evolves over time and the store records
each evolution, maintaining the entire `Value` history for each `Key`.

Note: `Id`s are chained per `Key`, which has these implications:

- the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.
- intermediate `Id` state must be maintained per key
    - (`Map[Key, Id]`)
    - scales with the number of unique keys!  
- chaining arbitrary blocks together is no longer straightforward  
    - (entails a `Map[Key, Id] => Map[Key, Id]` somewhere handy)

```scala  
type Shape[_] = Map[Key, _]
type Spec     = Value

val example: Model = Map(
  USD ->   5000.00,
  XAU ->    397.23
)
```

`Store`s of this shape are used in this example to map a currency to an amount.

#### specialization for `List`s

Id | (Key | Value)
:--- | --- | ---:
`a = chainedRowSha(_)`  | IBan.09993 | XAU
`b = chainedRowsSha(_)` | IBan.06776 | XAU
`b`                     | IBan.06776 | CHF
`c = chainedRowSha(_)`  | UsBan.04321 | USD
`d = chainedRowSha(b)`  | IBan.06776 | USD

This is a key value store of `List[Value]`s.

Note that `Row`s that are committed together get the same `Id`.

However, _all_ rows contribute to the data model!

```scala  
type Shape[_] = Map[Key, _]
type Spec     = List[Value]

val example: Model =
  Map(
    IBan.06776  -> List(XAU,
                        CHF,
                        USD)
    IBan.09993  -> List(XAU),
    UsBan.04321 -> List(USD),
  )
```

`Store`s of this shape are used in this example to implement a list of permitted currencies per
account.

---

#### specialization for `Map`s
```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.04321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993  | XAU |    420.33
`c = chainedRowSha(_)` | IBan.06776  | XAU |    397.23
`d = chainedRowSha(a)` | UsBan.04321 | USD |  5,000.00

This is a key value store of `Map[K2, V2]` rows.

```scala  
type Shape[_] = Map[Key, _]
type Spec     = Map[K2, V2]

val example: Model =
  Map(
    IBan.06776  -> Map(XAU ->    397.23),
    IBan.09993  -> Map(XAU ->    420.33),
    UsBan.04321 -> Map(USD ->   5000.00),
  )
```

`Store`s of this shape are used for `model.Folio`s

---

#### specialization for `Map`s of `List`s
```scala
  type K2 /*: Order */
  type V2 /*: Eq */
  type Value = (K2, V2)
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.04321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993 | XAU | 420.33
`c = chainedRowSha(_)` | IBan.06776 | XAU | 397.23
`d = chainedRowSha(a)` | UsBan.04321 | USD | 5,000.00

This is the most general structure supported.  

Note: same data commits as the previous example, but different semantics.

This is a key value store of `Map[K2, V2]` rows.

```scala  
type Shape[_] = Map[Key, _]
type Spec     = Map[K2, List[V2]]

val example: Model =
  Map(
    IBan.06776  -> Map(XAU -> List(  397.23)),
    IBan.09993  -> Map(XAU -> List(  420.33)),
    UsBan.04321 -> Map(USD -> List(10000.00  ,
                                    5000.00)),
  )
```

`Store`s of this shape are used for e.g. ???
TODO: identify use case, again probably using events as values

---

#### "deleting" `Key`s

`Value`s are `nullable` in the `WithKey` context.

In this example, the value associated with the key `USD` is semantically deleted.

Id | (Key | Value)
:--- | --- | ---:
`z` = chainedRowSha(`_`) | USD | `null`

For `Map[Map]` and `Map[Map[List]]` specializations, `null`ify the `K2` entry for a given `Key`

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`x` = chainedRowSha(`d`) | UsBan.04321 | `null` | `_`

#### deleting an entire `K2`

For `Map[Map]` and `Map[Map[List]]` specializations,
`null`ify the `V2` entry for a given `(Key, K2)`.

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`y` = chainedRowSha(`c`) | IBan.06776 | `XAU` | `_`
