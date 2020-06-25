# `package keyval`

Types and methods for persistent `value store`s and `key value store`s.  

Note: the following `scala` code is simplified for exposition.

### Companion mixin `WithValue`

```scala
trait WithValue {
  type Id = Sha
  type Row
  type Value /*: Eq */
  type Repr
  type Shape
}
```
Type swag for the free type variable `Value`. Any type `A` we bind to `Value` will have these provided. `Value` types must have value equality semantics - of course!

##### `Id` computation using `sha`

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
  type Repr = Map[Id, Row]
}
```
For immutable `value store`s, the `Row` is just the `Value`.

#### content address

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
type Shape = Set[Value]

val example: Shape = Set(33.33, 42.00)
```

Because the `Id` may be computed directly given the `Value`, or "content", to be referenced,
this type of `store` can be called `content addressed`. The sequential order of committed
`Value`s is not guaranteed to be maintained.

#### chained address

Id | Value
:--- | ---:
`a = chainedRowSha(_)` | 42.00
`b = chainedRowSha(a)` | 33.33
`c = chainedRowSha(b)` | 42.00

This is _also_ a pure immutable `value store`, but with `chain`ed `Id`s. No longer content addressable! Duplicate `Row`s will hash to distinct `Id`s due to chaining.
Entails `List` semantics: the order of commitment may be proved by the `Id` sequence.

Note: duplicate rows are _silently_ elided. 

```scala  
type Shape = List[Value]

val example: Shape = List(42.00, 33.33, 42.00)
```

Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

`Store`s of this shape are used for `model.Transaction`s.

##### specialization for `Set`s of `Map`s.

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
   type Repr = Map[Id, Set[K2, V2]]
```

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSha` | USD | 10,000.00
`a`           | XAU | 420.33
`b = rowsSha` | XAU | 397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, V2]`.
`Id`s are computed across all `Row`s belonging
to the same `Map`.

The `Id`s may be content addressed or chained.

In the example above, a `Map[K2, V2]` of size 2 is followed by a `Map` of size 1.

Note: The `Store` must **not** support innermost `List` arguments in the API
if the `Shape` will elide data. Why? Because the sha function will include data that is
subsequently erased, rendering the whole record inaccessible by `Id`.

```scala  
type Shape = Set[Map[K2, V2]]
val example: Shape = Set (
  Map (
    USD -> 10,000.00,
    // USD ->  5,000.00, would be nonsensical and broken in this context
    XAU ->    420.33
  ),
  Map (
    XAU ->    397.23
  )
)
```

`Store`s of this shape are used e.g. to model `model.Trade`s

#### specialization for `Set`s of `Lists`.

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
type Shape = Set[List[Value]]

val example: Shape = Set(
  List(XAU),
  List(XAU, CHF),
  List(USD, USD)
)
```

`Store`s of this shape are used in this example to implement an list of currencies.  

**TODO:** make this a set of `Command` or `Event` values of some kind - that shows off the `Shape`.

##### specialization for `Set`s of `Map`s of `Lists`.

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSha` | USD | 10,000.00
`a`           | USD | 5,000.00
`a`           | XAU | 420.33
`b = rowsSha` | XAU | 397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, List[V2]]`.
`Id`s are computed across all `Row`s belonging to the same `Map`.

The `Id`s may be content addressed or chained.

In the example above, a `Map[K2, V2]` of size 2 is followed by a `Map` of size 1.

Note: This `Store` **may** support innermost `List` arguments in the API
because the `Shape` comprehends all possible rows.

```scala  
type Shape = Set[Map[K2, List[V2]]]
val example: Shape = Set (
  Map (
    USD -> List(10,000.00, 5,000.00),
    XAU -> List(   420.33)
  ),
  Map (
    XAU -> List(   397.23)
  )
)
```

### Companion mixin `WithKey`
```scala
  trait WithKey extends WithValue {
    type Key /*: Order */
    type Row = (Key, Value)

    type Shape = Map[Key, Value]
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

`Id`s are chained per `Key`, which has these implications:

- the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.
- intermediate `Id` state must be maintained per key
    - (`Map[Key, Id]`)
    - scales with the number of unique keys!  
    - (`Map[Key, Id] => Map[Key, Id]`)
- chaining arbitrary blocks together is no longer straightforward  

##### specialization for `Map`s of `List`s

Id | (Key | Value)
:--- | --- | ---:
`a = chainedRowSha(_)`  | IBan.09993 | XAU
`b = chainedRowsSha(_)` | IBan.06776 | XAU
`b`                     | IBan.06776 | CHF
`c = chainedRowSha(_)`  | UsBan.04321 | USD
`d = chainedRowSha(b)`  | IBan.06776 | USD

This is a key value store of `List[Value]` rows.

Rows that are committed together get the same `Id`.

However, _all_ rows contribute to the data model.

```scala  
type Shape = Map[Key, List[Value]]

val example: Shape = Map(
  IBan.06776 -> List(XAU, CHF, USD)
  IBan.09993 -> List(XAU),
  UsBan.04321 -> List(USD),
)
```

`Store`s of this shape are used in this example to implement a list of permitted currencies per
account.

---

##### specialization for `Map`s of `Map`s
```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
   type Shape = Map[Key, Map[K2, V2]]
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.04321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993 | XAU | 420.33
`c = chainedRowSha(_)` | IBan.06776 | XAU | 397.23
`d = chainedRowSha(a)` | UsBan.04321 | USD | 5,000.00

This is a key value store of `Map[K2, V2]` rows.

`Store`s of this shape are used for `model.Folio`s

---

##### specialization for `Map`s of `Map`s of `List`s
```scala
  type K2 /*: Order */
  type V2 /*: Eq */
  type Value = (K2, V2)
  type Shape = Map[Key, Map[K2, List[V2]]]
  type SameShape = Map[(Key, K2), List[V2]]
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.04321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993 | XAU | 420.33
`c = chainedRowSha(_)` | IBan.06776 | XAU | 397.23
`d = chainedRowSha(a)` | UsBan.04321 | USD | 5,000.00

This is the most general structure supported.  

Note: same data commits as the previous example, but different semantics.

`Store`s of this shape are used for e.g. ???

---

##### "Deleting" `Key`s

`Value`s are `nullable` in the `WithKey` context.

In this example, the value associated with the key `USD` is semantically deleted.

Id | (Key | Value)
:--- | --- | ---:
`z` = chainedRowSha(`_`) | USD | `null`

For `Map[Map]` and `Map[Map[List]]` specializations, `null`ify the `K2` entry for a given `Key`

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`x` = chainedRowSha(`d`) | UsBan.04321 | `null` | `_`

##### deleting an entire `K2`

For `Map[Map]` and `Map[Map[List]]` specializations,
`null`ify the `V2` entry for a given `(Key, K2)`.

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`y` = chainedRowSha(`c`) | IBan.06776 | `XAU` | `_`
