### `package keyval`

Types and methods for persistent `value store`s and `key value store`s.  

Note: the following `scala` code is simplified.  


#### `WithValue`
```scala
trait WithValue { type Id = Sha; type Row; type Value /*: Eq */  ,,,
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

#### `WithId`
```scala
  trait WithId extends WithValue { type Row = Value }
```
For immutable `value store`s, the `Row` is just the `Value`.

##### content addressed

Id | Value
:--- | ---:
`a = rowSha` | 42.00
`b = rowSha` | 33.33
`a = rowSha` | 42.00

Here we compute the `Id` of the `Row` using a secure hash function
(`sha`).
This simple convention entails `Set` semantics: duplicates are (semantically) discarded;
identical `Row`s hash to the same `Id`.

Because the `Id` may be computed directly given the `Value`, or "content", to be referenced,
this type of `store` can be called `content addressed`. The sequential order of committed
`Value`s is not guaranteed to be maintained.

##### chained addressed

Id | Value
:--- | ---:
`a = chainedRowSha(_)` | 42.00
`b = chainedRowSha(a)` | 33.33
`c = chainedRowSha(b)` | 42.00

This is _also_ a pure immutable `value store`, but with `chain`ed `Id`s. No longer content addressable! Duplicate `Row`s will hash to distinct `Id`s due to chaining.
Entails `List` semantics: the order of commitment may be proved by the `Id` sequence.
Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

`Store`s of this shape are used for `model.Transaction`s.

##### specialization for `Set[Map[K2, V2]]` value store

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

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, V2]`.
`Id`s are computed across all `Row`s belonging
to the same `Map`.

The `Id`s may be content addressed or chained.

In the example above, a `Map[K2, V2]` of size 3 is followed by a `Map` of size 1.

`Store`s of this shape are used e.g. to model `model.Trade`s

#### `WithKey`  
```scala
  trait WithKey extends WithValue { type Key /*: Order */; type Row = (Key, Value) }
```

Id | (Key | Value)
:--- | --- | ---:
`a` = chainedRowSha(`_`) | USD | 10,000.00
`b` = chainedRowSha(`_`) | XAU | 420.33
`c` = chainedRowSha(`b`) | XAU | 397.23
`d` = chainedRowSha(`a`) | USD | 5,000.00

This is a key value store. The `Value` for a given `Key` evolves over time and the store records
each evolution, maintaining the entire `Value` history for each `Key`.

`Id`s are chained per `Key`, which has these implications:

- the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.
- intermediate `Id` state must be maintained per key
    - (`Map[Key, Id]`)
    - scales with the number of unique keys!  
- chaining arbitrary blocks together is no longer straightforward  
    - (`Map[Key, Id] => Map[Key, Id]`)

##### "Deleting" `Key`s

`Value`s are `nullable` in the `WithKey` context.

In this example, the value associated with the key `USD` is semantically deleted.

Id | (Key | Value)
:--- | --- | ---:
`z` = chainedRowSha(`_`) | USD | `null`


##### specialization for `Map`s of `Map`s
```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
   type type Shape = Map[Key, Map[K2, V2]]
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a` = chainedRowSha(`_`) | UsBan.04321 | USD | 10,000.00
`b` = chainedRowSha(`_`) | IBan.09993 | XAU | 420.33
`c` = chainedRowSha(`_`) | IBan.06776 | XAU | 397.23
`d` = chainedRowSha(`a`) | UsBan.04321 | USD | 5,000.00

This is a key value store of `Map[K2, V2]` rows.

`Store`s of this shape are used for `model.Folio`s

---

##### specialization for `Map`s of `Map`s of `List`s
```scala
  type K2 /*: Order */
  type V2 /*: Eq */
  type Value = (K2, V2)
  type type Shape = Map[Key, List[Map[K2, List[V2]]]]
```

Id | (Key | (K2 | V2))
:--- | --- | --- | ---:
`a` = chainedRowSha(`_`) | UsBan.04321 | USD | 10,000.00
`b` = chainedRowSha(`_`) | IBan.09993 | XAU | 420.33
`c` = chainedRowSha(`_`) | IBan.06776 | XAU | 397.23
`d` = chainedRowSha(`a`) | UsBan.04321 | USD | 5,000.00

This is the most general structure supported.  

Note: same data as the previous example, but different semantics.

`Store`s of this shape are used for e.g. ???

---
