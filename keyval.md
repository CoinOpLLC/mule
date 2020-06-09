### `package keyval`

Types and methods for persistent `value store`s and `key value store`s.  

Note: the following `scala` code is simplified.  

```scala
/** assume for example - other choices would work here as well */
type Sha = String Refined IsSha256AsBase58

/** elides many details about canonicalization and codecs */
def sha[A](a: A): Sha = { ... }
```
A cryptographic hash function (secure hash algorithm: `sha`) is the basis of index computation.  

#### `WithValue`
```scala
  trait WithValue { type Id = Sha; type Row; type Value /*: Eq */ }
```
Type swag for the free type variable `Value`. Any type `A` we bind to `Value` will have these provided. `Value` types must have value equality semantics - of course!
```scala  
  type RowSha = Row => Id
  def rowSha: RowSha = row => sha[Row](row)
```
Now we have all the types and methods necessary to compute `Id`s.  

#### `WithId`
```scala
  trait WithId extends WithValue { type Row = Value }
```
For immutable `value store`s, the `Row` is just the `Value`.

##### content address

Id | Value
:--- | ---:
`a` = rowSha | 42.00
`b` = rowSha | 33.33
`a` = rowSha | 42.00

Here we compute the `Id` of the `Row` using a secure hash function
(`sha`), and therefore possesses `Set` semantics without capturing the sequential order of
commitment. Duplicates are semantically discarded; identical `Row`s hash to the same `Id`. Because
the `Id` may be computed directly given the `Value`, this type of `store` can be called
`content addressed`.

##### chained address

We can use a previous `Id` (computed from some prior `Row`)
to compute a new (Merkel chained) `Id` for a given `Row`.

```scala
  def chainedRowSha: Id => RowSha =
    id => row => sha[(Id, Row)]((id, row))
```

Id | Value
:--- | ---:
`a` = chainedRowSha(`_`) | 42.00
`b` = chainedRowSha(`a`) | 33.33
`c` = chainedRowSha(`b`) | 42.00

This is _also_ a pure immutable `value store`, but with chained `Id`s. No longer `content addressable`. Duplicate `Row`s will hash to distinct `Id`s due to chaining. The order of commitment
may be proved by the `Id` sequence. Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

`Store`s of this shape are used for `model.Transaction`s.

##### specialization for `Set[Map[K2, V2]]` value store

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

Id | (K2 | V2)
:--- | --- | ---:
`a` = rowSha | USD | 10,000.00
`a` | USD | 5,000.00
`a` | XAU | 420.33
`b` = rowSha | XAU | 397.23

This is a pure value store of `Map[K2, V2]` `Row`s. `Id`s are computed across all `Row`s belonging
to the same `Map`. The `Id`s _may_ be computed with chainedRowSha although this is an implementation
detail (which will matter to verifiers).

In the example above, a `Map[K2, V2]` of size 3 is followed by a `Map` of size 1.

`Store`s of this shape are used for `model.Trade`s

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

  - intermediate state must be maintained per key
      - scales with the number of unique keys!  
      - chaining blocks is no longer straightforward  
  - the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.

##### specialization for `Map[Key, Map[K2, V2]]` key value store
```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
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

### What is a `Blotter`, anyway?

Here's an example "blotter" gathered randomly from the web - for reference only
there is a lot wrong with this example, actually, and it won't translate directly or uniquely

- Client name
- Trade name
- Settlement Date
- Buy/Sell
- CUSIP
- SecuritySymbol
- SecurityDesc.
- Quantity
- UnitPrice
- Principal/Proceeds
- TotalCommission
- Fees
- Net Proceeds
- Broker
