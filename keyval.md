# `package keyval`

Types and methods for persistent `value store`s and `key value store`s.  

Note: the following `scala` code is simplified for exposition.

### `Id` computation using `sha`

A cryptographic hash function (secure hash algorithm: `sha`) is the basis of index computation.  

```scala
  /** assume for example - other choices would work here as well */
  type Sha = String Refined IsSha256AsBase58

  /** elides many details about canonical formats and codecs */
  def sha[A](a: A): Sha = { ... }
```
Assume a suitable type `Row` that represents the bits to be persisted in association
with a given `Id`. (`Row` will be elaborated in the following section.)

We can use `sha` to compute an `Id` based only on instances of `Row`:

```scala

  /** content address for row */
  def rowSha: Row => Id =
    row => sha(row)
```
Or, we can use a previous `Id` (computed from some prior `Row`)
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
     (row, rows) => rows.foldLeft(rowSha(row))((s, r) => chain(s)(sha(r)))

   /** chained addressed `Id`s spanning multiple `Row`s (single transaction) */
   def chainedRowsSha: Id => (Row, List[Row]) => Id =
     id => (row, rows) => chain(id)(rowsSha(row, rows))

```
This specifies all the types and methods necessary to compute `Id`s for any Store

### Companion root trait `WithValue`

```scala
trait WithValue[V] {

  // bound type members

  type Id    = Sha  /*: Order */
  type Value = V    /*: Eq */

  // free type members  

  type Row

  type Shape[_]
  type Spec

  // derived type members

  type Repr    = Map[Id, Row]
  type Record  = (Id, Row)
  type StreamF[Record] = fs2.Stream[cats.effect.IO, Record] // for example

  type Model  = Shape[Spec]
}
```
Type swag provider for the free type variable `Value`.
Any type `A` we bind to `Value` will have these provided.
`Value` types must have value equality semantics - of course!

### Companion mixin `WithId`
```scala
trait WithId[V] extends WithValue[V] {
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
this type of `store` can be called `content addressed`.

Note: The sequential order of committed `Value`s is not guaranteed to be maintained.

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
*either* `content address` or `chained address` functions to compute `Id`.

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

#### specialization for `Nel`s.

Id |  Value
:--- | ---:
`a = chainedRowSha(_)`  | XAU
`b = chainedRowsSha(a)` | XAU
`b`                     | CHF
`c = chainedRowsSha(b)` | USD
`c`                     | USD

This is a value store of `Nel[Value]` rows.

Rows that are committed together get the same `Id`.

```scala  
type Shape[_] = Set[_] // or List[_] for chained address!
type Spec     = Nel[Value]

val example: Model =
  Set(
    Nel(XAU),
    Nel(XAU ,
        CHF),
    Nel(USD ,
        USD)
  )
```

`Store`s of this shape are used in this example to implement a
set of lists of arbitrary currencies.  

**TODO:** make this a set of `Command` or `Event` values of some kind - that shows off the `Shape`.

#### specialization for `Map`s of `Nel`s.

```scala
   type K2 /*: Order */
   type V2 /*: Eq */
   type Value = (K2, V2)
```

May use either content address or chained address for `Id`.
Duplicate rows must be appended within a single call that computes a single `Id`.

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSha` | USD |10,000.00
`a`           | USD | 5,000.00
`a`           | USD | 5,000.00
`a`           | XAU |   420.33
`b = rowsSha` | XAU |   397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, Nel[V2]]`.
`Id`s are computed across all `Row`s belonging to the same `Map`.

In the example above, a `Map[K2, V2]` of size 3 is followed by a `Map` of size 1.

```scala  

type Shape[_] = Set[_] // or List[_]  for chained address!
type Spec     = Map[K2, Nel[V2]]

val example: Model =
  Set (
    Map (
      USD -> Nel(10000.00 ,
                  5000.00 ,
                  5000.00),
      XAU -> Nel(  420.33)
    ),
    Map (
      XAU -> Nel(  397.23)
    )
  )
```

### Companion mixin `WithKey`

The use of chained address for `Id` is mandatory.

Rational: since we are tracing the dynamic evolution of a value indexed by a key,
multiple instances of the same `Row` entail positional significance in the data model.
(We can write the same row multiple times, and it matters in what order we write it.)

```scala
  trait WithKey[K, V] extends WithValue[V] {
    type Key  = K /*: Order */
    type Row  = (Key, Option[Value])
  }
```

Id | (Key | Option[Value])
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

val example: Model =
  Map(
    USD -> 5000.00,
    XAU ->  397.23
  )
```

`Store`s of this shape are used in this example to map a currency to an amount.

#### specialization for `Nel`s

Id | (Key | Option[Value])
:--- | --- | ---:
`a = chainedRowSha(_)`  | IBan.09993 | XAU
`b = chainedRowsSha(_)` | IBan.06776 | XAU
`b`                     | IBan.06776 | CHF
`c = chainedRowSha(_)`  | UsBan.5321 | USD
`d = chainedRowSha(b)`  | IBan.06776 | USD

This is a key value store of `Nel[Value]`s.

Note that `Row`s that are committed together get the same `Id`.

However, _all_ rows contribute to the data model!

```scala  
type Shape[_] = Map[Key, _]
type Spec     = Nel[Value]

val example: Model =
  Map(
    IBan.06776 -> Nel(XAU ,
                      CHF ,
                      USD),
    IBan.09993 -> Nel(XAU),
    UsBan.5321 -> Nel(USD),
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
   type Row  = (Key, Option[(K2, Option[V2])])
```

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.5321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993 | XAU |    420.33
`c = chainedRowSha(_)` | IBan.06776 | XAU |    397.23
`d = chainedRowSha(a)` | UsBan.5321 | USD |  5,000.00

This is a key value store of `Map[K2, V2]` rows.

```scala  
type Shape[_] = Map[Key, _]
type Spec     = Map[K2, V2]

val example: Model =
  Map(
    IBan.06776 -> Map(XAU ->  397.23),
    IBan.09993 -> Map(XAU ->  420.33),
    UsBan.5321 -> Map(USD -> 5000.00),
  )
```

`Store`s of this shape are used for `model.Folio`s

---

#### specialization for `Map`s of `Nel`s

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`a = chainedRowSha(_)` | UsBan.5321 | USD | 10,000.00
`b = chainedRowSha(_)` | IBan.09993 | XAU |    420.33
`c = chainedRowSha(_)` | IBan.06776 | XAU |    397.23
`d = chainedRowSha(a)` | UsBan.5321 | USD |  5,000.00

This is a key value store of `Map[K2, Nel[V2]]` rows
(the most general structure supported).

Note: same data commits as the previous example, but different semantics.


```scala  
type Shape[_] = Map[Key, _]
type Spec     = Map[K2, Nel[V2]]

val example: Model =
  Map(
    IBan.06776 -> Map(XAU -> Nel(  397.23)) ,
    IBan.09993 -> Map(XAU -> Nel(  420.33)) ,
    UsBan.5321 -> Map(USD -> Nel( 10000.00  ,
                                   5000.00)),
  )
```

`Store`s of this shape are used for e.g. ???
TODO: identify use case, again probably using events as values

---

#### "deleting" a `Key`

`Value`s are `nullable` in the `WithKey` context.

In this example, the value associated with the key `USD` is semantically deleted.

Id | (Key | Option[Value])
:--- | --- | ---:
`z` = chainedRowSha(`_`) | USD | `null`

For `Map[Map]` and `Map[Map[Nel]]` specializations, `null`ify both the `K2` field for a given `Key`.

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`x` = chainedRowSha(`d`) | UsBan.5321 | `null` | `null`

#### "deleting" a `K2`

For `Map[Map]` and `Map[Map[Nel]]` specializations,
`null`ify the `V2` field for a given `(Key, K2)`.

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`y` = chainedRowSha(`c`) | IBan.06776 | `XAU` | `null`

---

### codecs interior to companion objects

```scala

// identity, basically
// overrideable (?!) SADT / Json stuff
def fromValue[A](v: Value)(implicit asA: Value <~< A): A = asA 
def toValue(a: A)(implicit asValue: A <~< Value): Value  = asValue 

// `List`
def fromValues[A](vs: List[Value])(implicit asA: Value <~< A): List[A] = 
  vs map fromValue

def toValues(az: List[A])(implicit asValue: A <~< Value): List[Value]  = 
  az map toValue 

// TODO: consider `Stream` instead of `List` here 
def fromValues[K2: Order, V2: Eq](vs: List[Value])(
  implicit asK2V2: Value <~< (K2, V2)
): Map[K2, V2] = ...

// ditto
def toValues[K2: Order, V2: Eq](k2v2s: Map[K2, V2])(
  implicit asK2V2: (K2, V2) <~< Value
): List[Value] = ...

// collect the `Debit`s and `Credit`s with pattern matching (`unapply`)
// to extract the two `Map`s from one `List` for `Balance`
def fromValues[C: Currency](vs: List[Value])(
  implicit asBalanceRow: Value <~< (AccountingKey, Money[C])
): Balance[C] = ...

// one big list, as if one map
def toValues[C: Currency](balance: Balance[C])(
  implicit asValue: (AccountingKey, Money[C]) <~< Value
): List[Value] = ...

// no general implementation 
// special case override 
// used for `Roster` (others?)
def fromValues[A](vs: List[Value]): A
def toValues[A](a: A): List[Value]
```

### TODO

- KeyValueStore API: 12 distinct versions
```
  { concat, sum, replace } X { delete / no delete } X { single / multi }
```
- use foldMap to implement `snapshot` on all, `CQRS/ES` style
- `snapshot` effectively preserves cryptographic history 
    - by chaining `snapshot` results into the source stream


