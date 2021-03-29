[[toc]]

# The `keyval` package.

A crypto-friendly functional key value algebra family and functional relational mapper.

## Introduction

We observe that the business data object lifecycle procedes inexorably thus:

`In Process ==> ... ==> On Ice`

This exposition will cover the two endpoints (where `On Ice` is taken to be `.csv`).

### Immutable, Append-Only Data Model

Every level of persistence in between these two endpoints can be viewed as a _cache_ of some kind. That cache architecture is dramatically simplified by adopting a data model which is _immutable_ and _append-only_.

What role is left for the treacherous but appealing abstraction known as a "variable?"

Tracing the evolution of the value associated with a key, by folding the values associated with that key to provide a "sum" of some kind, is the technique by which "state variables" are created and maintained within memory.

Recording only the intent to delete identified data, and not the actual data, is usually a good practice. The exception would be where data _must_ be erased (e.g. for regulatory reasons). It will be necesssary to make a snapshot of the folded-up structure in a table or file which **completely replaces** the previous data in order to physically delete data. This will become necessary over time if bounded storage requirements are respected.

### Cryptographic Requirements

[normalization and uniqueness - reference XML Signature spec]

## Indexing Types

`deftrade` defines a [convention over configuration](https://en.wikipedia.org/wiki/Convention_over_configuration) system for `Id`s and `Key`s.

### Id

The generation of unique primary key `Id` instances has been left to the database itself in past architectures. Our approach is to compute a cryptographic `Id` over the set of persisted `Row`s representing an atomic append operation. These sets of rows may be independent (and therefore content addressable), or `chained` together such that the `Id` of each atomic `Row` set is dependent on the `Id` computed for each previous append.

`Order` and `Show` typeclass instances are defined naturally for `Id`.

The specific secure hash algorithm is bound by the application developers.

### Key

`Key` types must have `Order` and `Show` typeclass instances defined and in scope.

Typical `Key` types:
- domain legible (e.g. `Account.No`)
- opaque types (e.g. `UUID`s)
- foreign `Id` types
- foreign `Key` types

### Index Conventions

In contrast to some other programming conventions, a `type Foo` may not contain instances of, nor depend upon, type `Foo.Id` or `Foo.Key`.

Stated simply: there will be no `id: Id` or `key: Key` fields within domain types! Index types are stored separately at every level of the storage hierarchy (e.g. `key`s in an in-memory `scala.collection.Map`)

However, **foreign keys** which reference other domain value types are permitted within value types.

### `Value`s

Most often represents some kind of [value object](https://medium.com/swlh/value-objects-to-the-rescue-28c563ad97c6). Always requires typeclass instances for `Eq`, and `Show`. Note, `Value` is not the same as `Spec` (more later).

### `Id` calculation

We can compute each `Id` individually and independently, in which case we will obtain `Set` semantics for the `Store`.

Or, we can chain the computations together, and make each `Id` dependent on the previous. In this case, `List` semantics for the Store will obtain. Further, each block will entail
- an input: a virtual "previous `Id`", which can be thought of as an Initialization Vector (IV)
- an output: a (maximally lopsideded) Merkle root. Can be used to initialize subsequent blocks.

#### Content Addressing

As mentioned in the previous section, a cryptographic hash function (secure hash algorithm: `sha`) is the basis of index computation.

**Note**: the following `scala` code is _simplified_ for exposition.

```scala
/** assume for example - other choices would work here as well */
   type SHA = String Refined IsSHA256AsBase58

/** magical code! elides many details about canonical formats and codecs! */
   def sha[A](a: A): SHA = { ... }
```
Assume a suitable type `Row` that represents the bits to be persisted in association with a given `Id`. (`Row` will be elaborated in the following section.)

We can use `sha` to compute a unique `Id` based only on instances of `Row`:

```scala
type Id = SHA

/** content address for row */
def rowSHA: Row => Id =
  row => sha(row)
```
**Note:** An `Id` calculated via `sha` can (and often does) span **multiple** `Rows`.

```scala
/** content addressed `Id` spanning multiple `Row`s.
  *
  * single `sha` validates single transaction semantics
  */
def rowsSHA: (Row, List[Row]) => Id =
  (row, rows) => rows.foldLeft(rowSHA(row))((s, r) => chain(s)(sha(r)))

/** chained addressed `Id`s spanning multiple `Row`s (single transaction) */
def chainedRowsSHA: Id => (Row, List[Row]) => Id =
  id => (row, rows) => chain(id)(rowsSHA(row, rows))
```

##### content address models a `Set`

Id | Value
:--- | ---:
`a = rowSHA` | 42.00
`b = rowSHA` | 33.33
`a = rowSHA` | 42.00

Consider:

```scala
type Spec     = Value
type Shape[_] = Set[_]
type Model    = Shape[Spec]

val example: Model = Set(33.33, 42.00)
```

Note: duplicate row are elided when `append`ed, and this elision is signaled in the API.

Because the `Id` may be computed directly given the `Value`, or "content", to be referenced, this type of value store can be called `content addressed`.

Note: The sequential order of committed `Value`s is not guaranteed to be maintained. In particular, the physical presence of a `Record` which precedes another cannot be taken as indication, let alone proof, that there is a temporal ordering between them which is defined by the `ValueStore` (such ordering may be defined elsewhere withing a specific data model).

#### Chaining

Or, we can use a previous `Id` (computed from some prior `Row`)
to compute a new (`hash chain`ed) `Id` for a given `Row`.

```scala
/** chain `Id`s together into a sequence using `sha` */
def chain: Id => Id => Id =
 i => j => sha((i, j))

/** chained address for row */
def chainedRowSHA: Id => Row => Id =
  id => row => chain(id)(sha(row))
```

##### chained address models a `List`

Id | Value
:--- | ---:
`a = chainedRowSHA(_)` | 42.00
`b = chainedRowSHA(a)` | 33.33
`c = chainedRowSHA(b)` | 42.00

This is _also_ a pure immutable `value store`, but with `chain`ed `Id`s. No longer content addressable! Duplicate `Row`s will hash to distinct `Id`s due to chaining.
These value stores entail `List` semantics: in particular, the order of commitment may be proved by the `Id` sequence.

Consider:
```scala
type Spec     = Value
type Shape[_] = List[_]
type Model    = Shape[Spec]

val example: Model = List(42.00, 33.33, 42.00)
```

Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

## `Stores`

Instances of this type contain type members for a particular `Store`.

### three level polymorphic

There are three distinct stages to specifying a `Store`:

- specifying the type parameter `V` (what it is we store)
- specifying the (side effecting) IO system, which is the `F[_]` type parameter
- specifying the `Spec`, what it is we `get` and `put` (can differ from `V`)

The first two specifications are given as parameters of a nested type constructor, whose base class is given below:

```scala
/** once again simplified for exposition
  */
trait Stores[V] {

  final type Id    = SHA  // implicit `Order` instance in scope
  final type Value = V    // implicit `Eq` instance in scope

  type Spec               // what we get and put
  type ValueSpec          // what we append
  type Row                // think spreadsheet or rdb

  trait Store[F[_]] {

    final type Record = (Id, Row)

    def has(id: Id): F[Boolean]     // guess again lol

    def rows: Stream[F, Row]        // look ma no `Id`s
  }
}
```

The final binding of abstract types with a given `Store`, in particular the `Spec` type, is given by selecting one of several specializations of subclasses of the parent trait (`Stores`). These are given in a following section.

### `Id` usage in the API

A Store will afford a method which permits interrogation regarding a single `Id`. Note that the interrogator (e.g. an auditor of some kind) need not ever know the specific data which the `Id` references.

By default, `Id`s are returned **only** to the entity which wrote the data. The api does not afford a method to return the complete set of `Id`s, for example.


### constants and "variables"

There are two types of `Store`s: `Value` and `KeyValue`.
- `ValueStore`s persist immutable (in memory) `Spec` values
  - these represent true constants
  - useful for e.g. reference data in the financial domain
- `KeyValueStore`s persist associative arrays of (`Key`, `Spec`) pairs
  - event log of key updates
  - keys can be thought of as (effectful) "variables"


#### `ValueStore`s

All specializations of `ValueStore` may use *either* `content address` or `chained address` functions to compute `Id`.

##### API

```scala
trait ValueStores[V] extends Stores[V] {

  final type Row = V

  trait ValueStore[F[_]] extends Store[F] {
    // value store api
  }
}
```
The `Row` type is simply bound to the  `Value` type.

##### Alegbraic Laws

#### `KeyValueStore`s

All specializations of `KeyValueStore` **must** use `chained address` functions to compute `Id`. This is necessary to maintain ordered list semantics for the store.

##### API

```scala
trait KeyValueStores[K, V] extends Stores[V] {

  final type Key = K  // implicit `Order` instance
  final type Row = (K, Option[V])

  type NelSpec        // folded Nel[Spec]

  trait KeyValueStore[F[_]] extends Store[F] {
    // key value store api
  }
}
```
The `Row` type is bound to a tuple formed from the `Key`, and an `Option[Value]`, where a `None` is used to confer "delete" semantics on the specified `Key`.

##### Alegbraic Laws

### `ValueStore` Specializations

The (awkwardly and ambiguously named) `Spec` type derives from "Specializations". The list of pre-built specializations for `ValueStore`s is extensive:

label | `Spec` | `ValueSpec`
---: | ---: | ---:
`SADT` | `SADT[V]` | `SADT` (polymorphic, like JSON)
`VS` | `V` | `V`
`LV` | `List[W]` | `Option[W]`
`MKV` | `Map[K2, V2]` | `Option[(K2, V2)]`
`MKLV` | `Map[K2, List[V2]]` | `Option[(K2, Option[V2])]`
`NELV` | `NEL[V]` | `V`
`NEMKV` | `NEM[K2, V2]` | `(K2, V2)`
`NEMKLV` | `NEM[K2, List[V2]]` | `(K2, Option[V2])`

Some of these are examined in more detail below, to give a feeling for how the `Spec` types map to persistent representations - particularly the more complex ones.

#### `NEM`: non-empty Maps

Non-empty maps might be used to model a non-empty set of legs within a trade.

```scala
type K2 /*: Order */
type V2 /*: Eq */

type Spec  = NonEmptyMap[K2, V2]
type ValueSpec = (K2, V2)
```

Id            | (K2 |      V2)
:------------ | --- | --------:
`a = rowsSHA` | USD | 10,000.00
`a`           | XAU |    420.33
`b = rowsSHA` | XAU |    397.23

This is a pure value store `(K2, V2)` tuples, which are the rows of a `Map[K2, V2]`.
`Id`s are computed across all `Row`s belonging
to the same `Map`.

```scala
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

#### `NEL`: non-empty Lists

A value store of `NonEmptyList[Value]` rows is used is used in this example to implement a set of lists of arbitrary currencies.

Id |  Value
:--- | ---:
`a = chainedRowSHA(_)`  | XAU
`b = chainedRowsSHA(a)` | XAU
`b`                     | CHF
`c = chainedRowsSHA(b)` | USD
`c`                     | USD



Rows that are committed together get the same `Id`.

```scala
type Spec  = NonEmptyList[V]
type ValueSpec = V

val example: Model =
  Set(
    Nel(XAU),
    Nel(XAU ,
        CHF),
    Nel(USD ,
        USD)
  )
```

####  `MKLV`: Maps of Lists

Note that empty maps, and maps with keys holding empty lists, may be represented with this specialization. This could be useful in certain configuration use cases.

```scala
type K2 /*: Order */
type V2 /*: Eq */

type Spec      = Map[K2, List[V2]]
type ValueSpec = Option[(K2, Option[V2])]
```

May use either content address or chained address for `Id`.
Duplicate rows must be appended within a single call that computes a single `Id`.

Id            | Option[(K2 |      Option[V2])]
:------------ | --- | --------:
`a = rowsSHA` | USD |10,000.00
`a`           | USD | 5,000.00
`a`           | USD | 5,000.00
`a`           | XAU |   420.33
`b = rowsSHA` | XAU |   397.23

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

### `KeyValueStores`

The `KeyValueStore` type represents a family of persistence algebras with parametric effect type, indexed by an ordered key type. This key type is typically bound to a business key type from the domain model.

Note that we are tracing the dynamic evolution of a value indexed by a key, and therefore
multiple instances of the same `Row` entail positional significance in the data model. (Plain english: we can write the same row multiple times, and the order in which we we write all the rows matters.)

#### `KV`: simple key value

The most basic key value store. The `Value` for a given `Key` evolves over time and the store records each evolution, maintaining the entire `Value` history for each `Key`.

In this example we map a currency to an amount.

Id | (Key | Option[Value])
:--- | --- | ---:
`a = chainedRowSHA(_)` | USD | 10,000.00
`b = chainedRowSHA(_)` | XAU | 420.33
`c = chainedRowSHA(b)` | XAU | 397.23
`d = chainedRowSHA(a)` | USD | 5,000.00

```scala
     type Spec     = Value

     val example: Model =
       Map(
         USD -> 5000.00,
         XAU ->  397.23
       )
```

##### "keychained" `Id`s

Note: `Id`s are chained per `Key`, (`keychaining`), which has these implications:

- the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.
- intermediate `Id` state must be maintained per key
   - (`Map[Key, Id]`)
   - memory requirement scales with the number of unique keys!
- chaining arbitrary blocks together is no longer straightforward
   - (entails a `Map[Key, Id] => Map[Key, Id]` somewhere handy)

##### "deleting" a `Key`

`Value`s are `nullable` in the `WithKey` context.

In this example, the value associated with the key `USD` is semantically deleted.

Id | (Key | Option[Value])
:--- | --- | ---:
`z` = chainedRowSHA(`_`) | USD | `null`

<!-- #### KNELV: non-empty list

Used in this example to implement a list of permitted currencies per account.

Id | (Key | Option[Value])
:--- | --- | ---:
`a = chainedRowSHA(_)`  | IBan.09993 | XAU
`b = chainedRowsSHA(_)` | IBan.06776 | XAU
`b`                     | IBan.06776 | CHF
`c = chainedRowSHA(_)`  | UsBan.5321 | USD
`d = chainedRowSHA(b)`  | IBan.06776 | USD

This is a key value store of `Nel[Value]`s.

Recall that `Row`s that are committed together get the same `Id`.

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
``` -->

---

#### MKV: maps of K2 -> V2

In this example, a bank account consisting of an account number, a balance, and a currency are stored.

```scala
type K2 /*: Order */
type V2 /*: Eq */
type Value = (K2, V2)
type Row  = (Key, Option[(K2, Option[V2])])
```

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`a = chainedRowSHA(_)` | UsBan.5321 | USD | 10,000.00
`b = chainedRowSHA(_)` | IBan.09993 | XAU |    420.33
`c = chainedRowSHA(_)` | IBan.06776 | XAU |    397.23
`d = chainedRowSHA(a)` | UsBan.5321 | USD |  5,000.00

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

---

<!-- #### MKNELV

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`a = chainedRowSHA(_)` | UsBan.5321 | USD | 10,000.00
`b = chainedRowSHA(_)` | IBan.09993 | XAU |    420.33
`c = chainedRowSHA(_)` | IBan.06776 | XAU |    397.23
`d = chainedRowSHA(a)` | UsBan.5321 | USD |  5,000.00

This is a key value store of `Map[K2, Nel[V2]]` rows
(the most general structure supported).

Note: same data commits as the previous example, but different semantics.

```scala

  type Shape[_] = Map[Key, _]
  type Spec     = Map[K2, Nel[V2]]

  val example: Model =
    Map(
      IBan.06776 -> Map(XAU -> Nel(  397.23)),
      IBan.09993 -> Map(XAU -> Nel(  420.33)),
      UsBan.5321 -> Map(USD -> Nel(10000.00  ,
                                    5000.00)),
    )
```

`Store`s of this shape are used for e.g. ???
TODO: identify use case, again probably using events as values

--- -->

##### "deleting" a `K2`

For `Map[Map]` and `Map[Map[Nel]]` specializations, `null`ify both the `K2` field for a given `Key`.

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`x` = chainedRowSHA(`d`) | UsBan.5321 | `null` | `null`


For `Map[Map]` and `Map[Map[Nel]]` specializations,
`null`ify the `V2` field for a given `(Key, K2)`.

Id | (Key | Option[(K2 | Option[V2])])
:--- | --- | --- | ---:
`y` = chainedRowSHA(`c`) | IBan.06776 | `XAU` | `null`

---
## What about ORM?

Within a `Spec`, `Id`s can be recursively expanded for implicit joins. `Key`s are trickier but the "publisher" table designation given by Data Vault gives a direction. 
