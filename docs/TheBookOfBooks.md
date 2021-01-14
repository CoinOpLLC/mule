`deftrade`: The Book of Books
---

This document describes `deftrade`, a **foundational toolkit** for composing applications that serve investment funds, asset exchanges, and other **financial market participants**.

Developers will use `deftrade` to build systems that allow traders, market makers, deal makers, portfolio managers, and clients to operate within a _universal and distributed_ financial bookkeeping platform.

Users of these applications will be able to:

- lead their business: code _follows_ business, and code can be made _isomorphic_ to (legal) paper in a bounded, straightforward process
- strike deals of arbitrary complexity and have the "back office" keep up automatically
- compose new contracts and entities by combining standard contracts via a lawful algebra

The toolkit lays a principled and disciplined foundation that applications can leverage for trust and security.

In the spirit of **Domain Driven Design**, this document is meant to define a _ubiquitous vocabulary_ for developers and domain experts who use `deftrade` to get better domain models faster.

**Features:**
- Universal platform for all assets, real and virtual
  - Smart contracts for automated performance
  - "Dummy" contracts for legacy real world assets
- composable `Contract` DSL and evaluation engines
  - public `Instrument`s embed standard `Contract`s
    - for e.g. publicly traded securities or a standard convertible note
  - private `Instrument`s embed custom `Contract`s
  - `Form`s record the contract _state_ and its evolution over time
  - `DSL`s for commercial time and money calculations
- flexible `Ledger` data model
  - chained cryptographic hashes as primary key types
- `Account` management with privacy-first data model to protect PII
  - account and tax `Id`s are excluded from core `Ledger` data model
- core `Accounting` balances and reporting
- market data system (`MDS`) integration hooks for real time tick stream providers
- order management system (`OMS`) integration hooks for external markets


Nick Wade
CoinOp LLC
___
<p style="page-break-before: always">

[[toc]]

## Elevator FAQ

**What is `deftrade`?**
: A toolkit that provides the types and methods necessary to specify financial **domain models** that are **abstract** and algebraic. We say`abstract` because models are generic across all important types, including effect types. Models are `algebraic` because they compose formally within an algebraic system, including laws which the algebra respects.

**I hated algebra - why should I like algebraic domain models?**
:  The algebraically composed `domain model` is intended to be **legible** to - and thus **directly reviewable by** - the financial market participants (domain experts) who use the applications. Removing an entire level-of-human-indirection makes the application development path both more agile and more robust.

**What kind of financial domain models?**
: - Distributed Asset Exchanges (i.e. an OTC marketplace)
  - Algorithmic Trading Systems (ATS)
  - many others (can't list them all, that's the point)

**Who are you calling a "financial market participant"?**
: - _Initial scope_: small private equity funds, small hedge funds, family offices, RIAs,  loan funds, real estate funds, ad-hoc seed venture funds, etc.
  - _Potential scope_: banks, credit unions, CDFIs, broker/dealers, OTC market makers, crypto exchanges, and other actors with additional operational and regulatory requirements.

**Why should I trust my application to run on `deftrade`?**
: - correctness of the smart contracts underwrites trust in the business process
    - [Ricardian smart contracts](https://en.wikipedia.org/wiki/Ricardian_contract) composed via algebraic composition with parallel legal prose.
    - smart contracts are "just code", so all the same correctness and validation techniques apply
    - contract properties like "deterministic" and "terminating" _provable_ within the type system
  - domain level algebraic laws
    - capture higher level behavior
    - automated test generation to verify
  - soundly typed FP simplifies refactoring for maintenance and features
    - type safety obviates (some) low level unit testing
    - codec functions automatically _derived_ (not "generated")
  - cryptographic data model
    - pervasive use of secure hashes as `Id`
    - `SASH` table records signatures for any `Id`
    - pay-as-you-go overhead flexibility for defining protocols

**What about the Name?**
: Buried within the order pipeline is the following code:
  ```scala
  /** This phase is implemented by the brokerage api integration code. */
  def trade: Order ToStreamOf Execution = ...
  ```

Note that `deftrade` is _not_ a complete application. In particular, while the ledger is designed to be distributed, the distributed coherence mechanism is not specified. This is in keeping with the practice of some other systems such as `Corda`. Different systems with different trust/threat models will make different choices.

Manual reconciliation provides an _ultima ratio_ level of flexibility and capacity for integration. Custom smart contracts coexist / inter-operate with industry standard contracts (e.g. ICE Futures) which are modeled with "dummy" contracts, with parameters that are walked through their "real world" paths via manual reconciliation, if need be.

As a simple example, consider a publicly traded common stock which issues a dividend. Having no control or pre-knowledge of the dividend (until announced), the reconciliation of the external state and the internal (`Ledger`) state must be effected manually.

<!-- research test bed for Ricardian smart contracts -->
<!-- `deftrade` is `corda` for hobbit capital in the real economy -->

## What does it mean to "keep a set of books?"

Organizations of all kinds and in all ages have "kept books", and the literature on history and practice is vast.

In the last decade, the confluence of techniques which constitue "crypto-currency" - secure hashes, merkle trees, PKI, and distributed consensus - have given rise to a strange new respect for the role of bookkeeping in regards to the ontology of money itself.

In fact, the essential properties of money may be [viewed as deriving from the institutional memory of the ledgers that account for it](https://mariolaul.medium.com/the-bookkeeping-view-of-money-and-crypto-economic-analysis-f8a9a3c23de9):
>According to this [bookkeeping] view, the monetary system is a collective method of enabling and keeping track of economic activity and relations using a socially agreed upon measuring unit. In other words, **a bookkeeping system** that allows individuals, organizations and society at large to value assets and activities, mobilize real resources, clear and settle transactions, carry into the present their financial history, and project into the future their economic plans and interdependencies. As such, it represents a form of **communal, institutionalized memory**, regardless of whether its rules are officially mandated or based on a set of informal norms and practices.

### Foundational Requirements

Books should be like ink on parchment: indelible, immutable, and, when signed, irrefutable.

Implementing our solution here in the twenty first century, we need the following basic services
- storage with [ACID](https://en.wikipedia.org/wiki/ACID) properties
  - devil in the detail: multi-party Consistency
- [Public Key]() [signature nonrepudiation]()
  - devil in the detail: PKI

Ultimately the data storage foundation must provide **attestible immutability**
  - like **git** has (with PGP signed commits)
  - `deftrade` hash trees can populate a "Merkle Forest" in `IPFS`

### On-ledger counterparties vs external counterparties

The canonical use case is of an island of on-ledger parties, who are potentially each other's counterparties, and who also face an assortment of off-ledger (external) counterparties (such as external markets), accessed via brokerage account API integrations.

Different parties have different roles with respect to bookkeeping, and that includes parties representing regulatory authorities such as auditors.

TODO: explore "Smart regulatory contract" as a platform token
TODO: need [Corda](https://www.r3.com/wp-content/uploads/2019/06/corda-platform-whitepaper.pdf) diagram from R3cev here. Relevant usage model to reference.

> Figure 3: Consensus over transaction validity is performed only by parties to the
transaction in question. Therefore, data is only shared with those parties which
are required to see it. Other platforms generally reach consensus at the ledger
level. Thus, any given actor in a Corda system sees only a subset of the overall
data managed by the system as a whole. We allow arbitrary combinations of
actors to participate in the consensus process for any given piece of data.

## Contracts, their ancillary types, and instance lifecycle

"Smart contracts" are not a "map of the terrain". *They are the terrain*, in the sense that they directly embody contract _performance_.

But in the real world, where incomplete integration modulates the ability to automate, "dummy contracts" may be used to inter-operate with externally defined and pre-existing contracts, such as publicly traded equity securities ("stocks") and rated debt securities ("bonds").

The `deftrade` design was inspired by [Composing Contracts]() and other works listed in the [references](#References).

The `deftrade` platform builds on these techniques to implement the hybrid `Engine` architecture, which comprehends automation and manual workflow, in addition to model-based pricing.

### Contracts and the `contract` package

Within the `deftrade` toolkit, the `Contract` type unifies all modes of performance and analysis:
- captures the **operational semantics** of contract performance in an embedded domain specific language (DSL) that can be reviewed by the (non-programmer!) `Party`s to the `Contract`.
- guides workflow plan generation for manual contract execution via [Engine.Scheduling](./modules/core/target/api/io/deftrade/contracts/Engine$$Scheduling.html)
- supports smart contract execution via [Engine.Performing](./modules/core/target/api/io/deftrade/contracts/Engine$$Performing.html)
- provides the reference for all modes of analysis
   - [Engine.Pricing](./modules/core/target/api/io/deftrade/contracts/Engine$$Performing.html) begins with discrete time models
   - monte-carlo methods for path-dependent `Contract`s (later)
   - others tbd

The smart contracts that operate within the `deftrade` platform are instances of the `Contract` type.

`Contract` represents a recursive, lazily evaluated Algebraic Data Types (ADTs).

`Contract` _instances_ are specified using a domain specific language (DSL) embedded within scala, which enumerate a small number of primitives.

`Contract` _instances_ form an algebraic `Group`:
- the universe of contracts is **closed** under algebraic combination: two contacts combined are also a contract.
- there is an element which represents **identity** (the `zero: Contract`)
- there is an **inverse** for each element such that when an element is combined with its inverse, the result is unity.

The composite `Contract` governing an `Trade` - the quantities of instruments proposed to be exchanged in a Transaction - is precisely that algebraic combination of Contracts specified by the `Instrument`s within the `Trade`. More on `Instrument`s and `Trade`s later, but for now consider that they mean what they sound like they mean.

- for each side within a `Trade`, at least one `Contract` must be referenced, so as to be identifiable as "consideration".
- just like in the real world, there's always this:
  - ''Good and valuable consideration, the receipt and sufficiency of which are hearby acknowledged'', which looks like this in code:
    ```scala
    object GavcTrasowaHa extends Contract // == zero, of course
    ```

### Numéraire: the units of account

`Numéraire` is formal finance term which, contrary to what an understandably naive anglophone might think, signifies the **denominator** (not numerator) for `Contract`s and `transaction`s.

There are exactly two ways we can "settle the bill", so to speak: `InCoin`, and `InKind`.

The application level is where we declare the policy decisions about details associated with both modes.

#### InCoin: Money and Currency

`Money` has the following properties of interest in this model:
 - Money is an amount denominated in a (supported) **`Currency`**
 - Money is **fungable**
 - Money is **self-pricing** (within its denomination)

 Most kinds of debt are simply `Contract`s for future `Money`.

#### InKind: other things we can receive in consideration

For example:
 - `Contract`s for **equity** (e.g shares of common stock)
 - `Contract`s for **real property** (e.g. commercial property leases)
 - `Contract`s for **physical delivery of commodities** (e.g. tanks of propane)

### Contract DSL

What follows is a brief description of the `DSL` for contract specification, representation and evaluation, capturing the operational semantics of contract performance.

#### Primitives

```scala
   /** Party immediately acquires one unit of `Numéraire` from counterparty. */
   def unitOf(base: Numéraire): Contract

   /** Party acquires `c` multiplied by `n`. */
   def scale(n: Observable[Double])(c: => Contract): Contract

   /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`. */
   def branch(b: Observable[Boolean])(cT: => Contract)(cF: => Contract): Contract

   /** Party assumes role of counterparty with respect to `c`. */
   def give(c: => Contract): Contract = new Give(later(c)) {}

   /** Party will acquire c as soon as `b` is observed `true`.  */
   def when(b: Observable[Boolean])(c: => Contract): Contract

   /** Party acquires `c` with the obligation to abandon it when `o` is observed `true`. */
   def until(b: Observable[Boolean])(c: => Contract): Contract

   /** Once you acquire anytime obs c, you may acquire c at any time the observable obs is true. */
   def anytime(b: Observable[Boolean])(c: => Contract): Contract

   /** Party immediately receives both `c1` and `c2`. */
   def both(c1: => Contract, c2: => Contract): Contract

   /** Party freely and immediately chooses between `c1` or `c2`. */
   def pick(c1: => Contract, c2: => Contract): Contract
```

#### Observables and oracles

TODO: investigate potential for DeFi integration (which seems high)

### Engines

Use an `Engine` to `eval`uate a `Contract`s and obtain a result. That result may be **effectful**, but is not required to be.

The difference between "workflow automation" and "smart contract execution" is a matter of degree, perspective, and counterparty platform integration.

There are currently three kinds of `Engine`s:

#### Pricing

Evaluate the `Contract` in the specified `Currency`, returning a **real valued process** which represents the distribution of possible `Contract` values a given number of `TimeSteps` from `t0`.

#### Scheduling

Produces calendar schedules of actions for humans to follow up on:
 - make and expect payments, exercise options, etc.
 - uses automation tools as integrations become available

#### Performing

Automated `Contract` performance, including the capacity to commit Transactions to the Ledger.

### Evolution of the Instrument graph

Our investible universe is comprised of a graph of `Instrument`s issued by various entities.

Note in particular that the graph of the legal entities who issue the `Instrument`s is not recorded per se, only the graph of the `Instrument`s issued by those legal entities.

#### Instruments

Models a tradeable thing. Each `Instrument` embeds a `Contract`.

TODO:
 - specify a unified symbology behind `symbol`
 - factories for custom `Instruments` (e.g. `SAFEnote`)
 - `FpML` ingestion


#### Forms

Instances of `Form` memorialize the state of a `Contract` at a given point in time. (E.g.: the principle and interest balances in a term loan amortization schedule.) Therefore, a `Form` instance, associated with a given `Instrument`, will evolve over time. The key value store captures that evolution.

**Q:**  How many evolutions per Instrument key is it reasonable to expect?

**A:**  Dozens (e.g. monthly) not billions (e.g. every millisecond).

As an extreme example, the "trace" of bespoke structured product, quoted every
fifteen minutes for ten years, would run to about sixty thosand entries. This might be fine.

Other examples:
- interest or dividend payment
- optionality exercise
- price fixings

#### Novations

Links which model `Instrument` lifecycle transformation acts (such as M&A actions) as events connecting `Instruments.Key`s.

Motivation: an (immutable) `Instrument` can hide an extensive history.

Suppose that in 1990, you had 3 separate investments in DEC, COMPAQ, and HP stock...
and then this happens:
```
       HP ->  HPQ
               ^
               ^
DEC -> COMPAQ -+
```
you end up with one investment in HPQ!
- It is generally necessary to capture the evolution of these graphs in time.
- `Novation`s are the directed links that connect `Instrument`s in that graph.

`Novation`s can represent `Contract`:
- Issuance
- Assignment
- Termination

A `Novation.Id` makes an effective M&A "receipt".
There can be more than one leg in an M&A transaction:
- store a `List[Novation]` (or ideally a `NonEmptySet`)
- all `List` elements (legs) will get the same `Id`.
- thus the "receipt" is common to all the legs that make up the transaction.

TODO: `Provenance` factorization

### References

- Original papers at [Microsoft Resarch](https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/)
- [LexiFi](https://www.lexifi.com/apropos/)
  - sells a mature implementation of this technology in OCaml
  - founded by J.M. Eber, who co-authored above papers.)
- Anton van Straaten's [Haskell implementation](http://web.archive.org/web/20130814194431/http://contracts.scheming.org/)
  - highly referenced; no longer maintained
  - see in particular his section ''A note about the original papers''
- Channing Walton's [scala implementation](https://github.com/channingwalton/scala-contracts) (also not actively maintained)
- [Netrium](http://netrium.org/)'s open source [Haskell implementation](http://hackage.haskell.org/package/netrium) (status?)
- Financial DSLs [resource page](http://www.dslfin.org/resources.html)

## The Tables

This section specifies the data model.

### The `keyval` package.

The business data object lifecycle procedes inexorably:

`In Memory ==> ... ==> On Ice`

Every persistence in between can be viewed as a _cache_ of some kind.

This exposition will cover the two endpoints (where `On Ice` is taken to be `.csv`).

#### Immutable, append-only data model

Tracing the evolution of the value associated with a key, by folding the values associated with that key to provide a "sum" of some kind, is the technique by which "state variables" are created and maintained within memory.

Recording only the intent to delete identified data, and not the actual data, is usually a good practice. The exception would be where data _must_ be erased (e.g. for regulatory reasons). It will be necesssary to make a snapshot of the folded-up structure in a table or file which **completely replaces** the previous data in order to physically delete data. This will become necessary over time if bounded storage requirements are respected.

#### Indexing Types

`deftrade` defines a [convention over configuration](https://en.wikipedia.org/wiki/Convention_over_configuration) system for `Id`s and `Key`s.

##### Id

The generation of unique primary key `Id` instances has been left to the database itself in past architectures. Our approach is to compute a cryptographic `Id` over the set of persisted `Row`s representing an atomic append operation. These sets of rows may be independent (and therefore content addressable), or `chained` together such that the `Id` of each atomic `Row` set is dependent on the `Id` computed for each previous append.

`Order` and `Show` typeclass instances are defined naturally for `Id`.

The specific secure hash algorithm is bound by the application developers.

##### Key
- identifier types
- domain legible (e.g. `AccountNo`)
- opaque (e.g. `UUID`s)
- `Order`, and `Show` typeclass instances

##### Value
  - often a value class
  - typeclass instances for `Eq`, and `Show`.
  - `Value` is not the same as `Spec` (more later)

#### Primary Index Conventions

In contrast to some other programming conventions, a `type Foo` may not contain instances of, nor depend upon, type `Foo.Id` or `Foo.Key`.

Stated simply: there will be no `id: Id` or `key: Key` fields within domain types! Index types are stored separately at every level of the storage hierarchy (e.g. `key`s in an in-memory `scala.collection.Map`)

However, **foreign keys** which reference other domain value types are permitted within value types.

#### Data Vault Modeling

It is assumed that `keyval` package clients will generally pursue [Data Vault](https://en.wikipedia.org/wiki/Data_vault_modeling) style modeling and use `hub`s and `link`s to define graphs of `value types` defined by `business key`s.

A `link` in "Data Vault" terms (foreign key) must be either be a business key or an `Id` (the identifier of an immutable record) from some `Table`.

`Business Keys`
: - integral or string types with domain specific formats and semantics
  - possibly opaque (like a `UUID`), possibly public (like an `ISIN`)
  - may index `KeyValue` stores directly
  - _via negativa_, "**Real** business keys only change when the **business** changes!" (cite: lore).

`Essential Attributes`
: - ubiquitous
  - canonical (or projectable from a canonical form, e.g. `CUSIP` projected from `ISIN`)
  - permit **tactical denormalization**
    - deviates from strict Data Vault methodology
    - mixes `satelite` fields in with `link` or `hub` shaped relations
    - no redundancy from denormalizing due to ubiquity

`Optional Attributes`
: - always `link`ed, never denormalized.
  - single, polymorphic fields recorded as Serialized Algebraic Data Types (`SADT`s)
  - persisted / indexed as binary in Mongo and Postgres
  - simple `JSON` to begin
    - [IPLD compliant](https://specs.ipld.io/#ipld-codecs) (barely)
    - TODO: `DAG-JSON`, `DAG-CBOR`

### Id computation via secure hash

A cryptographic hash function (secure hash algorithm: `sha`) is the basis of index computation.

**Note**: the following `scala` code is _simplified_ for exposition.

```scala
/** assume for example - other choices would work here as well */
   type Sha = String Refined IsSha256AsBase58

/** magical code! elides many details about canonical formats and codecs! */
   def sha[A](a: A): Sha = { ... }
```
Assume a suitable type `Row` that represents the bits to be persisted in association with a given `Id`. (`Row` will be elaborated in the following section.)

We can use `sha` to compute an `Id` based only on instances of `Row`:

```scala

/** content address for row */
  def rowSha: Row => Id =
    row => sha(row)
```
Or, we can use a previous `Id` (computed from some prior `Row`)
to compute a new (`hash chain`ed) `Id` for a given `Row`.

```scala

       /** chain `Id`s together into a sequence using `sha` */
       def chain: Id => Id => Id =
         i => j => sha((i, j))

         /** chained address for row */
       def chainedRowSha: Id => Row => Id =
         id => row => chain(id)(sha(row))
```

**Note:** An `Id` calculated via `sha` can span multiple `Rows`.

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

### Stores

There are two types of `Store`s: `Value` and `KeyValue`.
- Value stores persist immutable (in memory) values
- KeyValue stores persist associative arrays of (key, value) pairs
- Both use the root WithValue type to augment companion objects of `type V`
  - define (specific) `Id` and (generic) `Record` types for _both_ kinds of `Store`.

#### `Store`s and companion root trait `WithValue`

```scala
/** once again simplified for exposition
  */
trait WithValue[V] {

 // bound type members
 //
 type Id    = Sha  /*: Order */
 type Value = V    /*: Eq */

 // free type members
 //
 type Row   // what we append, prefixed with Id
 type Spec  // what we get and put

 // derived type members
 //
 type Record = (Id, Row)
}
```
Type swag provider for the free type variable `Value`.

Any type `A` we bind to `Value` will have these provided. `Value` types must have value equality semantics - of course!

### `ValueStore`s and Companion mixin `WithId`
```scala
     trait WithId[V] extends WithValue[V] {
       type Row = Value
     }
```
The `Row` type is simply bound to the  `Value` type.

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
type Spec     = Value
type Shape[_] = Set[_]
type Model    = Shape[Spec]

val example: Model = Set(33.33, 42.00)
```

Note: duplicate rows are elided, and this elision is signaled in the API.

Because the `Id` may be computed directly given the `Value`, or "content", to be referenced, this type of `store` can be called `content addressed`.

Note: The sequential order of committed `Value`s is not guaranteed to be maintained. In particular, the physical presence of a `Record` which precedes another cannot be taken as indication, let alone proof, that there is a temporal ordering between them which is defined by the `ValueStore` (such ordering may be defined elsewhere withing a specific data model).

#### chained address models a `List`

Id | Value
:--- | ---:
`a = chainedRowSha(_)` | 42.00
`b = chainedRowSha(a)` | 33.33
`c = chainedRowSha(b)` | 42.00

This is _also_ a pure immutable `value store`, but with `chain`ed `Id`s. No longer content addressable! Duplicate `Row`s will hash to distinct `Id`s due to chaining.
Entails `List` semantics: the order of commitment may be proved by the `Id` sequence.


```scala
type Spec     = Value
/* type Shape[_] = Set[_] */
type Shape[_] = List[_]
type Model    = Shape[Spec]

val example: Model = List(42.00, 33.33, 42.00)
```

Note in particular that a _block_ of chained `Id`s forms a
function `Id => Id` (input to output) permitting blocks themselves to be chained (!).

`Store`s of this shape are used for `model.Transaction`s.

All `Shape` specializations of `WithId` may use *either* `content address` or `chained address` functions to compute `Id`.

#### specialization for `Nem`s (Non Empty Maps)

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
type Spec     = NonEmptyMap[K2, V2]

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

`Store`s of this shape are used e.g. to model `Trade`s (with `content address`ing).

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

`Store`s with this specialization are used in this example to implement a set of lists of arbitrary currencies.

**TODO:** make this example a set of `Command` or `Event` values of some kind - that shows off the `Shape`.

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
### KeyValueStores

The `KeyValueStore` type represents a family of persistence algebras with parametric effect type, indexed by an ordered key type. This key type is typically bound to a business key type from the domain model.



#### Companion mixin `WithKey`

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

This is a key value store. The `Value` for a given `Key` evolves over time and the store records each evolution, maintaining the entire `Value` history for each `Key`.

Note: `Id`s are chained per `Key`, (`keychaining`), which has these implications:

- the evolution of the value for a single key can be extracted from the store and
transferred or validated without additional context.
- intermediate `Id` state must be maintained per key
   - (`Map[Key, Id]`)
   - scales with the number of unique keys!
- chaining arbitrary blocks together is no longer straightforward
   - (entails a `Map[Key, Id] => Map[Key, Id]` somewhere handy)

```scala
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

`Store`s of this shape are used in this example to implement a list of permitted currencies per account.

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
      IBan.06776 -> Map(XAU -> Nel(  397.23)),
      IBan.09993 -> Map(XAU -> Nel(  420.33)),
      UsBan.5321 -> Map(USD -> Nel(10000.00  ,
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

## The Table of Tables

This sections describes how the data models are layered and relate to each other.

### The `model` package.

The `model` package implements records and computations which define a layered set of financial domain models and services.

The simplified model package illustrated below serves up the full stack of `layers`.

But note: a `model` package object analog customized for certain applications need **not** incorporate all layers! In particular, the `Accounts` layer isn't necessary where the personal information of the market participants isn't necessary for the performance of business logic.
```scala
package object model extends /* */ {
    //
    with Ledger          // folios, transactions, confirmations
    //
    with Accounting      // debits, credits, and all their descendants
    with Balances        // accounting sums across transactions
    with MarketData      // IBKR will be first integration
    with OrderManagement // IBKR will be first integration
    //
    // `Accounts` layer can be omited for applications not requiring information
    // about any `Party` to the `Position`s held.
    //
    with Accounts // binding of parties to sets of positions
    //
      /* ... */
    }
```

The `model` package object is also where we bind certain application policy decisions.

This is where the policy decision to choose generic tax accounting for entities treated as partnerships. Different objects instantiating the layers could make different policy decisions.

Also, this is where we bind the `MonetaryAmount` type (and thus `Money`) to `BigDecimal`, while other `Financial` `Quantity`s bind to `Double`. Again, other instanting objects could make other decisions; see the API docs (and the code!) for more details.

#### `Partition`s

Models anything from the division of a pizza to the equity capital of a firm per partner.

#### Ledger as the record of market events.

The key insight is that `Map`s are `Group`s if their values are. Maps are used to model both trades and portfolios of instruments, and so portfolios are naturally seen as sums of trades.

`Position`
: a quantity of a given `Instrument`

`Folio`s
: sets of `Position`s which can be (and indeed are created) empty

`Trade`s
: Non empty sets of `Leg`s

##### Transactions

`Transactions`s record agreement between parties to strike a deal
- are immutable once recorded

`Transaction`s are always created within a parameterized context defining a range of side effects, with persistence being the most important.

##### Confirmations

`Confirmation`s acknowledge the delivery of the assets specified by executed transactions
- recorded by recipients
- a single execution can be broken into multiple delivery confirmations
- are immutable once recorded
`Fiber`s are used for `Confirmation` flows.

How lightweight are `Fiber`s? So light we can create a pair for every outstanding (unconfirmed) `Execution`.

This means that "failed" transactions will result in "resource leaks" - except that the `Fiber` is so lightweight that it's just another heap allocated object, which must remain materialized (or persisted) to memorialize the failed transaction.

#### Cash and payment
So called "ready funds" such as bank account balances are a kind of `capital.Instrument`.

- we know **our** cash `instrument` details (e.g. `USD` bank accounts).
   - must track quantity (`balance`) per such `instrument` of ours
   - don't want to have to need to know `instrument` details of the other party
- This isomorphism relates `Money` and `Position`:
  ```scala
  //                 ____reify ___
  //                /             \
  //               /               V
  Money[C: Currency]     <==>      (Instruments.Key, Quantity)
  //               ^                /
  //                \____abstract__/
  ```

By platform convention, `instrument.symbol === currency.code` for **all** `cash instrument`s.

For `Position`s within `Folio`s:
- the `Instruments.Key` is a `String Refined capital.keys.IsUsBan`
- a `Folio` can contain a number of such `instrument`s and associated quantities: identified bank balances, which can be thought of as "bank account positions".

For `Leg`s within `Trade`s:
- the`Instruments.Key`s is constucted by taking the [three letter currency code](money.CurrencyLike.code) **_only_** as the **exact `Instruments.Key`**
- the `Transaction` record does **not** record (e.g. bank account) details but instead deal in cash `Instrument`s that are both **reified** and stripped of detail.

#### Processing Transactions

The `Transaction` is the concrete record for `Ledger` updates. Do we mean `Transaction` in the *business* sense, or the *computer science* sense? **Yes**: both parties must agree upon the result, under all semantics for the term.

The exact semantics will depend on the higher level context: a `Transaction` memorializing a booked `Trade` pair will spawn `Confirmation`s as that `Transaction` is settled.

Important design note: _there is no currency field_; cash payments are reified in currency-as-instrument.

What about metadata? the `meta` field, an `SADT` extension point, stores the **cryptographic hash** of whatever metadata there is, preserving uniform treatment and constant size.

Transaction settlment is easily `audit`ed, as a `Confirmation` trail links all the relevent `Folio.Id`s, where `Folio.Id` represent a **persistent update event** for the specified `Folios.Key`!

Cryptographic note: the `Transaction.Id` is chained into the `Confirmation.Id`s that go into the settlement computation.

The net effect of settling a Transaction must be to transfer the specified `Trade`s between `Folio`s.

We accomplish this by employing two _concurrent_ `Fiber`s per `Transaction`:

- side `A`
  - `expect.n = b.trade   // by convention, the thing being traded`
  - `escrow.n = a.trade   // the payment, typically`
  - workflow option one:
    - "manual" payment transfer
    - `fiber.wait(escrow.n === empty) // reactive`
  - workflow option two
    - assumes programmatic transfer capability for assets! (USD typically)
    - `open += escrow.n               // proactive`
    - `escrow.n = empty`
- side `B`:
    - same, _mutatis mutandis_ (`a` <==> `b`)
    - workflows which differ from those of side `A` are OK
- join on both fibers
 - *now* `trade.n` can be settled
 - both sides respectively:
   - `open += expect.n`
   - `expect.n = empty`

*Note*: no `Folio.Key`s need be exchanged at all, because `join` does all the work. Both scratch `Folio`s, `expect` and `escrow`, are left empty in a successful settlement, and are therefore eliminated on snapshot (`key`s which reference empty `Folio`s are dropped).

### Markets

This section describes how `Transaction`s are agreed upon by the parties to them, prior to those `Transaction` recording in the `Ledger`.

It should be understood that `Market`s cannot be defined without reference to real world agents, but we'll ignore that aspect for now and continue to add detail arount the processing of `Transaction`s.

`Internal` markets can be defined where
- all parties are "on Ledger"
- internal transactions save on external transaction costs
- self clearing / self settling
- possible to implement netting

`External` market integration topics include
- API integrations
- `MIC` code identities (reference data)
- Virtual markets (procedural order routers)

#### Market Data Systems

Various kinds of data about and from markets inhabit a modern financial markets office, and it is domain convention to refer to the different kinds of data by these names:

Market Data
: - bid/ask/trade ticks for live markets
  - can be very legit high freq (c.g. [OPRA](https://www.opraplan.com/))
  - but also low freq like announced dividend amounts, dates, and ex-dividend dates
  - Wall St Journal prime rate - low freq, very important, classic oracle problem

Historical Data
: - old market data, where `old` effectively means "useless for live pricing models"
  - used for back-testing
  - can be aggregated (e.g. ticks -> bars)

Reference Data
: - Think of this as market metadata
  - Market ID codes, hours, holiday lists,

Market data and Historical data are both used within various `Pricing` modules (described in a subsequent section).

#### Order Management

Ubiquitous within the domain is the acronym,`OMS`, which stands for **Order Management System**. Financial markets, especially publicly traded markets, are often grouped and accessed together over a session based API which manages the order lifecycle.

Reference for the order/execution pipeline: [Functional and Reactive Domain Modeling, section 4.4](https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270)

##### Orders

`Order`s propose transactions to `Market`s for execution
- within time and price limits
- ... or not! "market orders" request unconditional execution
  - (common, useful, dangerous)
- may be modified or canceled
  - identified by a session-lived `OrderNo`
  - so we model as _entities_ which can evolve over time
  - this means `KeyValue` store
- `SADT` extension point for arbitrary attributes organized as a client specified `ADT`
  - "limit" concept extendible to other parametric bounds (interest rate, volatility)

##### Executions

An `Execution` records a link between a `Transaction` and the `Order` which requested it
- a single order may be broken into multiple executions
- are immutable once recorded

What it is that the client wants `Execution` of. Note: a denominating currency is always required by the `MarketData.Exchange`s, in order to fully specify the trade, even if there is no limit amount attached.

Note that the first section of tables denotes the complete record of the financial transactions and can be parsed and analyzed without reference to the entities who did the transacting. The second section of tables denotes who did the transacting, and how it was accomplished.

Settlement updates the actual `Folio`s, with `Account` specific (and this `Folio` specific) cash account `capital.Instrument`s substituted for the raw `money.Currency` pseudo `Instrument` specified in the `Order` and enumerated within the `Leg`s of the `Trade` specified in the `Transaction`.

Each OMS must maintain a contra `Ledger.Folios.Key`. The creation of this account (and its `Ledger.Folios.Key`) must occur before the OMS is created.

TODO: revisit parent/child orders

---
<p style="page-break-before: always">

### Balances and Accounting

Double entry `Balance` calculation from a `fs2.Stream` of `Ledger.Transaction`s. When summing Transactions, this layer implements the algebra which maintains all the accounting definitions, identities, and computed sums.

These are the accounting terms and identities as we use them:

```
   Debits := Assets + Expenses                  // accounting definition
   Credits := Liability + Revenue               // accounting definition
   Debits === Credits                           // accounting identity
   Assets === Liabilities                       // balance sheet identity
   Assets + Expenses === Liabilities + Revenue  // substituting
   Income := Revenue net Expenses               // the "bottom line"
   Liabilities := Debt + Equity                 // always one or the other
   RetainedEarnings = Income net Distributions  // business keeps what partners don't
   Equity :=                                    // total value of partners' stakes
     ShareCapital                               // total raised across all rounds
   + Reserves                                   // you never know
   + RetainedEarnings                           // add to book value of equity
```

TODO: express these as _algebraic laws_.
#### Pricing

`Pricing` instances represent a price quote (in currency `C`) for instruments of type `A`. The two parameter type constructor takes advantage of the infix syntax; `A QuotedIn B` is a human-legible expression in the domain of market quotes.

Return both a `bid` and an `ask` for the given instrument.

**Important: requesting a `quote` _does not signal intent_**. For example, the client may ignore the returned ask and just hit the bid (if selling). Servers of this api (e.g. stockbrokers) cannot not predjudice their responses when asked for a quote, as the client reveals nothing about their intent.

Pricing instances can deliver live market feeds
- "Orderly market" invariant: `bid` < `ask`
- must accept _disorderly_ markets: not everything that comes at you down the wire can be expected to "make sense"

In addition to live markets, prices may orignate from
- `Model`s, which report a *fair value* modeled price
   - may depend on `market data` for calibration
   - therefore limited by accuracy (in practice)
- `Book`: the price we paid for the asset
   - only covers the assets we own(ed).
   - captured from our `Transaction` stream.

#### Trial Balance

There are exactly two transformations of a `TrialBalance` and an `amount: Money[C]` which result in another legal `TrialBalance`:
 - grow (shrink) balance by amount
 - constant balance; swap amount between keys within debits (credits)

These are broken out into separate methods.

TODO: how to guarantee this can be done in the general case
- extract the price from the Transaction
- create a `DoubleEntryKey` for it (depends on price - think about waterfall impl)
- create a TrialBalance from the price and de keys
- fold that TrialBalance into the running sum


#### Income Statement

#### Balance Sheet

"Balance" is a property of the type; `BalanceSheet`s cannot be out of balance.

#### Cash Flow Statement

Typically broken into three sections:
- Operations
- Investment
- Financing

#### Equity

`EquityStatement` should really be called [Statement of Changes in Equity](https://en.wikipedia.org/wiki/Statement_of_changes_in_equity)

Note the (arithmetic) indifference of the equity holder to equity
transactions at market price: their book value is unaffected by such
transactions.

Change in Equity value: `Credits net Debits`, as usual for a `Balance`
- Debits
   - Dividends paid (per share)
   - Share buy-backs (premium over book value per share)
- Credits
   - Shares issued (premium over book value per share)
   - Comprehensive Income (per share)

---
### Accounts

`Account`s model the relation of `Party`s to `Folio`s, and define `Role`s that `Party`s to an account may take.

`Accounts` are long lived entities that can evolve over time. Each is created with a `Roster`, specifying the `Principal` owners and their `NonPrincipal` crew: their `Agent`, their `Manager`s, and the `Auditor`s.

The set of accounts is keyed by an `Account.No`, a refined long integer defining a very conventional looking account numbering scheme. It is intended to be transcribed by human hand if necessary.

`Account`s comprise:
- a `Folio` of settled `Ledger.Transaction`s
- a `Folio` of `Transaction`s not yet settled
- a `Roster` - specifies a mapping of `Party`s to `Role`s, and who are the beneficial owners
    - linked to the `Account` via its own table, indexed by the `Account.Key`

#### Parties

A `Party` - either a `NaturalPerson` or a `LegalEntity` - is presumed to be a real world actor under the aegis of, and posessed of all required registations with, a real world juristiction.

Accounts link the personal information of the account holders with the financial data of the ledgers, so handle with care!

One simple but crucial design feature is that `Tax.No`'s are not used as `Key`s to identify `Party`s.

#### Roles

Each `Party` within an `Account`s `Roster` is assigned one of a finite enumeration of `Role`s. . Every `Role` is mapped to a `Party` via a `Roster` which is **application and jurisdiction specific**.

While least one `Party` **must** be specified for each role in the roster, the default is to assign the principal to perform each of the non principal roles for themselves.


#### Principals and NonPrincipals, enumerated

`Principal`
: That `Party` which is the market participant responsible for establishing the `Account`. Semantics for `Principal` are conditioned on the status of account, for example
  - beneficial owner for an asset
  - responsible party for a liability
  - shareholder for equity
  - business unit chief for revenue and expenses

`Agent`
: The primary delegate selected by the `Principal`s. A `Princple` is their own `Agent` unless otherwise specified.

`Manager`
: The primary delegate selected by the `Agent`. `Party`(s) with responsibility for, and authority over,
the disposition of assets in the `Account`. In particular, `Manager`s may initiate actions which will result in `Transaction`s settling to the `Account`.

`Auditor`
: First class participants, with a package of rights and responsibilities. `Auditor`s will have a (possibly limited) view into the state of the `Ledger`, and (optionally) the ability to block the **settlement** of a `Transaction` (but not its **record**) to the `Ledger` (i.e. "break trades"), or even to initiate the recording of a `Transaction`.
  - Actions of the `Auditor` may include the publishing of specific summaries of its views into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s.
  - Note: the `Auditor` need not be a regulatory entity; in particular this role might be suited e.g. to a "risk manager" in the context of a hedge fund.
  - Note further: an `Auditor` function could be something as low level (and automatable) as certifying that a set of Folios has never "double spent" - that everything that came out of it had gone into it at some point in the past.

### Table Specifications by Cohort

Adapting the terminology of Data Vault modeling gives us the following usage:
- Publishes: Hub
- Links: Link
- Attrs: Satellite

#### Contracts

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Novations** | Value[V] | Id | `Instruments.Key`[1,2], `Metas.Id` | `date: LocalDate`
**Instruments** | KeyValue[V] | Key[`USIN`] | `LegalEntity.Key` | `symbol: Label`, `issuedIn: Option[Currency]`
**Forms** | KeyValue[V] | Id | **`Instruments.Key`** | `display: Label`, `contract: Contract`, `state: SADT`

#### Ledger

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Trades** | Value[NEMKV] | Id | `Instruments.Key` | `_: Quantity`
**Folios** | KeyValue[MKV] | Id, Key[`UUID`] | `Instruments.Key` | `_: Quantity`
**Transactions** | Value[V] | Id | `Trade.Id`[2], `Folios.Key`[2], `Metas.Id` | `at: Instant`
**Confirmations** | KeyValue[V] | | **`Transaction.Id`**, `Folio.Id`[2] | `at: Instant`
**Metas** | Value[SADT] | Id | | `sadt: SADT`

#### Accounting

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Balances** | KeyValue[MKV] | Id | **`Folios.Key`** | `(AccountingKey, FinancialAmount)`
**Reports** | KeyValue[LV] | Id | **`Folios.Key`** , `Balance.Id`[3,4], *`Report.Id`*[0,1] | `asOf: Instant`, `period: Period`

#### People

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Accounts** | KeyValue[V] | Key[`AccountNo`] | `Folios.Key`[2], `Roster.Id` |
**Rosters** | Value[V] | Id | `Parties.Key` | `role: Role`, `stake: Option[Quantity]`
**NaturalPersons** | KeyValue[V] | `Parties.Key` | `Contact.Id` | `label: Label`, `ssn: Ssn`
**LegalEntities** |  KeyValue[V] | `Parties.Key` | `Contact.Id` | `label: Label`, `ein: Ein`
**Contacts** |  Value[SADT] | Id | | `sadt: SADT`

#### Markets

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Counterparties** | KeyValue[V] | Key[`UUID`] |  `Parties.Key`, `Folios.Key`, `Metas.Id` |
**Exchanges** | KeyValue[V] | Key[`MIC`] |  `Parties.Key`, `Folios.Key`, `Metas.Id` |
**MDSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`, `Metas.Id`, `MarketList.Id` |
**MarketLists** | Value[NELV] | Id | `MDS.Key`, `Exchange.Key` |
**OMSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`, `MarketList.Id`, `Folios.Key`[2] |
**Orders** | KeyValue[V] | Key[`Long`] | `OMS.Key`, `Market.Key`, `Trade.Id` | `at: Instant`, `currency: Currency`, `limit: Option[MonetaryAmount]`, `goodTill: Option[Instant]`, `attrs: Metas.Id`
**Executions** | Value[V] | Id | `Order.Key`, `Transaction.Id` | `at: Instant`

#### Misc

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**TickData** | KeyValue[V] | Key[`Label`] |  | `at: Instant`, `tick: Tick`, `price: MonetaryAmount`, `size: Quantity`
---|---|---|---|---|---
**SASH** | KeyValue[V] | |  **`*.Id`**, `Parties.Key` | `sig: Sig`
