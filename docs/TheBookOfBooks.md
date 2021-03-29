`deftrade`: The Book of Books
---

For the Developer:

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
- algebraically composable `Contract`s
  - creation `DSL`
  - evaluation `Engine`s
  - public `Instrument`s embed standard `Contract`s
    - for e.g. publicly traded securities or a standard convertible note
  - private `Instrument`s embed custom `Contract`s
  - `Form`s record the `Contract`s **_state_** and its _evolution over time_
  - `DSL` includes commercial time and money calculation primitives
- flexible `Ledger` data model
  - cryptographic hashes as primary key types
  - simple (content-addressed `Set` index) or _chained_ (`List`)
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

<!-- `deftrade` is a _model_ in the functional-reactive sense: prefer _models_ to "presentations" ("Fran" paper - MS Research '97) -->

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

### `Numéraire`: units of account

`Numéraire` is formal finance term which, contrary to what an understandably naive anglophone might think, signifies the **denominator** (not numerator) for `Contract`s and `transaction`s.

There are exactly two ways we can "settle the bill", so to speak: `InCoin`, and `InKind`.

The application level is where we declare the policy decisions about details associated with both modes.

Two significant feature of the `Numéraire`:
- it **_is_** a (primitive)`Contract` - see primitives above
- it also **_has_** a `Contract`, embedded in an effect type `F[_]` with both `Monad` and `Defer` typeclass instances.

#### `InCoin`: money and currency

`Money` has the following properties of interest in this model:
 - Money is an amount denominated in a (supported) **`Currency`**
 - Money is **fungable**
 - Money is **self-pricing** (within its denomination)

 The embeded `Contract` for all `InCoin` denominations is _self referencing_.

#### `InKind`: all other consideration

Most kinds of debt are simply `Contract`s for future `Money`; as such they constitute non self-referential `InKind` contracts. Equity "call" options constitute another.

_Some_ `InKind` contracts are self referential, like `InCoin`:
 - `Contract`s for **equity** (e.g shares of common stock)
 - `Contract`s for **real property** (e.g. commercial property leases)
 - `Contract`s for **physical delivery of commodities** (e.g. tanks of propane)

 These types of self-referential `InKind` contracts represent a bidirectional channel, defined by the `Contract`:
 - delivering:
   - a subscription to privately published information (e.g. board minutes)
   - a calendar of dividend distributions
- receiving:
  - votes
  - other elections?

Point being: equity is both a pub/sub system and a voting platform; aspects not commonly considered when conjering the "stock market".

### Contract DSL

What follows is a brief description of the `DSL` for contract specification, representation and evaluation, capturing the operational semantics of contract performance.

#### Primitives

```scala
   /** Party immediately acquires one unit of `Numéraire` from counterparty. */
   def unitOf(base: Numéraire): Contract

   /** Party acquires `c` multiplied by `n`. */
   def scale(n: Oracle[Double])(c: => Contract): Contract

   /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`. */
   def branch(b: Oracle[Boolean])(cT: => Contract)(cF: => Contract): Contract

   /** Party assumes role of counterparty with respect to `c`. */
   def give(c: => Contract): Contract = new Give(later(c)) {}

   /** Party will acquire c as soon as `b` is observed `true`.  */
   def when(b: Oracle[Boolean])(c: => Contract): Contract

   /** Party acquires `c` with the obligation to abandon it when `o` is observed `true`. */
   def until(b: Oracle[Boolean])(c: => Contract): Contract

   /** Once you acquire anytime obs c, you may acquire c at any time the observable obs is true. */
   def anytime(b: Oracle[Boolean])(c: => Contract): Contract

   /** Party immediately receives both `c1` and `c2`. */
   def both(c1: => Contract, c2: => Contract): Contract

   /** Party freely and immediately chooses between `c1` or `c2`. */
   def pick(c1: => Contract, c2: => Contract): Contract
```

#### `Oracle`s

Rather than the aphasic glossalila attributed to the pithia of the classical hellenic world, in _**our**_ system, `Oracle`s deliver truth.

There are a number of different kinds of oracles:
- time
- parametric state
  - e.g. strike and expiry for a call option
- (private) state
  - e.g. principle amount due on a term loan
- issuer commands
  - e.g. callable bond issuer exercising option
- holder commands
  - e.g. equity call buyer exercising option
- ledger derived facts
  - e.g. "credit rating" (underwriting metric)
- market data
  - e.g. S&P 500 quarterly closing level
  - e.g. WSJ prime rate

Note that not all contracts will use all kinds of oracles.

TODO: investigate potential for DeFi integration (which seems high)

### Engines

Use an `Engine` to `eval`uate a `Contract`s and obtain a result. That result may be **effectful**, but is not required to be.

The difference between "workflow automation" and "smart contract execution" is a matter of degree, perspective, and counterparty platform integration.

There are currently two kinds of `Engine`s:

#### Pricing

Evaluate the `Contract` in the specified `Currency`, returning a **real valued process** which represents the distribution of possible `Contract` values a given number of `TimeSteps` from `t0`.


#### Performing

Automated `Contract` performance, including the capacity to commit Transactions to the Ledger.

Note that "automation" may include flows provided standard cloud based document signing platform providers (such as DocuSign). From the perspective of the `Contract` issuer, the humans are just mechanical Turks providing inputs which the signing platform "lifts" into to `Oracle` status (providing both type safety and legal binding, if done right).

For drop-down into manual workflows, `Engine.eval` produces calendar schedules of actions for humans to follow up on:
 - make and expect payments, exercise options, etc.
 - uses automation tools as integrations become available

##### Transaction Driver Transfers

```scala
  Oracle
  ======        __________     ______
  holder =====>|          |==>|      |==> command(ck: ContractKey, amount: Quantity,
  issuer =====>| Contract |   | F[_] |            sender: Portfolios.Key,
  other  =====>| FSM      |   |      |            receiver: Portfolios.Key)
           +==>|__________|==>|___^__|==+ state
           |                      ^     |
           |                 flatMap()  |
           +============================+
```

`command` | description
---: | :---
deliver | send to recipient
expect | await the arrival of

If both `sender` and `receiver` are _local_, we can effect the transfer immediately and natively.

If either is a portfolio representing a _contra account_ for a gateway, additional automation and/or manual steps must be provisioned.

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

### Contract interaction examples and use cases

####

What happens when you have a covered call position, and the call your are short gets exercised against you, even if it makes no sense economically to do so? (e.g. the call buying counterparty may have been shorting a stock and needs to deliver that stock, even though the call option (that you sold them) expired "worthless".

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

TODO: Cite FRAN97 and explain how the system is functionally reactive - "updates like a spreadsheet" - with respect to market data.

#### Order Management

There is a discipline to the bidding process for proposed transactions between two parties. This discipline is historically durable and observed across timescales (from weeks and months, which is the timescale of real estate) to microseconds, which mark time for market makers that employ HFT technology.

The `Execution` of `Order`s, accomplished with the `Transaction`s already described, implements this discipline within our model. The `Order` events can be used to propose transactions for any asset from buildings to bitcoin.

The important points is that `Order` issuance, modification, and cancelation are decoupled from `Execution` (`Transaction` recording) in the `Market`.

Similarly, `Execution` is decoupled from the recording of `Confirmation`s.

This temporal decoupling introduces flexibility which historical practice suggests is valuable, and to which market participants are accustomed. However, it also creates complexity and data, which must be tracked.

Reference for the order/execution pipeline: [Functional and Reactive Domain Modeling, section 4.4](https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270)

TODO: need diagram of Ghosh's Kleisli pipe: Order -> Execution -> Confirmation -> Allocation

Ubiquitous within the domain is the acronym,`OMS`, which stands for **Order Management System**. Financial markets, especially publicly traded markets, are often grouped and accessed together over a session based API which manages the order lifecycle.


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

```scala
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
  - etc.

  A complete enumeration and definition is beyond the scope of this specification.

`Agent`
: The primary delegate selected by the `Principal`s.

  A `Princple` is their own `Agent` unless otherwise specified.

`Manager`
: The primary delegate selected by the `Agent`. `Party`(s) with responsibility for, and authority over, the disposition of assets in the `Account`. In particular, `Manager`s (and **only** `Managers`) may initiate actions which will result in `Transaction`s settling to the `Account`.

  An `Agent` is their own `Manager` unless otherwise specified.

`Auditor`
: First class participants, with a package of rights and responsibilities. `Auditor`s will have a (possibly limited) view into the state of the `Ledger`, and (optionally) the ability to block the **settlement** of a `Transaction` (but not its **record**) to the `Ledger` (i.e. "break trades"), or even to initiate the recording of a `Transaction`.
  - Actions of the `Auditor` may include the publishing of specific summaries of its views into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s. Therefore an `Auditor` can be considered as a kind of `Oracle`.
  - Note: the `Auditor` need not be a regulatory entity; in particular this role might be suited e.g. to a "risk manager" in the context of a hedge fund.
  - Note further: an `Auditor` function could be something as low level (and automatable) as certifying that a set of Folios has never "double spent" - that everything that came out of it had gone into it at some point in the past.

---
## The Table of Tables

This sections describes how the data models are layered and relate to each other.

The tables are specified using the [`keval` package](keval.md).

### Data Vault Modeling

Tables are described using [Data Vault](https://en.wikipedia.org/wiki/Data_vault_modeling) style modeling, and to use `hub`s and `link`s to define graphs of `value types` defined by `business key`s.

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

### Table Specifications by Cohort

Adapting the terminology of Data Vault modeling gives us the following usage:
- Publishes: Hub
- Links: Link
- Attrs: Satellite

#### Contracts

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Novations** | VS[`Novation`] | Id | `ante: Option[LegalEntities.Key]`, `post: Option[LegalEntities.Key]` | `asOf: LocalDate`
**Instruments** | KVS[`UUID`, `Instrument`] | Key | `issuedBy: LegalEntities.Key` | `symbol: Label`, `issuedIn: Option[Currency]`
**Forms** | KVS[`Instruments.Key`, `SADT[Form]`] | Id |  | `display: Label`, `contract: Contract`

#### Ledger

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | ---------|------ |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Trades** | VS[`Trade`] | Id |  | `_1: Instruments.Key` | `_2: Quantity`
**Folios** | KVS[`UUID`, `Folio`] | Id, Key | `_1: Instruments.Key` | `_2: Quantity`
**Portfolios** | VS[`Portfolio`] | Id | `open, escrowed, expected: Folios.Key` |
**Transactions** | VS[`Transaction`] | Id  |  `a, b: (Trades.Id, Folios.Key)`, `meta: Metas.Id` | `at: Instant`
**Confirmations** | KVS[`Transactions.Id`, `Confirmation`] |  | `from, to: Folio.Id` | `at: Instant`
**Metas** | VS.SADT[`Memo`] | Id  | | `Memo`

#### Accounting

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Balances** | VS[`Balance`] | Id | | `(_1: AccountingKey, _2: FinancialAmount)`
**Reports** | KVS[`Folios.Key`, `Report`] | Id | `cs, bs, is, es: Balance.Id`, `prev: Option[Report.Id]` | `asOf: Instant`, `period: Period`

#### Accounts

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Accounts** | KVS[`Account.No`, `Account`] | Key | `roster: Roster.Id`, `positions: Portfolios.Id` |
**Rosters** | VS.Codec[`Roster`] | Id | `party: Parties.Key` | `role: Role`, `stake: Option[Quantity]`

#### Entities

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|---------------------------------------------------------------------------------------
**NaturalPersons** | KVS[`Parties.Key`] |  Key | `contact: Contact.Id` | `label: Label`, `ssn: SSN`
**LegalEntities** |  KVS[`Parties.Key`] |  Key | `Contact.Id` | `label: Label`, `ein: EIN`
**Contacts** |  VS.SADT[`Contact`] | Id | | `_: Contact`

#### External Markets

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Counterparties** | KVS[`UUID`, `Counterparty`] | Key |  `host: NaturalPersons.Key`, `contra: Portfolios.Id`, `meta: Metas.Id` |
**Exchanges** | KVS[`MIC`, `Exchange`] | Key  |  `host: LegalEntities.Key`, `contra: Portfolios.Id`, `meta: Metas.Id` |
**ExchangeSets** | KLVS[`Label`, `Exchanges.Key`] | Key  |  `Exchanges.Key` |

#### Trading

Name | Store[`Spec`] | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**MDSs** | KVS[`Label`, `MDS`] | Key | `provider: LegalEntities.Key`, `markets: ExchangeSets.Key` `meta: Metas.Id` |
**TickDataSets** | KVS[`Label`, `TickData`] | Key |  | `at: Instant`, `tick: Tick`, `price: MonetaryAmount`, `size: Quantity`
**OMSs** | KVS[`Label`, `OMS`] | Key | `host: LegalEntities.Key`, `markets: ExchangeSets.Key`, `meta: Metas.Id` |
**Orders** | KVS[`Opaque[Long]`, `Order`] | Key | `oms: OMSs.Key`, `exchange: Exchanges.Key`, `account: AccountNo`, `Trades.Id`, `attrs: Metas.Id` | `at: Instant`, `currency: Currency`, `limit: Option[MonetaryAmount]`, `goodTill: Option[Instant]`
**Executions** | V[`Execution`] | Id | `order: Orders.Key`, `transaction: Transaction.Id` | `at: Instant`

#### Misc

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
---|---|---|---|---|---
**SASHs** | KVS[`*.Id`, `SASH`] | Id | `signer: Parties.Key` | `sig: Sig`

### Laws of the Model Algebra

TODO: placeholders; these need further development

The idea is to be inspired by Debashish Ghosh's exortation for find laws in your business logic; this is what makes it an _algebra_.

#### Single Spend

Law: When summed over all `Folio`s, the total amount of each `Numéraire` is zero.

#### Complete Confirmation Coverage

Law: the set union of `Confirmation`s exactly equals the set union of `Folio`s.

#### Contract Pricing Arbitrage Neutrality

Law: the `Contract` pricing `Engine` obeys arbitrage neutrality
