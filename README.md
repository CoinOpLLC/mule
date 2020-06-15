# `deftrade`  

A stream algebra toolkit for financial market participants.

```scala
  import io.deftrade._
  import money.Currency
  import model.Money
  import cats.effect.Sync
  import fs2.Stream

  /** Witnesses that `X` is exchangeable in a fair and lawful contract. */
  abstract class Consideration[X] { /* ... */ }

  /** Synchronous, effectful Arrow of Acquisition. */
  abstract class Ferengi[F[_]: Sync] { /* ... */ }

  /** Mission statement in a method signature gives us our project name. */
  def trade[F[_]: Ferengi, X: Consideration, C: Currency](x: X): Stream[F, Money[C]] =
    // ...
```

## Foundational financial domain values and calculations

- `ledger` data model specifies chained (!) cryptographic hashes
- `account` management: privacy-first data model protects PII
    - account and tax `Id`s are excluded from core `Ledger` data model  
    - `cash account` details (e.g. bank account) remain private to the `account`  
- composable `contract` DSL and evaluation engines
    - `form`s encapsulate common `contract`s for publicly traded `instrument`s
    - custom `contract`s (e.g. convertible notes) can be embedded within ad-hoc `instrument`s
        - "exempt" securities
    - non-cash instruments (e.g. public stock) are identified with common keys (e.g. `CUSIP`s)
- core `accounting` balances and reporting
- `order management system` integration for external markets
- `market data system` integration for real time tick stream providers
- `DSL`s for commercial time and money calculations

## Implementation notes

### data definitions use `ADT`s and `SACC`s.

- **`ADT`** := algebraic data type
    - enable principled `codec` derivation
    - binary (scodec)
    - csv (cormorant)
    - jdbc (doobie)
- **`SACC`** := sealed abstract case class
- "unforgable" typed values
    - private constructor
    - no copy method

### Abstract `ValueStore`s and `KeyValueStore`s
- `ValueStore`: source and sink `stream`s of `value`s for persistence
- `KeyValueStore`: tracks the evolution over time of a `value` identified by a `key`
- spreadsheet integration via csv file based persistence with json for adts
- KeyValue database candidates:
    - LightningDB:
    - FoundationDB: Keys cannot exceed 10 kB in size. Values cannot exceed 100 kB in size.
- Quill/Doobie/Postgres persistence layer
    - use Kafka to push pg log to the cloud for replication?
    - CockroachDB

### Stream based computations

`Stream`s are (possibly) effectful computations which produce sequences of `value`s.

- `fs2.Stream` (in memory)
- Kafka streams
- Spark

In-memory computations can be thought of as producers of `Stream`s of `Event`s.
Results of these computations should be recomputable by replaying the event stream to its consumers and *discarding their effects*.

- trading algos implement state as `aggregate-entities`
    - `aggregate entities` are maintained by `Stream` based computations
    - this is `CQRS/ES` and can be used to replicate / restore application or session state

Why scala 2.13?  
- Map is less broken
- type level is less broken

A project of [CoinOpLLC](https://coinopllc.com).

### NO WARRANTY  

>This is free software; see the source for copying conditions.
There is no warranty, not even for merchantability or fitness
for a particular purpose.
