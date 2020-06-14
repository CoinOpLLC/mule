# mule

`mule` carries all of the libraries used by our [CoinOpLLC](https://coinopllc.com) projects.

`mule` integrates them so that they pack up well together for the journey.

`mule` wanders and loads new libraries in anticipation of their use.

---

### **`deftrade`**

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

##### Foundational financial domain values and calculations

- composable contract dsl and evaluation engines
- ledger schema includes chained (!) cryptographic hashes
- account management: privacy-first data model protects PII
- core accounting balances and reporting
- order management system integration for external markets
- market data systems integration
- `dsl`s for commercial time and money calculations

- `adt-sacc` definitions
    - **`adt`**`:= algebraic data type`
    - **`sacc`**`:= sealed abstract case class`
    - "unforgable" typed values
        - private constructor
        - no copy method
- `adt`s enable principled `codec` derivation
    - binary (scodec)
    - csv (cormorant)
    - jdbc (doobie)

##### Abstract `ValueStore`s and `KeyValueStore`s
- `ValueStore`: source and sink `stream`s of `value`s for persistence
- `KeyValueStore`: tracks the evolution over time of a `value` identified by a `key`
- spreadsheet integration via csv file based persistence with json for adts
- KeyValue database candidates:
    - LightningDB:
    - FoundationDB: Keys cannot exceed 10 kB in size. Values cannot exceed 100 kB in size.
- Quill/Doobie/Postgres persistence layer
    - use Kafka to push pg log to the cloud for replication?
    - CockroachDB

##### Stream based computations

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

### NO WARRANTY  

>This is free software; see the source for copying conditions.
There is no warranty, not even for merchantability or fitness
for a particular purpose.
