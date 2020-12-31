# `deftrade`

A stream algebra for financial market participants.

## Installation

(don't, yet)

## Architecture
See the [docs](docs).

## Implementation notes

### `Stream` based computations

`fs2.Stream`s are (possibly) effectful computations which produce sequences of `value`s. In-memory computations can be thought of as producers of `Stream`s of `Event`s.

Trading "algo"s implement _state_ as DDD _aggregate-entities_
- maintained in memory by `Stream` based computations
- `KeyValueStore`: tracks the evolution over time of a `value` identified by a `key`
- this is `CQRS/ES` and can be used to replicate / restore application or session state
  - Results of these computations should be recomputable by replaying the event stream and *discarding the effects*.

### in-memory data definitions use `ADT`s and `SACC`s.

- **`ADT`** := algebraic data type
    - enable principled `codec` derivation
    - binary (scodec)
    - csv (cormorant)
    - jdbc (doobie)
- **`SACC`** := sealed abstract case class
- "unforgable" typed values
    - private constructor
    - no copy method

### persistence roadmap
- spreadsheet integration via csv file based persistence with json for adts
- KeyValue database candidates:
    - LightningDB:
    - FoundationDB: Keys cannot exceed 10 kB in size. Values cannot exceed 100 kB in size.
- Quill/Doobie/Postgres persistence layer
    - use Kafka to push pg log to the cloud for replication?
    - CockroachDB

### Why `scala 2.13`?
- `LazyList` for better Haskell porting
- `Map` is less broken
- literal singleton types
- `cats.evidence` integration


A project of [CoinOpLLC](https://coinopllc.com).

### NO WARRANTY

>This is free software; see the source for copying conditions.
There is no warranty, not even for merchantability or fitness
for a particular purpose.
