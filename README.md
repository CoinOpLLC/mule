# mule

`mule` carries all of the libraries used by our [CoinOpLLC](https://coinopllc.com) projects.

`mule` integrates them so that they pack up well together for the journey.

`mule` wanders and loads new libraries in anticipation of their use.

---

### A Financial Stream Algebra Toolkit  

  - `value`s and `compound value`s
      - memory-first definitions
      - ADT basis enables persistence derivation

  - `streams` of `values`s
      - `fs2.Stream` (memory)
      - Kafka persistence layer (data in motion)

  - `stores`
      - source and sink `stream`s of `value`s for persistence
      - enable traceable evolution of an identified value over time
      - Postgres (Cockroach) persistence layer (data at rest)
      - spreadsheet integration via csv file based persistence

  - `aggregate entities`
      - `CQRS/ES` backed computations producing `stream`s of `event`s
      - can be used to replicate / restore application or session state

### NO WARRANTY  

>This is free software; see the source for copying conditions.
There is no warranty, not even for merchantability or fitness
for a particular purpose.
