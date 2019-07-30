# mule

`mule` carries all of the libraries used by our [CoinOpLLC](https://coinopllc.com) projects.

`mule` integrates them so that they pack up well together for the journey.

`mule` wanders and loads new libraries in anticipation of their use.

### Premise: KAPS architecture
: Primary components of the `datapath`:
  - `value`s and `compound value`s
      - self describing (eg XML, JSON)
      - schema'd (eg Protobuf, GraphQL)
      - either way: _isomorphism_ (think `bimap`) between wire/persistence format and memory format
  - `streams` of `values`s
      - where `stream` is (**at least**) a formal a formal abstraction, eg Kafka or Reactive
      - moving towards complete encapsulation in an algebra (i.e. Monix, Quill)

  - `collections`s of `value`s, where `collection` is both
      - a formal abstraction eg a `RowSet`
      - and an algebra (eg `Monoid`)

  - `repos`
      - instances which source and sink `collection`s or `stream`s of `value`s for persistence

  - `aggregate entities`
      - CQRS/ES backed actors
