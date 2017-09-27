# mule
test mule for muh scala dev env

This is text entered directly from [the GitHub website](https://github.com).

(Fixed, hopefully, in the local Atom editor.)

The new reality
: Primary components of the `datapath`:
  - `serialized compound values` (`SCV`s)
        - self describing (e.g. XML, JSON)
        - schema'd (e.g. Protobuf, GraphQL)
  - `streams` of `SCV`s (where `stream` is formal, e.g. Kafka or Reactive)
  - `collections`s of `SCV`s (where `collection` is e.g. a RowSet)
  - `database` instances which source and sink `collection`s or `stream`s of `SCV`s
