# `def trade:` The Book of Books

### The Table of Tables

Note that the first section of tables denotes the complete record of the financial transactions and can be parsed and analyzed without reference to the entities who did the transacting.

The second section of tables denotes who did the transacting, and how it was accomplished.

Name | `Store` Shape | | Data Vault Factors | |
---- | --------------- |-----------------|-----------|----------------------------------------------------------------------------------------
 | |  | `Hub` | `Link` | `Satellite`
 | |  |---|---|---
**Novations** | Value[V] | Id | Instruments[1,2], Metas[1] | `date: LocalDate`
**Instruments** | KeyValue[V] | Key[Usin] | LegalEntities[1] | `symbol: Label`, `issuedIn: Option[Currency]`
**Forms** | KeyValue[V] | Id, *`Instrument.Key`*| | `display: Label`, `contract: Contract`
 | |  |---|---|---
**Trades** | Value[NEMKV] | Id | Instruments[1] | `_: Quantity`
**Folios** | KeyValue[MKV] | Id, Key[UUID] | Instruments[1] | `_: Quantity`
**Transactions** | Value[V] | Id | Trades[1], `Folio.Key`[2], Metas[1] | `at: Instant`
**Confirmations** | KeyValue[V] | *`Transaction.Id`* | `Folio.Id`[2] | `at: Instant`
**Metas** | Value[SADT] | Id | | `sadt: SADT`
 | |  |---|---|---
**Balances** | KeyValue[MKV] | *`Folio.Key`* | | `(AccountingKey, FinancialAmount)`
**Reports** | KeyValue[V] | Id, *`Folio.Key`* | Balances[3,4], Reports[0,1] | `asOf: Instant`, `period: Period`
 | |  |===|===|===
**Accounts** | KeyValue[V] | Key[Account.No] | `Folio.Key`[2], Rosters[1] |
**Rosters** | Value[V] | Id | `Party.Key`[1,N] | `role: Role`, `stake: Option[Quantity]`
**NaturalPersons** | KeyValue[V] | *`Party.Key`* | Contacts[1] | `label: Label`, `ssn: Tax.Ssn`
**LegalEntities** |  KeyValue[V] | *`Party.Key`* | Contacts[1] | `label: Label`, `ein: Tax.Ein`
**Contacts** |  Value[SADT] | Id | | `sadt: SADT`
 | |  |---|---|---
**Counterparty** | KeyValue[V] | Key[UUID] |  `Party.Key`[1], `Folio.Key`[1], Metas[1] |
**Exchange** | KeyValue[V] | Key[MIC] |  `Party.Key`[1], `Folio.Key`[1], Metas[1] |
**MDSs** | KeyValue[V] | Key[Label] | LegalEntities[1], Metas[1], Markets[1-N] |
**TickData** | KeyValue[V] | Key[Label] |  | `at: Instant`, `tick: Tick`, `price: MonetaryAmount`, `size: Quantity`
 | |  |---|---|---
**OMSs** | KeyValue[V] | Key[Label] | LegalEntities[1], Markets[1-N], `Folio.Key`[2] |
**Orders** | KeyValue[V] | Key[Long] | Trades[1], Markets[1] | `at: Instant`, `currency: Currency`, `limit: Option[MonetaryAmount]`, `goodTill: Option[Instant]`, `attrs: Meta.Id`
**Executions** | Value[V] | Id | Orders[1], Transactions[1] | `at: Instant`
 | |  |---|---|---
**SASH** | KeyValue[V] | *`*.Id`* |  `Party.Key`[1] | `sig: Sig`
