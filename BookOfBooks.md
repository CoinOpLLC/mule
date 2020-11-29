# `def trade:` The Book of Books

### The Table of Tables

Note that the first section of tables denotes the complete record of the financial transactions and can be parsed and analyzed without reference to the entities who did the transacting.

The second section of tables denotes who did the transacting, and how it was accomplished.

- Publishes: Hub
- Links: Link
- Attrs: Satellite 

Name | `Store` Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Novations** | Value[V] | Id | `Instrument.Key`[1,2], `Meta.Id`[1] | `date: LocalDate`
**Instruments** | KeyValue[V] | Key[`USIN`] | `LegalEntity.Key`[1] | `symbol: Label`, `issuedIn: Option[Currency]`
**Forms** | KeyValue[V] | Id, *`Instrument.Key`*| | `display: Label`, `contract: Contract`
**Trades** | Value[NEMKV] | Id | `Instrument.Key`[1] | `_: Quantity`
**Folios** | KeyValue[MKV] | Id, Key[`UUID`] | `Instrument.Key`[1] | `_: Quantity`
**Transactions** | Value[V] | Id | `Trade.Id`[1], `Folio.Key`[2], `Meta.Id`[1] | `at: Instant`
**Confirmations** | KeyValue[V] | *`Transaction.Id`* | `Folio.Id`[2] | `at: Instant`
**Metas** | Value[SADT] | Id | | `sadt: SADT`
**Balances** | KeyValue[MKV] | *`Folio.Key`* | | `(AccountingKey, FinancialAmount)`
**Reports** | KeyValue[V] | Id, *`Folio.Key`* | `Balance.Key`[3,4], *`Report.Id`*[0,1] | `asOf: Instant`, `period: Period`
 | |  |===|===|===
**Accounts** | KeyValue[V] | Key[`AccountNo`] | `Folio.Key`[2], `Roster.Id`[1] |
**Rosters** | Value[V] | Id | `Party.Key`[1,N] | `role: Role`, `stake: Option[Quantity]`
**NaturalPersons** | KeyValue[V] | *`Party.Key`* | `Contact.Id`[1] | `label: Label`, `ssn: Ssn`
**LegalEntities** |  KeyValue[V] | *`Party.Key`* | `Contact.Id`[1] | `label: Label`, `ein: Ein`
**Contacts** |  Value[SADT] | Id | | `sadt: SADT`
**Counterparties** | KeyValue[V] | Key[`UUID`] |  `Party.Key`[1], `Folio.Key`[1], `Meta.Id`[1] |
**Exchanges** | KeyValue[V] | Key[`MIC`] |  `Party.Key`[1], `Folio.Key`[1], `Meta.Id`[1] |
**MarketLists** | Value[NELV] | Id | `MDS.Key`[1], `MarketList.Id`[1] |  
**MDSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`[1], `Meta.Id`[1], `MarketList.Id`[1] |
**TickData** | KeyValue[V] | Key[`Label`] |  | `at: Instant`, `tick: Tick`, `price: MonetaryAmount`, `size: Quantity`
**OMSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`[1], `MarketList.Id`[1], `Folio.Key`[2] |
**Orders** | KeyValue[V] | Key[`Long`] | `Trade.Id`[1], `Market.Key`[1] | `at: Instant`, `currency: Currency`, `limit: Option[MonetaryAmount]`, `goodTill: Option[Instant]`, `attrs: Meta.Id`
**Executions** | Value[V] | Id | `Order.Key`[1], `Transaction.Id`[1] | `at: Instant`
 | |  |---|---|---
**SASH** | KeyValue[V] | *`*.Id`* |  `Party.Key`[1] | `sig: Sig`
