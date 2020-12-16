# `def trade:` The Book of Books

### The Table of Tables

Note that the first section of tables denotes the complete record of the financial transactions and can be parsed and analyzed without reference to the entities who did the transacting.

The second section of tables denotes who did the transacting, and how it was accomplished.

- Publishes: Hub
- Links: Link
- Attrs: Satellite

---
<p style="page-break-before: always">

#### Contracts  

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Novations** | Value[V] | Id | `Instrument.Key`[1,2], `Meta.Id` | `date: LocalDate`
**Instruments** | KeyValue[V] | Key[`USIN`] | `LegalEntity.Key` | `symbol: Label`, `issuedIn: Option[Currency]`
**Forms** | KeyValue[V] | Id | **`Instrument.Key`** | `display: Label`, `contract: Contract`

#### Ledger  

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Trades** | Value[NEMKV] | Id | `Instrument.Key` | `_: Quantity`
**Folios** | KeyValue[MKV] | Id, Key[`UUID`] | `Instrument.Key` | `_: Quantity`
**Transactions** | Value[V] | Id | `Trade.Id`, `Folio.Key`[2], `Meta.Id` | `at: Instant`
**Confirmations** | KeyValue[V] | | **`Transaction.Id`**, `Folio.Id`[2] | `at: Instant`
**Metas** | Value[SADT] | Id | | `sadt: SADT`

#### Accounting  

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Balances** | KeyValue[MKV] | Id | **`Folio.Key`** | `(AccountingKey, FinancialAmount)`
**Reports** | KeyValue[LV] | Id | **`Folio.Key`** , `Balance.Id`[3,4], *`Report.Id`*[0,1] | `asOf: Instant`, `period: Period`

#### People   

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Accounts** | KeyValue[V] | Key[`AccountNo`] | `Folio.Key`[2], `Roster.Id` |
**Rosters** | Value[V] | Id | `Party.Key` | `role: Role`, `stake: Option[Quantity]`
**NaturalPersons** | KeyValue[V] | `Party.Key` | `Contact.Id` | `label: Label`, `ssn: Ssn`
**LegalEntities** |  KeyValue[V] | `Party.Key` | `Contact.Id` | `label: Label`, `ein: Ein`
**Contacts** |  Value[SADT] | Id | | `sadt: SADT`

#### Markets  

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**Counterparties** | KeyValue[V] | Key[`UUID`] |  `Party.Key`, `Folio.Key`, `Meta.Id` |
**Exchanges** | KeyValue[V] | Key[`MIC`] |  `Party.Key`, `Folio.Key`, `Meta.Id` |
**MDSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`, `Meta.Id`, `MarketList.Id` |
**MarketLists** | Value[NELV] | Id | `MDS.Key`, `Exchange.Key` |  
**OMSs** | KeyValue[V] | Key[`Label`] | `LegalEntity.Key`, `MarketList.Id`, `Folio.Key`[2] |
**Orders** | KeyValue[V] | Key[`Long`] | `OMS.Key`, `Market.Key`, `Trade.Id` | `at: Instant`, `currency: Currency`, `limit: Option[MonetaryAmount]`, `goodTill: Option[Instant]`, `attrs: Meta.Id`
**Executions** | Value[V] | Id | `Order.Key`, `Transaction.Id` | `at: Instant`

#### Misc  

Name | Shape | Publishes | Links | Attrs
---- | --------------- |-----------------|-------------------------------|----------------------------------------------------------------------------------------
**TickData** | KeyValue[V] | Key[`Label`] |  | `at: Instant`, `tick: Tick`, `price: MonetaryAmount`, `size: Quantity`
---|---|---|---|---|---
**SASH** | KeyValue[V] | |  **`*.Id`**, `Party.Key` | `sig: Sig`
