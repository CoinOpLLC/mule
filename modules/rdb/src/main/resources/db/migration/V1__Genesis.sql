
/*
 * Passive Agressive Record: basic idea: CR<strike>UD</strike> enforced through repository methods
 *
 * - decouple the notion of the entity id in the domain model
 *   (which has continutity over evolution of the entity state)
 *   from the primary key in the db (which is different for every state change in the entity)
 *
 * - entity id within the domain model has no naming convention...
 *   however a single non-unique index ending in _eid indicates an entity id on the column(s) referenced
 *
 * - 'id serial' is then a distinguished name and always serial4 or serial8 primary key
 *   guaranteed ascending as time evolves
 *
 * - 'span tstzrange' indicates when the field is valid
 *
 * - use views to get queries for "valid" or "current" rows
 *
 */

/*
http://stackoverflow.com/questions/1884758/generate-ddl-programmatically-on-postgresql
http://stackoverflow.com/questions/1771543/postgresql-updating-an-enum-type
*/
--
-- PIENET = human-free reactive pizza service
--  - online ordering
--  - robotic kitchens
--  - drone delivered
--  - BTC payments
--
drop type if exists pizza_size_e cascade;
create type pizza_size_e as enum (
  'slice',
  'small',
  'med',
  'large'
);

drop type if exists pizza_topping_e cascade;
create type pizza_topping_e as enum (
  'mozzarella',
  'pepperoni',
  'feta',
  'sausage',
  'ham',
  'onions',
  'peppers',
  'pineapple',
  'olives',
  'mushrooms'
);

drop type if exists order_status_e cascade;
create type order_status_e as enum (
  'submitted',
  'quoted',
  'confirmed',
  'queued',
  'prepared',
  'delivered',
  'canceled'
);

drop table if exists pizzas cascade;
create table pizzas (

  id serial4 primary key,

  name varchar(126) not null, -- this is the entity identity for the domain model
  toppings pizza_topping_e[] not null,
  meta jsonb default('{}') not null,

  span tstzrange not null
);
create index pizzas_name_dk on pizzas(name);
create index pizzas_meta_index on pizzas using gin (meta);
create index pizzas_span on pizzas using gist(span);

-- want constant id as user evolves (eg changes contact info)
drop table if exists users cascade;
create table users (

  id UUID primary key,

  email varchar(126) not null,            -- how we're all known now; new email -> new user
  signup timestamptz not null,            -- never changes for a given user name

  -- nb two ways of defaulting to empty; two different motivations
  default_location jsonb default null,    -- can change for a given email
  meta jsonb default('{}') not null,      -- can change for a given email

  span tstzrange not null
);
create index users_email_dk on users(email);
create index users_meta on users using gin (meta);
create index users_span on users using gist(span);

drop table if exists orders cascade;
create table orders (

  id serial8 primary key,

  user_id UUID references users(id) not null,

  deliver_to jsonb default null, -- NULL indicates use default user location
  est_wait interval not null,
  status order_status_e default('submitted') not null

);
create index orders_deliver_to on orders using gin(deliver_to);

--
-- users direct payments to go to these addrs
--
drop table if exists pienet_btc_addrs;
create table pienet_btc_addrs (
  addr varchar(36) primary key,
  span tstzrange not null
);
create index pienet_btc_addrs_span on pienet_btc_addrs using gist(span);

--
-- users direct change to go to these addrs
--
drop table if exists user_btc_addrs;
create table user_btc_addrs (
  user_id uuid references users(id),
  addr varchar(36) not null,
  span tstzrange not null,
  primary key (user_id, addr)
);
create index user_btc_addrs_span_user_id_dk on user_btc_addrs(user_id);
create index user_btc_addrs_span on user_btc_addrs using gist(span);

--
-- all payments (BTC transactions) recorded here
--
drop table if exists payments cascade;
create table payments (
  order_id int8 references orders(id) not null,
  pay_to varchar(36) not null,
  amount decimal not null,
  change_to varchar(36) not null,
  change decimal not null -- negative means change returned to user
);
create index payments_order_id_dk on payments(order_id);
--
-- note - because prices are quoted in BTC, every order item will vary due to BTC volatility.
-- (the real time pricing engine is proprietary to PIENET).
--
drop table if exists order_items cascade;
create table order_items (

  id serial8 primary key,

  pizza_id int4 references pizzas(id) not null,
  pizza_size pizza_size_e default ('slice') not null,
  quantity int2 default 1 not null, -- 32K pizzas ought to be enough for anyone
  unit_price decimal default -1.0 not null, -- in BTC - negative indicates order not yet quoted
  requests jsonb default ('{}')
);
create index order_items_pizza_id_dk on order_items(pizza_id);

drop table if exists order_item_links cascade;
create table order_item_links (
  order_id int8 references orders(id) not null,
  order_item_id int8 references order_items(id) not null,
  primary key (order_id, order_item_id)
);
create index order_item_links_order_id on order_item_links(order_id);
create index order_item_links_order_item_id on order_item_links(order_item_id);

insert into pizzas (name, toppings, span, meta) values
  ('cheese', '{"mozzarella"}', '[12/21/2012,)', '{}'),
  ('pepperoni', '{"mozzarella", "pepperoni"}', '[12/21/2012,)','{}'),
  ('mushroom', '{"mozzarella", "mushrooms"}', '[12/21/2012,)', '{}'),
  ('greek', '{"feta", "sausage", "olives", "onions", "peppers"}', '[12/21/2012,)', '{}'),
  ('hawaiian', '{"mozzarella", "ham", "pineapple"}', '[12/21/2012,)', '{}');
