# jinx Database Tools

jDbT is a small utility designed to quickly generate SQL schemas and
associated information from a lightweight spec.

## TL;DR

`schema.yml`

```yaml

status:
  - Test
  - Prod

team:
  name: text

address:
  line1: text
  line2: text
  zip: text
  city: text
  country: text

post:
  +?title: text
  ?content: text

member:
  firstname: text
  lastname: text
  nickname: text
  ?team_id:
  address_id:
  status: status | 'Test'
  __unique: [ firstname, lastname ]

tag:
    +name: text
    __check: name <> 'prolapse'

post_tag:
    post_id:
    tag_id:
    __pk: [ tag_id, post_id ]

```

    jdbt ./schema.yml > schema.sql
    jdbt ./schema.yml dot | dot -Tpng -o schema.png
    # or
    curl -F "schema=<schema.yml" http://jdbt.cleverapps.io/


`schema.sql`

```sql

create type status as enum('Test', 'Prod');

create table team (
    team_id uuid primary key,
    name text not null
);

create table address (
    address_id uuid primary key,
    line1 text not null,
    line2 text not null,
    zip text not null,
    city text not null,
    country text not null
);

create table post (
    post_id uuid primary key,
    title text unique,
    content text
);

create table member (
    member_id uuid primary key,
    firstname text not null,
    lastname text not null,
    nickname text not null,
    team_id uuid references team(team_id),
    address_id uuid not null references address(address_id),
    status status  default  'Test'::status  not null,
    unique (firstname, lastname)
);

create table tag (
    tag_id uuid primary key,
    name text not null unique,
    constraint cst_0 check (name <> 'prolapse')
);

create table post_tag (
    post_id uuid not null references post(post_id),
    tag_id uuid not null references tag(tag_id),
    primary key (tag_id, post_id)
);

```

`schema.png`

![](./schema.png)

## Install

    cabal sandbox init
    cabal install
    cp .cabal-sandbox/bin/jdbt ~/.local/lib

## Use

### Input file

The input is a `yml` file describing the tables.

The following conventions are assumed:

 - every field has a `not null` constraint by default, unless it has a `?` at
   the beginning of its name
 - `+` at the beginning of a field name means it has a `unique` constraint
 - all tables have a primary key named `<table_name>_id` of type uuid
 - every column named `<something>_id` is assumed to be a foreign key

ToDo: allow the overriding of these conventions:

 - allow to define these properties explicitely
 - don't add the primary key if there is already one defined

### Output formats

#### SQL

Right now only PostgreSQL is supported. Are there any other RDBMSs out there
anyway?

#### Dot

jDbT can output a file in the `dot` format, which you can compile with `dot`,
from `Graphviz`.

#### Play Evolution scripts

ToDo

#### Scala case classes

ToDo

#### Haskell records

ToDo

#### SQL dummy data

ToDo

## History

jDbT was originally a python script put together by @clementd, @ahoy\_jon and
@benjaminvialle for contract work. @clementd rewrote it from scratch in
Haskell after a few years
