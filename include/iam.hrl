%-define(set(Value, Default), case Value=/=undefined of true->Value; false->Default end).

-record(developer_identity, {
  '_id',
   
  display_name,
  email,
  password,
  phone,
 
  kid,
  key, 
 
  status,
  time_created,
  time_deleted
}).
 
-record(key_pair, {
  '_id',
  key,

  description
}).

-record(key_ring, {
  key_pairs
}).

-record(app_identity, {
  '_id',
   
  display_name,
  developer_id,

  entitlement,

  status,
  time_created,
  time_deleted
}).


-record(user_identity, {
  '_id',
   
  display_name,
  email,
  password,
  slug,
  devices,

  kid,
  key,
  quotas,
  connectors,
 
  status,
  time_created,
  time_deleted,

  user_credential
}).

-record(vuser, {
  '_id',
   
  user_id,
  display_name,
 
  kid,
  key, 

  groups,
  entitlement
}).


-record(connector, {
  '_id',
  ref,

  user_id,
  agent_type,
  agent_id,
  agent_group,
 
  time_created,
  user_credential
}).

-record(session, {
  id,
  
  agent_type,
  agent_id,
  agent_group,
  user, 
  vuser,
  vgroup,
  application,

  policy
}).

-record(entitlement, {
  '_id',
  ref,

  status,
  zone_id,

  agent_type,
  agent_id,
  agent_group,
  agent_slug,

  policy,

  time_created
}).

-record(policy_statement, {
  action,
  effect,
  resource
}).

-record(vgroup, {
  '_id',
  user_id,
  display_name
}).
