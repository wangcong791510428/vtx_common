-define(set(Value, Default), case Value=/=undefined of true->Value; false->Default end).
-define(AUTH_HEAD, "Web realm=hive").

-define(ROOT_RID, "0").

-record(dsid, {
  next_dsid
}).

-record(user, {
  '_id', 
  uid,
  
  type,
  trait,
 
  user_name,
  password,
  nick_name,
  slug,
  salt,

  status,
  time_created,
  time_deleted,

  quota,
  
  sync_folders,
  workspaces,
  
  deleted,
  magic_briefcase,
  web_archive,
  mobile_photos,
  albums,
  
  recent_activities,
  received_shares,
  public_links,
  maximum_public_link_size,
  contacts
}).

-record(quota, {
  limit,
  usage
}).

-record(workspaces, {
  contents
}).

-record(workspace, {
  '_id',
  dsid,
  parent_dsid,
  uid,

  type,
  trait,

  display_name,
  status,
  time_created,
  time_deleted,
  icon_id,

  ref,
  contents,
  collections,
  files
}).

-record(workspace_content, {
  '_id',
  workspace_dsid,
  parent_dsid,

  type,
  trait,
 
  display_name,
  folder_dsid,
  folder_display_name
}).

-record(syncfolders, {
  contents
}).

-record(folders, {
  contents
}).

-record(folder, {
  '_id',
  dsid,
  uid,

  type,
  trait,

  ancestors,
  parent_dsid,
  display_name,

  status,
  time_created,
  last_modified,
  time_deleted,

  share_enabled,

  ref,
  parent,
  contents,
  collections,
  files
}).

-record(copy_file, {
  display_name, 
  source
}).

-record(entry, {
  type,
  trait,

  display_name,
  media_type,
  target
}).

-record(files, {
  contents
}).

-record(file_entry, {
  '_id',
  dsid,
  uid,

  type,
  trait,

  ancestors,
  parent_dsid,
  display_name,

  status,
  time_created,
  last_modified,
  time_deleted,

  media_type,
  size,
  stored_size,
  present_on_server,

  ref,
  parent,
  file_data,
  file_version_history,
  tags,
  image,
  public_link,
  last_file_version
}).

-record(public_link, {
  enabled
}).

-record(image, {
  height,
  width,
  rotation
}).


-record(collection_contents, {
  type,
  trait,

  start,
  has_more,
  end_,
  collections,
  files
}).

-record(collection, {
  type,
  trait,

  display_name,
  ref,
  contents,
  icon_id
}).

-record(file_version_history, {
  type,
  trait,

  start,
  has_more,
  end_,
  file_versions
}).

-record(file_version, {
  '_id',
  dsid,
  uid,
  file_entry_dsid,
  
  type,
  trait,

  stored_size,
  size,

  status,
  time_created,
  last_modified,
  time_deleted,

  media_type,
  present_on_server,
  file_data,
  ref
}).

-record(share, {
  user_id,
  dsid,

  display_name,
  time_received,
  shared_folder,
  permissions,
  owner
}).

-record(received_shares, {
  received_share
}).

-record(permissions, {
  read_allowed,
  write_allowed
}).

