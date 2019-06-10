-record(iris_user, {
  '_id',
  user_name,
  password,
  slug,

  status,
  quota,

  mount_list,

  time_created
}).

-record(iris_mount_point, {
  nsid,
  path
}).

-record(iris_namespace, {
  '_id',
  name,
  slug,
  vclock,

  refs,
  refs_num,

  archors
}).

-record(iris_name_entry, {
  '_id',
  nsid,
  path,
  slug,
  
  is_dir,
  is_shared,
  mime_type,
  bytes,
  
  status,
  ctime,
  mtime,
  
  hash
}).

-record(iris_journal_entry, {
  '_id',
  nsid,
  path,
  slug,
  
  rev,
  vclock,
  
  is_dir,
  is_shared,
  mime_type,
  bytes,
  
  status,
  ctime,
  mtime,
  
  hash,

  staled_nsid,
  staled_path,
  staled_slug,
  
  staled_rev,
  
  staled_is_dir,
  staled_is_shared,
  staled_mime_type,
  staled_bytes,
  
  staled_status,
  staled_ctime,
  staled_mtime,
  
  staled_hash
}).

-record(iris_snap_entry, {
  '_id',
  snap_id,
  nsid,
  path,
  slug,
  
  rev,
  vclock,
  
  is_dir,
  is_shared,
  mime_type,
  bytes,
  
  ctime,
  mtime,
  
  hash
}).

-record(iris_cursor, {
  '_id',
  last,
  bookmarks,

  % life cycle management
  status,
  expiration
}).

-record(iris_bookmark, {
  nsid,
  type,
  serial,
  position
}).

-record(iris_cursor_clue, {
  id,
  epoch
%  , last
}).

-record(iris_archor_entry, {
  '_id',
  archor,
  nsid,
  path,
  slug,
  
  rev,
  vclock,
  status,
  
  is_dir,
  is_shared,
  mime_type,
  bytes,
  
  ctime,
  mtime,
  
  hash
}).

-record(iris_delta, {
  cursor,
  entries,
  has_more,
  reset
}).

-record(iris_delta_entry, {
  '_id',
  nsid,
  path,
  slug,
  
  rev,
  vclock,
  status,
  
  is_dir,
  is_shared,
  mime_type,
  bytes,
  
  ctime,
  mtime,
  
  hash
}).

-record(iris_delta_iterator, {
  cursor_clue
}).

-record(iris_chunk, {
  '_id',
  size,
  
  refs,
  refs_num
}).

-record(iris_sharing, {
  '_id',
  nsid,
  ns_name,
  
  user_name,
  role,
  
  status,
  ctime
}).

-record(iris_ns_job, {
  '_id',
  nsid,
  path,
  
  type,
  ctime,
  rev,
  
  state,
  mtime
}).

-record(iris_ns_revision, {
  '_id',
  job_id,
  nsid,
  type,
  
  vclock,
  padding,
  hopping,
  overwrite,
  parent_rev,
  
  active_path,
  active_slug,
  active_rev,
  active_is_dir,
  active_is_shared,
  active_mime_type,
  active_bytes,
  active_status,
  active_ctime,
  active_mtime,
  active_hash,
  active_chunk_list,
  
  updated_path,
  updated_slug,
  updated_rev,
  updated_is_dir,
  updated_is_shared,
  updated_mime_type,
  updated_bytes,
  updated_status,
  updated_ctime,
  updated_mtime,
  updated_hash,
  updated_chunk_list
}).

-record(iris_rendezvous_entry, {
  '_id',
  rendezvous_id,
  nsid,
  type,

  begin_vclock,
  end_vclock
}).

%-record(iris_folder_metadata, {
%  '_id',
%  nsid,
%  path,
%  slug,
%  
%  is_dir,
%  is_shared,
%  mime_type,
%  bytes,
%  
%  status,
%  ctime,
%  mtime,
%  
%  hash,
%  content
%}).

-define(HOME_PATH,  "/").

-define(NSJOB_INIT,  0).
-define(NSJOB_RUN,   1).
-define(NSJOB_DONE,  2).

-define(JOURNAL_SIZE,  10).
-define(KEY, "12345678901234567890").
