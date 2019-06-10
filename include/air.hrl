-record(app_connector, {
  developer_kid,
  developer_key,
  app_id,
  use_credential,
  workspace
}).

-record(developer_studio, {
  developer_display_name,
  developer_password,
  developer_email,
  developer_phone,
  app_display_name,
  syncfolders,
  user_display_name,
  user_credential
}).
