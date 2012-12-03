DECLARE
   lNullTable  DBMS_SQL.VARCHAR2_TABLE;
   lDebug      VARCHAR2(4000);
   lSql        VARCHAR2(4000);
   lCount      NUMBER;
BEGIN
   lSql := apex_ir_pkg.get_ir_sql
      (
         p_app_id             => :APP_ID,
         p_session_id         => :APP_SESSION,
         p_page_id            => :APP_PAGE_ID,
         p_report_id          => NULL,
         p_app_user           => :APP_USER,
         p_use_session_state  => TRUE,
         p_binds              => lNullTable,
         p_binds_val          => lNullTable,
         p_incl_filters       => TRUE,
         p_incl_order_by      => FALSE
      );
   
   lSql := 'SELECT count(*) FROM ('||lSql||')';
   
   EXECUTE IMMEDIATE lSql INTO lCount;
END;