CREATE OR REPLACE PACKAGE APEX_IR_PKG
IS
      ------------------------------------------------------------------------------------------------
   /* Parses the sql and checks for the existence of a display and/or
      return value column.
      If the sql (which can be from a static or dynamic lov) does not
      contain a display value column, the return value one doubles as
      one.
      Valid values for:
      display value column: D, DISPLAY_VALUE
      return value column: R, RETURN_VALUE

      These values are what apex would require you to provide when
      creating an lov.

      The name for the display and return value columns are returned
      through the output variables.
   */
   PROCEDURE parse_sql_for_columns
   (
      p_sql          IN VARCHAR2,  -- the sql to be parsed for display/return columns
      o_display_col  OUT VARCHAR2, -- the column alias for the display column
      o_return_col   OUT VARCHAR2  -- the column alias for the return column
   );
   ------------------------------------------------------------------------------------------------
   /* Will parse the condition sql and search for columns which are based on
      an LOV. These columns then need to be remapped to the display value
      of those LOVs
   */
   FUNCTION get_ir_filter_lov_row
   (
      p_app_id          IN NUMBER,
      p_ir_bid          IN VARCHAR2, -- report BASE id
      p_condition_sql   IN VARCHAR2  -- condition sql
   )
   RETURN VARCHAR2;
   ------------------------------------------------------------------------------------------------
   /* It is possible to search on the displayed value of entries. For example, when there is a 'STATUS'
      with value 'In Use', then you can apply a filter on STATUS which only searches for 'Use' -> LIKE '%Use%'
      An exact match is POSSIBLE, but not necessarily!

      What has to happen:
      The filter condition which is applied on a column based on a LOV has to be applied to the
      DISPLAY_VALUEs of the LOV, NOT on the RETURN_VALUEs.
   */
   FUNCTION get_ir_filter_lov_col
   (
      p_app_id                IN NUMBER,
      p_named_lov             IN VARCHAR2,  -- the name of the referenced LOV
      p_condition_col_name    IN VARCHAR2,  -- the name of the (db)column being filtered
      p_condition_sql         IN VARCHAR2,  --
      p_condition_operator    IN VARCHAR2,
      p_condition_expression1 IN VARCHAR2,  -- expression is usually the value of the search
      p_condition_expression2 IN VARCHAR2   -- expression is usually the value of the search
   )
   RETURN VARCHAR2;
   ------------------------------------------------------------------------------------------------
   /* NEXT_PREV_VALUES:
      fetches the NEXT, PREVIOUS, TOP, BOTTOM, CURRENT and TOP values for the
      specified application+page+interactive report+column (usually an id column)
      When you specify filters and searches on an IR, you can't easily retrieve
      the next and previous values of a single record.
      For example, when you have a list of models and filter it down, and go to
      a detail page of a model (= a form page), you can't find the next and
      previous model within that filtered set. The standard form process in apex
      which provides record navigation does not offer navigation based off an IR
      either. It also has sorting limitations.

      Wish to use ROWID? Then make sure you have ROWID ALIASED in your query!
      Since the ir query will be made into a subquery, and ROWID is a pseudocolumn,
      it has to be aliased if it is to be selected out of this subquery.

      Substitution variables are NOT supported. If your query has been generated
      then there is a good chance it'll include #OWNER#. This procedure will
      fail because i have provided no support for replacing those strings.
      Simply alter your region source and remove those strings.

      p_use_bvar1-4: specify TRUE when this bind variable has to be used
                     This is done so your bind var can have a NULL value
                     It is assumed that when you flag bindvar 3 as being
                     used, bind var 1 and 2 are also being used! There is
                     no conditional testing on which combinations of vars
                     are used!
   */
   PROCEDURE get_navigation_values
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_column_id          IN  VARCHAR2, -- the column for which to get the next/prev vals
      p_value              IN  VARCHAR2, -- The id value (for p_column_id) of the selected record: indicates current record
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_report_id          IN  NUMBER,   -- report id, leave NULL to automatically retrieve this.
      p_app_user           IN  VARCHAR2, -- user (APP_USER)
      p_use_session_state  IN  BOOLEAN DEFAULT TRUE, -- true for using apex session state bind vars. If False p_binds+vals are to be filled.
      p_binds              IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables
      p_binds_val          IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables VALUES
      p_next               OUT VARCHAR2, -- next value
      p_prev               OUT VARCHAR2, -- previous value
      p_top                OUT VARCHAR2, -- top value: first record
      p_bot                OUT VARCHAR2, -- bottom value: last record
      p_cur_tot            OUT VARCHAR2, -- current of total: '4 of 132'
      p_debug              OUT VARCHAR2  -- Returns the final and adjusted executed query
   );
   ------------------------------------------------------------------------------------------------
   FUNCTION get_column_ac_values
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_column_id          IN  VARCHAR2, -- the column for which to get the next/prev vals
      p_value              IN  VARCHAR2, -- the current search value for p_column_id IF NULL THEN ALL
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_report_id          IN  NUMBER,   -- id of the selected IR, this can be null
      p_use_session_state  IN  BOOLEAN DEFAULT TRUE, -- true for using apex session state bind vars. If False p_binds+vals are to be filled.
      p_binds              IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables
      p_binds_val          IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables VALUES
      p_search_behaviour   IN  VARCHAR2 DEFAULT 'CONTAINS', -- LIKE, CONTAINS: how results are fetched
      p_filter_behaviour   IN  VARCHAR2 DEFAULT 'FILTER' -- FILTER, ALL: filtering of results
   )
   RETURN CLOB;
   ------------------------------------------------------------------------------------------------
   FUNCTION get_existing_ir_filter
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_app_user           IN  VARCHAR2, -- user (APP_USER)
      p_column_name        IN  VARCHAR2  -- column for which to retrieve filter
   )
   RETURN VARCHAR2;
   ------------------------------------------------------------------------------------------------
      FUNCTION get_ir_sql
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_report_id          IN  NUMBER,   -- report id, leave NULL to automatically retrieve this.
      p_app_user           IN  VARCHAR2, -- user (APP_USER)
      p_use_session_state  IN  BOOLEAN DEFAULT TRUE, -- true for using apex session state bind vars. If False p_binds+vals are to be filled.
      p_binds              IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables
      p_binds_val          IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables VALUES      
      p_incl_filters       IN  BOOLEAN DEFAULT TRUE, --whether to include applied filters or not      
      p_incl_order_by      IN  BOOLEAN DEFAULT TRUE  --whether to include the order by or not
   )
   RETURN VARCHAR2;
   ------------------------------------------------------------------------------------------------
END apex_ir_pkg;
/