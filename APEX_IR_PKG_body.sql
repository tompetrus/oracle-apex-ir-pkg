CREATE OR REPLACE PACKAGE BODY APEX_IR_PKG

IS
   pa_condition_type_filter         CONSTANT VARCHAR2(30) := 'Filter';
   pa_condition_type_search         CONSTANT VARCHAR2(30) := 'Search';

   pa_lov_filter_default            CONSTANT VARCHAR2(60) := 'Default Based on Column Type';
   pa_lov_filter_named_exact        CONSTANT VARCHAR2(60) := 'Use Named List of Values to Filter Exact Match';
   pa_lov_filter_named_contains     CONSTANT VARCHAR2(60) := 'Use Named List of Values to Filter Word Contains';
   pa_lov_filter_defined_exact      CONSTANT VARCHAR2(60) := 'Use Defined List of Values to Filter Exact Match';
   pa_lov_filter_defined_contains   CONSTANT VARCHAR2(60) := 'Use Defined List of Values to Filter Word Contains';
   ------------------------------------------------------------------------------------------------
   FUNCTION is_column_lov_based
   (
      p_app_id       IN NUMBER,
      p_ir_bid       IN NUMBER,
      p_column_id    IN VARCHAR2
   )
   RETURN BOOLEAN
   IS
      v_lov_based NUMBER(1,0) := 0;
   BEGIN
      BEGIN
      SELECT CASE filter_lov_source
             WHEN pa_lov_filter_named_exact THEN 1
             ELSE 0 END
        INTO v_lov_based
        FROM apex_application_page_ir_col
       WHERE application_id = p_app_id
         AND interactive_report_id = p_ir_bid
         AND NOT display_text_as = 'HIDDEN'
         AND filter_lov_source = pa_lov_filter_named_exact
         AND column_alias = p_column_id;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
             v_lov_based := 0;
      END;

      RETURN CASE v_lov_based
             WHEN 1 THEN TRUE
             ELSE FALSE END;
   END;
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
   )
   IS
      d_col_name VARCHAR2(20);
      r_col_name VARCHAR2(20);
      c number;
      d number;
      col_cnt number;
      rec_tab DBMS_SQL.DESC_TAB;
      col_num NUMBER;
   BEGIN
      /* execute the sql to retrieve the correct column names.
         D/R/DISPLAY_VALUE/RETURN_VALUE is expected */
      c := dbms_sql.open_cursor;

      dbms_sql.parse(c, p_sql, dbms_sql.native);

      d := dbms_sql.execute(c);

      dbms_sql.describe_columns(c, col_cnt, rec_tab);

      col_num := rec_tab.first;
      IF (col_num IS NOT NULL) THEN
        LOOP
          IF rec_tab(col_num).col_name IN ('D', 'DISPLAY_VALUE')
          THEN
             d_col_name := rec_tab(col_num).col_name;
          ELSIF rec_tab(col_num).col_name IN ('R', 'RETURN_VALUE')
          THEN
             r_col_name := rec_tab(col_num).col_name;
          END IF;

          col_num := rec_tab.next(col_num);
          EXIT WHEN (col_num IS NULL);
        END LOOP;
      END IF;

      DBMS_SQL.CLOSE_CURSOR(c);

      -- If the LOV has only return values, then those are also the display value
      IF d_col_name IS NULL AND r_col_name IS NOT NULL
      THEN
         d_col_name := r_col_name;
      END IF;

      IF d_col_name IS NULL AND r_col_name IS NULL
      THEN
         -- normally lovs always have to be defined with a display and/or return column,
         -- still.
         --dbms_output.put_line('no display and return column');
         RAISE_APPLICATION_ERROR(-20010, 'The LOV query has no dispay and return columns');
      END IF;

      o_display_col := d_col_name;
      o_return_col  := r_col_name;
   END parse_sql_for_columns;
   ------------------------------------------------------------------------------------------------
   /*
   Looks for p_replace_col in p_condition, and replaces the column with a select based
   off the LOV (p_named_lov) the column is based on
   */
   FUNCTION replace_lov_column
   (
      p_app_id          IN NUMBER,
      p_named_lov       IN VARCHAR2, -- name of the LOV
      p_condition       IN VARCHAR2, -- condition to be processed
      p_replace_col     IN VARCHAR2  -- column to be replaced
   )
   RETURN VARCHAR2
   IS
      lv_lov_type       VARCHAR2(20);
      lv_lov_qry        VARCHAR2(5000);
      d_col_name        VARCHAR2(20);
      r_col_name        VARCHAR2(20);
      lv_col_repl       VARCHAR2(2000);
   BEGIN
       dbms_output.put_line('replacing '||p_replace_col||' in '||p_condition);
      IF INSTR(p_condition, p_replace_col)>0
      THEN
         --dbms_output.put_line(p_replace_col||' appears in the condition.');
         SELECT l.lov_type, l.list_of_values_query
           INTO lv_lov_type, lv_lov_qry
           FROM APEX_APPLICATION_LOVS l
          WHERE l.application_id = p_app_id
            AND l.list_of_values_name = p_named_lov;

         IF lv_lov_type = 'Static' THEN
            -- build query
            lv_lov_qry :=
               'SELECT DISPLAY_VALUE, RETURN_VALUE
                 FROM APEX_APPLICATION_LOV_ENTRIES
                WHERE application_id = '||p_app_id||
                ' AND list_of_values_name = '''||p_named_lov||'''';
         END IF;
         -- get the display and return value column names
         parse_sql_for_columns(p_sql         => lv_lov_qry,
                               o_display_col => d_col_name,
                               o_return_col  => r_col_name);

         lv_col_repl := '(SELECT '||d_col_name||
                        '   FROM ('||lv_lov_qry||')'||
                        '  WHERE '||r_col_name||' = TO_CHAR('||p_replace_col||'))';
         --dbms_output.put_line('replace column '||p_replace_col||' with '||lv_col_repl);
         RETURN REPLACE(p_condition, p_replace_col, lv_col_repl);
      END IF;
      RETURN p_condition;
   END;
   ------------------------------------------------------------------------------------------------
   /* Will parse the condition sql and search for columns which are based on
      an LOV. These columns then need to be remapped to the display value
      of those LOVs
     */
   FUNCTION get_ir_filter_lov_row
   (
      p_app_id          IN NUMBER,
      p_ir_bid          IN VARCHAR2, -- the report base id
      p_condition_sql   IN VARCHAR2  -- sql to be parsed
   )
   RETURN VARCHAR2
   IS
      lv_condition      VARCHAR2(5000);
   BEGIN
      --dbms_output.put_line('get_ir_filter_lov_row');
      --dbms_output.put_line('p_app_id: '||p_app_id);
      --dbms_output.put_line('p_ir_bid: '||p_ir_bid);
      --dbms_output.put_line('p_condition_sql: '||p_condition_sql);
      lv_condition := p_condition_sql;
      -- find columns which are based on an LOV
      FOR c IN (SELECT '"'||column_alias||'"' column_search, named_lov
                 FROM apex_application_page_ir_col
                WHERE application_id = p_app_id
                  AND interactive_report_id = p_ir_bid
                  AND NOT display_text_as = 'HIDDEN'
                  AND filter_lov_source = pa_lov_filter_named_exact)
      LOOP
         --dbms_output.put_line('column '||c.column_search||'is based on an lov');
         lv_condition := replace_lov_column(p_app_id, c.named_lov, lv_condition, c.column_search);
      END LOOP;
      --dbms_output.put_line('Returning: '||lv_condition);
      RETURN lv_condition;
   END get_ir_filter_lov_row;
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
      p_condition_operator    IN VARCHAR2,  --
      p_condition_expression1 IN VARCHAR2,  -- expression is usually the value of the search
      p_condition_expression2 IN VARCHAR2   -- expression is usually the value of the search
   )
   RETURN VARCHAR2
   IS
      lv_condition      VARCHAR2(4000);
   BEGIN
      lv_condition := replace_lov_column(p_app_id, p_named_lov, p_condition_sql, '"'||UPPER(p_condition_col_name)||'"');

      lv_condition :=
         REPLACE(REPLACE(lv_condition,
                        '#APXWS_EXPR2#',
                        ''''|| p_condition_expression2|| ''''
                        ),
                '#APXWS_EXPR#',
                CASE SUBSTR(p_condition_operator, 1, 9)
                WHEN 'is in the' THEN
                p_condition_expression1
                ELSE
                ''''|| p_condition_expression1|| ''''
                END
               );

      RETURN lv_condition;
   END get_ir_filter_lov_col;
   ------------------------------------------------------------------------------------------------
   FUNCTION get_ir_report_id
   (
      p_app_id      IN NUMBER, 
      p_page_id     IN NUMBER, 
      p_app_user    IN VARCHAR2, 
      p_session_id  IN NUMBER, 
      p_report_id   IN NUMBER
   )
   RETURN NUMBER
   IS
      lv_report_id      NUMBER(20);
      lv_pref           VARCHAR2(50);
   BEGIN
      apex_debug_message.log_message('-- GET_IR_REPORT_ID --');
      -- How the report IDs work:
      -- your base IR can be found in APEX_APPLICATION_PAGE_IR. Per page you'll find one IR here,
      -- with an interactive_report_id (the base base IR).
      -- In APEX_APPLICATION_PAGE_IR_RPT you can find the reports based off the IR in APEX_APPLICATION_PAGE_IR.
      -- There'll be a report with the primary defaults, ie the report the developer has created.
      -- The reports have an interactive_report_id identical to that of the base base IR.
      -- report_id is a unique id per IR report.
      -- When a user visits the page with the ir on it, a session-specific IR will be initialized for him = report.
      -- This ir will initially take over the values of the primary default IR. Its interactive_report_id will again
      -- be identical to that of the primary defaults report and base base IR. The report_id will refer to the report_id
      -- of the report it is based on, initially the primary defaults then.
      IF p_report_id IS NULL THEN
         BEGIN
            SELECT interactive_report_id
              INTO lv_report_id
              FROM apex_application_page_ir
             WHERE application_id = p_app_id
               AND page_id = p_page_id;

            apex_debug_message.log_message('interactive report_id: '||lv_report_id);

            --BUG: here a check should be included on app_user. There seems to be no pref for nobody (public page)
            --unsure of how to get which report is active for nobody!?
            lv_pref := apex_util.get_preference(p_preference => 'FSP_IR_'||p_app_id||'_P'||p_page_id||'_W'||lv_report_id, p_user => p_app_user);
            lv_pref := substr(lv_pref, 1, instr(lv_pref, '_')-1);
            apex_debug_message.log_message('base_report_id: '||lv_pref);

            SELECT report_id
              INTO lv_report_id
              FROM apex_application_page_ir_rpt
             WHERE application_id = p_app_id
               AND page_id = p_page_id
               AND base_report_id = lv_pref
               AND session_id = p_session_id;
         EXCEPTION
            WHEN no_data_found THEN
               apex_debug_message.log_message('no IR id could be found. Check input parameters. -> end');
               RETURN NULL;
         END;
      ELSE
         lv_report_id := p_report_id;
      END IF;
      apex_debug_message.log_message('-- GET_IR_REPORT_ID END --');
      RETURN lv_report_id;
   END;
   ------------------------------------------------------------------------------------------------
   PROCEDURE get_ir_details
   (
      p_app_id       IN NUMBER, 
      p_page_id      IN NUMBER, 
      p_report_id    IN NUMBER,
      o_ir_bid       OUT NUMBER,
      o_ir_sid       OUT NUMBER,
      o_ir_sql       OUT VARCHAR2,
      o_ir_sql_sort  OUT VARCHAR2,
      o_ir_rpt_cols  OUT VARCHAR2
   )
   IS
      lv_sql            VARCHAR2(32767);
      lv_sql_sort       VARCHAR2(4000);
      lv_ir_bid         NUMBER(20); -- this is the IR BASE ID, NOT a session specific IR ID!!
      lv_ir_sid         NUMBER(20); -- this is IR SESSION ID, NOT the BASE ID
      lv_rpt_cols       VARCHAR2(4000);
   BEGIN
      apex_debug_message.log_message('-- GET_IR_DETAILS --');
      /*Return the report query and any addition sort from the Interactive Report*/
      SELECT z.interactive_report_id,
             x.report_id,
             z.sql_query,
             nvl2(x.sort_column_1, x.sort_column_1
             || ' '
             || x.sort_direction_1
             || nvl2(x.sort_column_2, ', '
             || x.sort_column_2
             || ' '
             || x.sort_direction_2, NULL)
             || nvl2(x.sort_column_3, ', '
             || x.sort_column_3
             || ' '
             || x.sort_direction_3, NULL)
             || nvl2(x.sort_column_4, ', '
             || x.sort_column_4
             || ' '
             || x.sort_direction_4, NULL)
             || nvl2(x.sort_column_5, ', '
             || x.sort_column_5
             || ' '
             || x.sort_direction_5, NULL)
             || nvl2(x.sort_column_6, ', '
             || x.sort_column_6
             || ' '
             || x.sort_direction_6, NULL)||', rownum ', ' rownum ') sql_sort,
             x.report_columns
        INTO lv_ir_bid,
             lv_ir_sid,
             lv_sql,
             lv_sql_sort,
             lv_rpt_cols
        FROM /* The following view contains the report original report query*/
             apex_application_page_ir z,
             /*The following view provides any column sorting*/
             apex_application_page_ir_rpt x
       WHERE x.interactive_report_id = z.interactive_report_id
         AND x.application_id = p_app_id
         AND x.report_id = p_report_id
         AND x.page_id = p_page_id;

      apex_debug_message.log_message('IR base ID: '||lv_ir_bid);
      apex_debug_message.log_message('IR session ID: '||lv_ir_sid);
      apex_debug_message.log_message('IR sort: '||lv_sql_sort);
      apex_debug_message.log_message('IR columns: '||lv_rpt_cols);
      
      o_ir_bid      := lv_ir_bid;
      o_ir_sid      := lv_ir_sid;
      o_ir_sql      := lv_sql;
      o_ir_sql_sort := lv_sql_sort;
      o_ir_rpt_cols := lv_rpt_cols;
      apex_debug_message.log_message('-- GET_IR_DETAILS END -- ');
   END;
   ------------------------------------------------------------------------------------------------
   /*
   NEXT_PREV_VALUES:
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

   P_REPORT_ID
   leave this parameter empty to have the procedure automatically retrieve the
   value of the active report id. It is mainly here to provide a way to run this
   code from a sql command. Since the value is retrieved from preferences,
   and apex_util only works within an apex session, it would be necessary to
   retrieve the value manually and provide it as an input parameter. See docs.

   P_USE_SESSION_STATE // P_BINDS // P_BINDS_VAL
   p_use_session_state is by default true and means that for the final query the
   bind variables used in that query will be found and replaced by the session
   state of those bind variables. If your where-clause would look like this:
   WHERE model_id = :P80_MODEL_ID AND ...
   Then P80_MODEL_ID would be replaced by the session state for that var, using
   the v() function.
   However, using v() and session state means that this procedure would only be
   callable from an apex session. These three parameters are included so that
   you can call this procedure from (for example) the SQL Commands.
   You will then have to provide the bind variables and their values yourself.
   For example, using P80_MODEL_ID would means that you pass in a table with 1
   record in for p_binds, with 'P80_MODEL_ID', and 1 record in p_binds_val with
   the value for P80_MODEL_ID. Note that values are mapped to their variables
   by their position in the plsql table.
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
      p_cur_tot            OUT VARCHAR2  -- current of total: '4 of 132'
   )
   AS
      v_report_id       NUMBER(20);
      lv_sql            VARCHAR2(32767);
      lv_sql_sort       VARCHAR2(4000);
      lv_ir_bid         NUMBER(20); -- this is the IR BASE ID, NOT a session specific IR ID!!
      lv_ir_sid         NUMBER(20); -- this is IR SESSION ID, NOT the BASE ID
      lv_rpt_cols       VARCHAR2(4000);
      lv_condition      VARCHAR2(4000);
      lv_filters        VARCHAR2(8000);
      lv_search         VARCHAR2(4000);
      lv_searches       VARCHAR2(8000);
      lv_conditions     VARCHAR2(16000);
      lv_combined       VARCHAR2(32767);
      lv_binds          DBMS_SQL.VARCHAR2_TABLE;
      lv_pref           VARCHAR2(50);
   BEGIN
      apex_debug_message.log_message('-- GET_NAVIGATION_VALUES -- ');
      IF    p_app_id       IS NULL
         OR p_session_id   IS NULL
         OR p_column_id    IS NULL
         OR p_value        IS NULL
         OR p_page_id      IS NULL
         OR p_app_user     IS NULL
      THEN
         apex_debug_message.log_message('app_id/session_id/column_id/value/page_id : one or more of the input parameters is empty -> end');
         RETURN;
      END IF;
      
      v_report_id := get_ir_report_id(
          p_app_id      => p_app_id,
          p_page_id     => p_page_id,
          p_app_user    => p_app_user,
          p_session_id  => p_session_id,
          p_report_id   => p_report_id
      );

      apex_debug_message.log_message('report_id: '||v_report_id);

      get_ir_details(
          p_app_id       => p_app_id,
          p_page_id      => p_page_id,
          p_report_id    => v_report_id,
          o_ir_bid       => lv_ir_bid,
          o_ir_sid       => lv_ir_sid,
          o_ir_sql       => lv_sql,
          o_ir_sql_sort  => lv_sql_sort,
          o_ir_rpt_cols  => lv_rpt_cols
      ); 
      
      lv_sql := get_ir_sql(
          p_app_id             => p_app_id,
          p_session_id         => p_session_id,
          p_page_id            => p_page_id,
          p_report_id          => v_report_id,
          p_app_user           => p_app_user,
          p_use_session_state  => p_use_session_state,
          p_binds              => p_binds,
          p_binds_val          => p_binds_val,
          p_all_columns        => TRUE,
          p_incl_filters       => TRUE,
          p_incl_order_by      => FALSE
      );

      -- combine the filters and searches into the final sql
      lv_combined :=
                 'select prev, next, top, bot, cur||'' of '' ||tot
                    from ( select ' ||p_column_id || '
                                  ,         lag (' || p_column_id || ') over (order by ' || lv_sql_sort ||') prev
                                  ,        lead (' || p_column_id || ') over (order by ' || lv_sql_sort ||') next
                                  , first_value (' || p_column_id || ') over (order by ' || lv_sql_sort ||') top
                                  ,  last_value (' || p_column_id || ') over (order by ' || lv_sql_sort ||
                                                        ' RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) bot
                                  ,       count (' || p_column_id ||') over (order by ' || lv_sql_sort || ') cur
                                  ,       count (' || p_column_id ||') over () tot
                             from (' || lv_sql || '))
                   where ' || p_column_id || ' = ' || p_value;
      
      apex_debug_message.log_message('Combined SQL: '||lv_combined);

      EXECUTE IMMEDIATE lv_combined
                   INTO p_prev,
                        p_next,
                        p_top,
                        p_bot,
                        p_cur_tot;

      p_cur_tot := LOWER(p_cur_tot);

      IF p_prev IS NULL THEN
         p_top := NULL;
      END IF;

      IF p_next IS NULL THEN
         p_bot := NULL;
      END IF;

      apex_debug_message.log_message('Fetched values:');
      apex_debug_message.log_message('previous: '||p_prev);
      apex_debug_message.log_message('next: '    ||p_next);
      apex_debug_message.log_message('top: '     ||p_top);
      apex_debug_message.log_message('bot: '     ||p_bot);
      apex_debug_message.log_message('current of total: '||p_cur_tot);

      apex_debug_message.log_message('-- GET_NAVIGATION_VALUES END -- ');
   END get_navigation_values;
   ------------------------------------------------------------------------------------------------
   FUNCTION get_column_ac_values
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_column_id          IN  VARCHAR2, -- the column for which to get the next/prev vals
      p_value              IN  VARCHAR2, -- the current search value for p_column_id IF NULL THEN ALL
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_report_id          IN  NUMBER,   -- id of the selected IR, this can be null
      p_app_user           IN  VARCHAR2, -- app user (APP_USER)
      p_use_session_state  IN  BOOLEAN DEFAULT TRUE, -- true for using apex session state bind vars. If False p_binds+vals are to be filled.
      p_binds              IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables
      p_binds_val          IN  DBMS_SQL.VARCHAR2_TABLE, -- plsql table with bind variables VALUES
      p_search_behaviour   IN  VARCHAR2 DEFAULT 'CONTAINS', -- LIKE, CONTAINS: how results are fetched
      p_filter_behaviour   IN  VARCHAR2 DEFAULT 'FILTER' -- FILTER, ALL: filtering of results
   )
   RETURN CLOB
   IS
      lv_ir_bid         NUMBER(20); -- this is the IR BASE ID, NOT a session specific IR ID!!
      lv_sql            VARCHAR2(32767);     
      lv_combined       VARCHAR2(32767);
      lv_results        CLOB;
      lv_result         VARCHAR2(1000);
      TYPE lt_IR        IS REF CURSOR;
      lv_ir_cursor      lt_IR;
      lv_column_slct    VARCHAR2(2000);
      l_crlf            CHAR(2) := CHR(13)||CHR(10);
   BEGIN
      IF    p_app_id       IS NULL
         OR p_session_id   IS NULL
         OR p_column_id    IS NULL
         OR p_page_id      IS NULL
      THEN
         RETURN NULL;
      END IF;

      lv_sql := get_ir_sql(
          p_app_id             => p_app_id,
          p_session_id         => p_session_id,
          p_page_id            => p_page_id,
          p_report_id          => p_report_id,
          p_app_user           => p_app_user,
          p_use_session_state  => p_use_session_state,
          p_binds              => p_binds,
          p_binds_val          => p_binds_val,
          p_all_columns        => FALSE,
          p_incl_filters       => TRUE,
          p_incl_order_by      => FALSE
      );
      
      -- this select wont fail nor does it need to be session specific
      -- columns are those derived from the base ir, and their details 
      -- are not session specific
      SELECT interactive_report_id
        INTO lv_ir_bid
        FROM apex_application_page_ir
       WHERE application_id = p_app_id
         AND page_id = p_page_id;

      IF is_column_lov_based(p_app_id, lv_ir_bid, p_column_id) THEN
         lv_column_slct := get_ir_filter_lov_row(p_app_id, lv_ir_bid, '"'||p_column_id||'"');
      ELSE
         lv_column_slct := p_column_id;
      END IF;

      -- replacing newlines in values, and doublequotes: these characters mess up the JSON parse
      -- combine the filters and searches into the final sql
      lv_combined :=
                 'select ''"''||sys.htf.escape_sc(REPLACE(REPLACE('|| lv_column_slct ||',''"'',''''), CHR(13)||CHR(10), ''''))||''"''' ||
                 '  from ( select distinct ' ||p_column_id ||
                 '           from ('||lv_sql||')'||
                 '          order by 1)'||
                 '  where '||CASE WHEN p_value IS NULL THEN '' ELSE
                               CASE p_search_behaviour
                                  WHEN 'CONTAINS' THEN
                                     'INSTR(UPPER('||lv_column_slct||'), UPPER('''||p_value||'''))>0'
                                  WHEN 'LIKE' THEN
                                     'UPPER('||lv_column_slct||') LIKE UPPER('''||p_value||''')'
                               END ||' and'
                            END||
                 ' rownum < 500';

      -- IT MAY BE NECESSARY TO REPLACE BINDS IN THE TOTAL SELECT!
      -- However, it seems very unlikely. The base SQL will be handled, and the
      -- only place this may occur is in an LOV query for the filtered column

      OPEN lv_ir_cursor FOR lv_combined;
      LOOP
         FETCH lv_ir_cursor INTO lv_result;
         EXIT WHEN lv_ir_cursor%NOTFOUND;
         lv_results := lv_results ||','||lv_result;
      END LOOP;
      CLOSE lv_ir_cursor;

      --dbms_output.put_line('lv_results: '||lv_results);
      lv_results := '['|| LTRIM(lv_results, ',') ||']';

      --dbms_output.put_line('-- GET_COLUMN_AC_VALUES -- ');
      RETURN lv_results;
   END get_column_ac_values;
   ------------------------------------------------------------------------------------------------
   FUNCTION get_existing_ir_filter
   (
      p_app_id             IN  NUMBER,   -- application id (APP_ID)
      p_session_id         IN  NUMBER,   -- session id (APP_SESSION)
      p_page_id            IN  NUMBER,   -- Page number of the interactive report
      p_app_user           IN  VARCHAR2, -- user (APP_USER)
      p_column_name        IN  VARCHAR2  -- column for which to retrieve filter
   )
   RETURN VARCHAR2
   IS
      v_report_id    NUMBER;
      lv_pref        VARCHAR2(50);
      lv_filter_id   NUMBER;
   BEGIN
      BEGIN
         SELECT interactive_report_id
           INTO v_report_id
           FROM apex_application_page_ir
          WHERE application_id = p_app_id
            AND page_id = p_page_id;

         lv_pref := apex_util.get_preference(p_preference => 'FSP_IR_'||p_app_id||'_P'||p_page_id||'_W'||v_report_id, p_user => p_app_user);
         lv_pref := substr(lv_pref, 1, instr(lv_pref, '_')-1);

         SELECT report_id
           INTO v_report_id
           FROM apex_application_page_ir_rpt
          WHERE application_id = p_app_id
            AND page_id = p_page_id
            AND base_report_id = lv_pref
            AND session_id = p_session_id;
      EXCEPTION
         WHEN no_data_found THEN
            RETURN 'ADD';
      END;

      BEGIN
         SELECT condition_id
           INTO lv_filter_id
           FROM apex_application_page_ir_cond
          WHERE application_id = p_app_id
            AND page_id = p_page_id
            AND report_id = v_report_id
            AND condition_column_name = p_column_name
            AND condition_type = 'Filter'
            AND condition_operator = 'between';

         RETURN 'UPDATE:'||lv_filter_id;
      EXCEPTION
      WHEN no_data_found OR too_many_rows THEN
         RETURN 'ADD';
      END;
   END get_existing_ir_filter;
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
      p_all_columns        IN  BOOLEAN DEFAULT FALSE, -- select * from ir sql or only selected columns
      p_incl_filters       IN  BOOLEAN DEFAULT TRUE, --whether to include applied filters or not
      p_incl_order_by      IN  BOOLEAN DEFAULT TRUE  --whether to include the order by or not
   )
   RETURN VARCHAR2
   AS
      v_report_id       NUMBER(20);
      lv_sql            VARCHAR2(32767);
      lv_sql_sort       VARCHAR2(4000);
      lv_ir_bid         NUMBER(20); -- this is the IR BASE ID, NOT a session specific IR ID!!
      lv_ir_sid         NUMBER(20); -- this is IR SESSION ID, NOT the BASE ID
      lv_rpt_cols       VARCHAR2(4000);
      lv_condition      VARCHAR2(4000);
      lv_filters        VARCHAR2(8000);
      lv_search         VARCHAR2(4000);
      lv_searches       VARCHAR2(8000);
      lv_conditions     VARCHAR2(16000);
      lv_combined       VARCHAR2(32767);
      lv_binds          DBMS_SQL.VARCHAR2_TABLE;
      lv_pref           VARCHAR2(50);
      lv_bind_val       VARCHAR2(200);
   BEGIN
      apex_debug_message.log_message('-- GET_IR_SQL -- ');
      
      apex_debug_message.log_message('p_app_id: '    ||p_app_id);
      apex_debug_message.log_message('p_session_id: '||p_session_id);
      apex_debug_message.log_message('p_page_id: '   ||p_page_id);
      apex_debug_message.log_message('p_app_user: '   ||p_app_user);
      
      IF    p_app_id       IS NULL
         OR p_session_id   IS NULL
         OR p_page_id      IS NULL
         OR p_app_user     IS NULL
      THEN
         apex_debug_message.log_message('app_id/session_id/page_id : one or more of the input parameters is empty -> end');
         RETURN NULL;
      END IF;

      v_report_id := get_ir_report_id(
          p_app_id      => p_app_id,
          p_page_id     => p_page_id,
          p_app_user    => p_app_user,
          p_session_id  => p_session_id,
          p_report_id   => p_report_id
      );

      apex_debug_message.log_message('report_id: '||v_report_id);

      get_ir_details(
          p_app_id       => p_app_id,
          p_page_id      => p_page_id,
          p_report_id    => v_report_id,
          o_ir_bid       => lv_ir_bid,
          o_ir_sid       => lv_ir_sid,
          o_ir_sql       => lv_sql,
          o_ir_sql_sort  => lv_sql_sort,
          o_ir_rpt_cols  => lv_rpt_cols
      );      
      
      IF p_incl_filters THEN
        /*FILTER TYPES ON COLUMNS ONLY IN THIS LOOP!
          SEARCH type is on each column in the report!
  
          Filters: each filter is AND
          Search: each search is AND, but the expression on each column is an OR
        */
        apex_debug_message.log_message('* Processing Filters on Columns *');
        FOR cc IN (SELECT c.condition_type,
                          c.condition_column_name,
                          c.condition_expression,
                          c.condition_expression2,
                          c.condition_operator,
                          c.condition_sql,
                          col.column_type,
                          col.filter_lov_source,
                          col.named_lov,
                          col.rpt_lov
                     FROM APEX_APPLICATION_PAGE_IR_COND c
                     JOIN APEX_APPLICATION_PAGE_IR_COL col
                       ON c.condition_column_name = col.column_alias
                      AND c.interactive_report_id = col.interactive_report_id
                    WHERE c.interactive_report_id = lv_ir_bid -- BASE REPORT ID
                      AND c.report_id = lv_ir_sid -- SESSION REPORT ID
                      AND c.condition_enabled = 'Yes'
                   )
        LOOP
           -----------------------------------------------------
           -- Only named lists are allowed for display types of
           --          'Display as Text(based on LOV, escape special characters)
           -- so, only those are being parsed.
           -- Apex 4.1.0.00.32
           -----------------------------------------------------
           apex_debug_message.log_message('filter on column '||cc.condition_column_name);
           IF cc.filter_lov_source = pa_lov_filter_named_exact
           THEN
              apex_debug_message.log_message('column is based on an LOV');
  
              lv_condition := get_ir_filter_lov_col(p_app_id                => p_app_id,
                                                    p_named_lov             => cc.named_lov,
                                                    p_condition_col_name    => cc.condition_column_name,
                                                    p_condition_sql         => cc.condition_sql,
                                                    p_condition_operator    => cc.condition_operator,
                                                    p_condition_expression1 => cc.condition_expression,
                                                    p_condition_expression2 => cc.condition_expression2);
           ELSIF cc.filter_lov_source = pa_lov_filter_default
           THEN
              apex_debug_message.log_message('regular column');
              IF cc.condition_type = pa_condition_type_filter THEN
                 lv_condition := REPLACE(REPLACE(cc.condition_sql, '#APXWS_EXPR2#', ''''|| cc.condition_expression2|| ''''),
                                         '#APXWS_EXPR#', CASE SUBSTR(cc.condition_operator, 1, 9)
                                                         WHEN 'is in the' THEN cc.condition_expression
                                                         ELSE ''''||cc.condition_expression||''''
                                                         END);
  
  
              ELSIF cc.condition_type = pa_condition_type_search THEN
                 apex_debug_message.log_message('column filter of SEARCH type (column contains)');
                 lv_condition := 'INSTR(UPPER("' || cc.condition_column_name || '"),UPPER(''' || cc.condition_expression ||'''))>0 ';
              END IF;
           END IF;
           apex_debug_message.log_message('parsed condition: '||lv_condition);
  
           lv_filters := lv_filters || 'AND ' || lv_condition || ' ';
        END LOOP;
        apex_debug_message.log_message('All column filters processed');
        apex_debug_message.log_message('conditions aggregated: '||lv_filters);
        apex_debug_message.log_message('* Finished processing column filters *');
  
        /* Filters of the ROW type!
        */
        apex_debug_message.log_message('* Processing Filters on Rows *');
        FOR cc IN (SELECT condition_sql
                     FROM APEX_APPLICATION_PAGE_IR_COND
                    WHERE interactive_report_id = lv_ir_bid -- BASE REPORT ID
                      AND report_id = lv_ir_sid -- SESSION REPORT ID
                      AND condition_type = pa_condition_type_filter
                      AND condition_expr_type = 'ROW'
                      AND condition_enabled = 'Yes'
                   )
        LOOP
           lv_condition := get_ir_filter_lov_row(p_app_id        => p_app_id,
                                                 p_ir_bid        => lv_ir_bid,
                                                 p_condition_sql => cc.condition_sql);
           lv_filters := lv_filters || 'AND ' || lv_condition || ' ';
        END LOOP;
  
        IF lv_filters IS NOT NULL THEN
           lv_filters := ' where ' || LTRIM(lv_filters, 'AND');
        END IF;
  
        apex_debug_message.log_message('All row filters processed');
        apex_debug_message.log_message('conditions aggregated: '||lv_filters);
        apex_debug_message.log_message('* Finished processing row filters *');
    
        /*SEARCH TYPES ONLY IN THIS LOOP
          For each Search condition a filter needs to be applied to each column in the query
          date columns excluded
          lv_condition: a condition on one column, part of a search (instr)
          lv_search: the aggregate of the conditions on each column, for one search (instr or instr or instr...)
          lv_searches: the aggregate of the all the search filters (and (instr or instr or ...) and (instr or instr or ...) ...
          */
        apex_debug_message.log_message('* Processing Search Conditions *');
        FOR cond IN (SELECT c.condition_type,
                            c.condition_column_name,
                            c.condition_expression
                       FROM APEX_APPLICATION_PAGE_IR_COND c
                      WHERE c.interactive_report_id = lv_ir_bid -- THIS IS THE BASE REPORT ID
                        AND c.report_id = lv_ir_sid
                        AND c.condition_type = pa_condition_type_search
                        AND c.condition_enabled = 'Yes')
        LOOP
           /*
           date columns are excluded
           only columns selected for display are searched through a search type condition
           for these columns, take into account lov based columns
           */
           apex_debug_message.log_message('parsing: '||cond.condition_expression);
  
           FOR c IN ( SELECT b.column_alias, b.column_type, b.filter_lov_source, b.named_lov
                       FROM (SELECT REGEXP_SUBSTR (str, '[^:]+', 1, LEVEL) rpt_col
                               FROM (SELECT lv_rpt_cols str
                                       FROM DUAL)
                            CONNECT BY LEVEL <= length (regexp_replace (str, '[^:]+'))  + 1
                           ) a
                      JOIN apex_application_page_ir_col b
                        ON b.interactive_report_id = lv_ir_bid -- THIS IS THE BASE REPORT ID
                       AND a.rpt_col = b.column_alias
                     WHERE column_type != 'DATE')
           LOOP
              apex_debug_message.log_message('column '||c.column_alias);
              IF c.filter_lov_source = pa_lov_filter_named_exact
              THEN
                 apex_debug_message.log_message('column is based on an LOV');
                 lv_condition := get_ir_filter_lov_col(p_app_id                => p_app_id,
                                                       p_named_lov             => c.named_lov,
                                                       p_condition_col_name    => c.column_alias,
                                                       p_condition_sql         => 'INSTR(UPPER("' || c.column_alias || '"),UPPER(''' || cond.condition_expression ||'''))>0',
                                                       p_condition_operator    => NULL,
                                                       p_condition_expression1 => cond.condition_expression,
                                                       p_condition_expression2 => NULL);
              ELSIF c.filter_lov_source = pa_lov_filter_default
              THEN
                 apex_debug_message.log_message('regular column');
                 lv_condition := 'INSTR(UPPER("' || c.column_alias || '"),UPPER(''' || cond.condition_expression ||'''))>0';
              END IF;
  
              --aggregate the conditions
              lv_search := lv_search || 'OR ' || lv_condition || ' ';
           END LOOP;
           apex_debug_message.log_message('aggregated search: '||lv_search);
  
           lv_search := LTRIM(lv_search, 'OR');
           lv_searches := lv_searches||'AND ('||lv_search || ') ';
        END LOOP;
  
        IF lv_searches IS NOT NULL THEN
           lv_searches := ' where ' || LTRIM(lv_searches, 'AND');
        END IF;
  
        apex_debug_message.log_message('All searches parsed');
        apex_debug_message.log_message('searches aggregated: '||lv_searches);
        apex_debug_message.log_message('* Finished processing Search conditions');
      END IF;

      -- create base sql      
      IF p_all_columns THEN
         apex_debug_message.log_message('* Selecting all columns (*)');
         lv_combined := 'select * from (' || lv_sql || ')';
      ELSE
         apex_debug_message.log_message('*  Only selecting selected column');
         lv_combined := 'select '||REPLACE(RTRIM(lv_rpt_cols,':'), ':',',')||' from (' || lv_sql || ')';
      END IF;
      
      -- combine the filters and searches into the final sql
      IF p_incl_filters THEN
         apex_debug_message.log_message('* Appending filters');
         lv_combined := lv_combined||lv_searches||lv_filters;
      END IF;
      
      -- combine with order by
      IF p_incl_order_by THEN
         apex_debug_message.log_message('* Appending order by clause');
         lv_combined := lv_combined||' ORDER BY '||lv_sql_sort;
      END IF;

      -- get the bind variables
      apex_debug_message.log_message('Process bind variables');
      IF p_use_session_state THEN
         dbms_output.put_line('No binds as input, collecting binds from sql and populate through apex session state');
         lv_binds := wwv_flow_utilities.get_binds(lv_combined);

         FOR i IN 1..lv_binds.COUNT
         LOOP
            dbms_output.put_line('bind var: '||lv_binds(i)||' replaced by '||v(LTRIM(UPPER(lv_binds(i)), ':')));
            --deal with bind var values. Note that this will currently put numbers in varchar2 aswell.
            --bind vars are by default parsed as varchar though, so this might not impact performance too much.
            lv_bind_val := v(LTRIM(UPPER(lv_binds(i)), ':'));
            IF lv_bind_val IS NULL THEN
              lv_bind_val := 'NULL';
            ELSE
              lv_bind_val := ''''||lv_bind_val||'''';
            END IF;
            
            lv_combined := regexp_replace(lv_combined, UPPER(lv_binds(i)), lv_bind_val, 1, 0, 'i');
         END LOOP;
      ELSE
         dbms_output.put_line('Binds have been given as input, populate through input values');
         FOR i IN 1..p_binds.COUNT
         LOOP
            dbms_output.put_line('bind var: :'||LTRIM(p_binds(i), ':')||' replaced by '||p_binds_val(i));
            --unsure if bind var checking should happen here.
            lv_combined := regexp_replace(lv_combined, ':' || UPPER(LTRIM(p_binds(i), ':')), NVL(p_binds_val(i), 'NULL'), 1, 0, 'i');
         END LOOP;
      END IF;

      apex_debug_message.log_message('The combined statement: '||lv_combined);

      apex_debug_message.log_message('-- GET_IR_SQL -- ');
      RETURN lv_combined;
   END get_ir_sql;
   ------------------------------------------------------------------------------------------------
END apex_ir_pkg;

/
