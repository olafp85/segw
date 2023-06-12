REPORT zsegw.

CLASS main DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    TYPES:
      tv_icon TYPE c LENGTH 8,

      BEGIN OF ts_operation,
        project TYPE c LENGTH 35,
        type    TYPE c LENGTH 8,
        name    TYPE c LENGTH 35,
        create  TYPE tv_icon,
        read    TYPE tv_icon,
        update  TYPE tv_icon,
        delete  TYPE tv_icon,
        query   TYPE tv_icon,
        "Technical
        service TYPE REF TO /iwbep/if_sbdm_service,
        class   TYPE seoclsname,
        methods TYPE name2value_table,
      END OF ts_operation.

    DATA:
      alv        TYPE REF TO cl_salv_table,
      operations TYPE STANDARD TABLE OF ts_operation WITH EMPTY KEY.

    METHODS:
      show_result,
      on_before_salv_function
        FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_double_click
        FOR EVENT double_click OF cl_salv_events_table IMPORTING row,
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.

ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD run.

    TYPES:
      BEGIN OF ts_entity_set,
        entity_set  TYPE /iwbep/sbdm_node_name,
        entity_type TYPE /iwbep/sbdm_node_name,
        operations  TYPE /iwbep/if_sbdsp_operation=>ty_t_operations,
      END OF ts_entity_set.

    DATA:
      entity_sets TYPE HASHED TABLE OF ts_entity_set WITH UNIQUE KEY entity_type.

    TRY.
        "Current projects (as in SEGW)
        DATA(favorites) = /iwbep/cl_sbdm_transact_handlr=>go_instance->mo_user_project_manager.
        DATA(projects) = favorites->open_projects( ).

        LOOP AT projects INTO DATA(project).  "/iwbep/if_sbdm_project
          "Node: Service Implementation
          DATA(services) = project->get_children( is_node_type = /iwbep/if_sbdm_service=>gc_node_type ).
          DATA(service) = CAST /iwbep/if_sbdm_service( services[ 1 ] ).

          "Entity Sets » Entity Types and Operations
          entity_sets = VALUE #( FOR set IN services[ 1 ]->get_children( is_node_type = /iwbep/if_sbdsp_entity_set=>gc_node_type )
                                 ( entity_set  = set->get_name( )
                                   entity_type = CAST /iwbep/if_sbdsp_entity_set( set )->get_odata_entity_set( )->get_entity_type( )->get_name( )
                                   operations  = CAST /iwbep/if_sbdsp_entity_set( set )->get_operations( ) ) ) .

          "Node: Runtime Artifacts
          DATA(class) = VALUE sobj_name( ).
          LOOP AT project->get_generated_artifacts( ) INTO DATA(artifact).
            IF artifact->get_type( ) = 'DPCS'.  "/iwbep/cl_sbdm_gen_artifact=>gc_class_type-dpc (private)
              class = artifact->get_tadir_data( )-trobj_name.
              EXIT.
            ENDIF.
          ENDLOOP.

          "Node: Data Model » Entity Types
          DATA(model) = project->get_children( is_node_type = /iwbep/if_sbdm_model=>gc_node_type ).
          DATA(entity_types) = model[ 1 ]->get_children( is_node_type = /iwbep/if_sbod_entity_type=>gc_node_type ).
          LOOP AT entity_types INTO DATA(entity_type).
            DATA(wa) = VALUE ts_operation(
              project = project->get_name( )
              type    = 'Entity'
              service = service
              name    = entity_type->get_name( )
              class   = class ).

            "Operations
            DATA(operations) = VALUE #( entity_sets[ entity_type = entity_type->get_name( ) ]-operations OPTIONAL ).
            LOOP AT operations INTO DATA(operation).  "/iwbep/if_sbdsp_operation
              DATA(name) = operation->/iwbep/if_sbdm_node~get_name( ).
              DATA(method) = operation->get_imp_method( ).

              "Check for mapping to a search help
              DATA(data_source) = COND #( LET mapping = operation->get_mapping( ) IN WHEN mapping IS NOT INITIAL THEN mapping[ 1 ]->get_data_source( ) ).
              DATA(is_search_help) = COND #( WHEN data_source IS BOUND AND data_source->get_data_source_type( ) = /iwbep/cl_sb_gen_dpc_plugin=>gc_ds_type-shlp THEN abap_true ).

              "Check for custom implementation
              DATA(is_redefined) = /iwbep/cl_sbui_dp_se24_navig_u=>check_method_redefinition( iv_class_name  = VALUE #( clsname = wa-class )
                                                                                          iv_method_name = method ).

              IF is_redefined = abap_true OR
                 is_search_help = abap_true.
                DATA(column) = SWITCH #( name WHEN 'Create'               THEN 'CREATE'
                                              WHEN 'GetEntity (Read)'     THEN 'READ'
                                              WHEN 'Update'               THEN 'UPDATE'
                                              WHEN 'Delete'               THEN 'DELETE'
                                              WHEN 'GetEntitySet (Query)' THEN 'QUERY' ).
                ASSIGN COMPONENT column OF STRUCTURE wa TO FIELD-SYMBOL(<field>).
                <field> = icon_okay.
                APPEND VALUE #( name  = column
                                value = method ) TO wa-methods.
              ENDIF.
            ENDLOOP.
            APPEND wa TO me->operations.
            CLEAR wa.
          ENDLOOP.

          "Node: Data Model » Function Imports
          DATA(functions) = model[ 1 ]->get_children( is_node_type = /iwbep/if_sbod_func_imp=>gc_node_type ).
          LOOP AT functions INTO DATA(function).
            APPEND VALUE #(
              project = project->get_name( )
              type    = 'Function'
              name    = function->get_name( )
              service = service
              class   = class
              methods = VALUE #( ( value = '/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION' ) )
              ) TO me->operations.
          ENDLOOP.

          "Other implemented methods
          DATA(interface) = '/IWBEP/IF_MGW_APPL_SRV_RUNTIME~'.
          LOOP AT CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( class ) )->methods INTO DATA(meth)
            WHERE name CS interface
              AND name <> interface && 'EXECUTE_ACTION'
              AND is_redefined = abap_true.
            APPEND VALUE #(
              project = project->get_name( )
              type    = 'Other'
              name    = to_mixed( substring_after( val = meth-name sub = interface ) )
              service = service
              class   = class
              methods = VALUE #( ( value = meth-name ) )
              ) TO me->operations.
          ENDLOOP.

        ENDLOOP.

      CATCH /iwbep/cx_sbcm_exception INTO DATA(exc).
        MESSAGE exc TYPE 'E'.
    ENDTRY.

    show_result( ).

  ENDMETHOD.

  METHOD show_result.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(alv)
          CHANGING  t_table      = me->operations ).
        me->alv = alv.  "tbv. event handler

        alv->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
        alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
        alv->get_layout( )->set_default( abap_true ).

        alv->get_functions( )->set_default( ).
        alv->get_functions( )->set_print_preview( ).     "Cleanup cache
        alv->get_functions( )->set_export_localfile( ).  "SEGW

        DATA(structure) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( me->operations ) )->get_table_line_type( ).
        DATA(components) = CAST cl_abap_structdescr( structure )->components.

        LOOP AT components INTO DATA(component) WHERE NOT ( name = 'SERVICE' OR name = 'METHODS' ).
          DATA(index) = sy-tabix.
          DATA(column) = CAST cl_salv_column_table( alv->get_columns( )->get_column( component-name ) ).
          column->set_medium_text( to_mixed( component-name ) ).

          IF index = 1.
            column->set_key( ).
          ENDIF.

          IF index > 3.
            column->set_icon( ).
            column->set_cell_type( if_salv_c_cell_type=>hotspot ).
          ENDIF.

          IF component-name = 'CLASS'.
            column->set_technical( ).
          ENDIF.
        ENDLOOP.

        alv->get_sorts( )->add_sort( 'PROJECT' ).
        alv->get_sorts( )->add_sort( 'TYPE' ).
        alv->get_sorts( )->add_sort( 'NAME' ).

        alv->get_display_settings( )->set_striped_pattern( abap_true ).

        SET HANDLER on_before_salv_function FOR alv->get_event( ).
        SET HANDLER on_double_click FOR alv->get_event( ).
        SET HANDLER on_link_click FOR alv->get_event( ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(exc).
        MESSAGE exc TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD on_before_salv_function.

    "Kaap enkele standaard functies...

    CASE e_salv_function.
      WHEN '%PC'.  "Local File...
        CALL TRANSACTION 'SEGW'.

        "Voorkom de standaard afhandeling
        MESSAGE '' TYPE 'E'.

      WHEN '&RNT_PREV'.  "Print Preview
        DATA(cell) = me->alv->get_selections( )->get_current_cell( ).
        DATA(service) = VALUE #( me->operations[ cell-row ]-service OPTIONAL ).
        IF service IS INITIAL.
          RETURN.
        ENDIF.

        "Clear de cache zoals in de SAP Gateway Client via menu Metadata » Cleanup Cache
        /iwfnd/cl_sutil_moni=>cleanup_metadata_cache(
          EXPORTING
            iv_mode            = 'A'  "All
            iv_multi_origin    = abap_true
            iv_namespace       = '/SAP/'
            iv_service_name    = service->get_service_name( )
            iv_service_version = service->get_service_version( )
          IMPORTING
            ev_error_text      = DATA(error) ).

        "Clear proxy model data
        /iwbep/cl_cp_pm_cache_dba=>delete_all_model_data( ).

        IF error IS INITIAL.
          MESSAGE |Metadata cache deleted for service { service->get_service_name( ) }| TYPE 'E' DISPLAY LIKE 'S'.
        ELSE.
          MESSAGE error TYPE 'E'.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD on_double_click.

    DATA(operation) = operations[ row ].
    DATA(method) = COND #( WHEN lines( operation-methods ) = 1 THEN operation-methods[ 1 ]-value ).

    TRY.
        IF method IS INITIAL.
          /iwbep/cl_sbui_dp_se24_navig_u=>navigate_to_class( CONV #( operation-class ) ).
        ELSE.
          /iwbep/cl_sbui_dp_se24_navig_u=>navigate_to_method( iv_method_name      = CONV #( method )
                                                              iv_enclosing_object = CONV #( operation-class ) ).
        ENDIF.

      CATCH /iwbep/cx_sbcm_exception INTO DATA(exc).
        MESSAGE exc TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD on_link_click.

    DATA(operation) = operations[ row ].
    DATA(method) = VALUE #( operation-methods[ name = column ]-value OPTIONAL ).

    IF method IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        /iwbep/cl_sbui_dp_se24_navig_u=>navigate_to_method( iv_method_name      = CONV #( method )
                                                            iv_enclosing_object = CONV #( operation-class ) ).

      CATCH /iwbep/cx_sbcm_exception INTO DATA(exc).
        MESSAGE exc TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW main( )->run( ).
