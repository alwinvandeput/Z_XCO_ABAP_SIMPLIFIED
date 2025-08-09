CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_minimal
      "FOR TESTING
      RAISING cx_static_check.

    METHODS create_full_draft
      FOR TESTING
      RAISING cx_static_check.


ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create_minimal.

    DATA(ls_create) = VALUE z_xco_cds_behavior_definition=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      behavior_definition = VALUE #(
        name                      = 'ZSDR_SLSORDERTP2'
        description               = 'Unit Test Behavior Definition'
        implementation_type       = xco_cp_behavior_definition=>implementation_type->managed
        implementation_class_name = 'ZBP_SDR_SLSORDERTP2'
        strict_mode               = zdmo_cl_rap_node=>strict_mode_2

        behaviors = VALUE #(
          ( cds_entity_name              = 'ZSDR_SLSORDERTP2'
            alias_name                   = 'SalesOrder'
            persistent_db_table_name     = 'ZSDSLSORDER000TP'
            implementation_class_name    = ''
            extensible                   = abap_false
            etag_master_field_name       = 'LocalLastChangedAt'
            lock_master                  = abap_true
            authorization_master_global  = abap_true

            fields = VALUE #(
              ( name = 'Uuid'
                numbering_managed = abap_true
                read_only = abap_true )
            )

            standard_operations = VALUE #(
              ( xco_cp_behavior_definition=>standard_operation->create )
              ( xco_cp_behavior_definition=>standard_operation->update )
              ( xco_cp_behavior_definition=>standard_operation->delete )
            )

            mapping = VALUE #(
              database_table_name = 'ZSDSLSORDER000TP'
              corresponding = abap_true
              field_mappings = VALUE #(
                ( cds_view_field = 'Uuid'
                  dbtable_field  = 'uuid' )
                ( cds_view_field = 'SalesOrderNo'
                  dbtable_field  = 'sales_order_no' )
                ( cds_view_field = 'SalesOrderType'
                  dbtable_field  = 'sales_order_type' )
                ( cds_view_field = 'SalesOrderTypeDescr'
                  dbtable_field  = 'sales_order_type_descr' )
                ( cds_view_field = 'Description'
                  dbtable_field  = 'description' )
                ( cds_view_field = 'LocalCreatedBy'
                  dbtable_field  = 'local_created_by' )
                ( cds_view_field = 'LocalCreatedAt'
                  dbtable_field  = 'local_created_at' )
                ( cds_view_field = 'LocalLastChangedBy'
                  dbtable_field  = 'local_last_changed_by' )
                ( cds_view_field = 'LocalLastChangedAt'
                  dbtable_field  = 'local_last_changed_at' )
                ( cds_view_field = 'LastChangedAt'
                  dbtable_field  = 'last_changed_at' )
              )
            )

          )
        )
      )
    ).

    "Create
    DATA(lo_create_behavior_definition) = z_xco_cds_behavior_definition=>create_or_update_instance( ls_create ).

    cl_abap_unit_assert=>assert_not_initial( lo_create_behavior_definition ).

  ENDMETHOD.


  METHOD create_full_draft.

    DATA(ls_create) = VALUE z_xco_cds_behavior_definition=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      behavior_definition = VALUE #(
        name                      = 'ZSDR_SlsOrderTP3'
        description               = 'Unit Test Behavior Definition'
        implementation_type       = xco_cp_behavior_definition=>implementation_type->managed
        implementation_class_name = 'zbp_sdr_slsordertp3'
        strict_mode               = zdmo_cl_rap_node=>strict_mode_2
        draft_enabled             = abap_true
        extensible                = abap_false

        behaviors = VALUE #(
          ( cds_entity_name              = 'ZSDR_SlsOrderTP3'
            alias_name                   = 'SalesOrder'
            persistent_db_table_name     = 'zsdslsorder000tp'
            implementation_class_name    = ''
            extensible                   = abap_false
            draft_table_name             = 'zsdslsorder_dtp'
            etag_master_field_name       = 'LocalLastChangedAt'
            lock_master                  = abap_false
            lock_master_total_etag_field = 'LastChangedAt'
            authorization_master_global  = abap_true

            fields = VALUE #(
              ( name = 'Uuid'
                numbering_managed = abap_true
                read_only = abap_true )

              ( name = 'LocalCreatedBy'
                read_only = abap_true )
              ( name = 'LocalCreatedAt'
                read_only = abap_true )
              ( name = 'LocalLastChangedBy'
                read_only = abap_true )
              ( name = 'LocalLastChangedAt'
                read_only = abap_true )
              ( name = 'LastChangedAt'
                read_only = abap_true )
            )

            standard_operations = VALUE #(
              ( xco_cp_behavior_definition=>standard_operation->create )
              ( xco_cp_behavior_definition=>standard_operation->update )
              ( xco_cp_behavior_definition=>standard_operation->delete )
            )

            actions = VALUE #(

              "Draft Actions
              ( name = 'Activate'
                draft = abap_true
                optimized = abap_true )
              ( name = 'Discard'
                draft = abap_true )
              ( name = 'Edit'
                draft = abap_true )
              ( name = 'Resume'
                draft = abap_true )
              ( name = 'Prepare'
                draft = abap_true
                determine = abap_true )

              "Custom Actions
              ( name = 'TestCustomAction'
                features_instance = abap_true
                parameter_entity_name = ''
              )

            )

            mapping = VALUE #(
              database_table_name = 'zsdslsorder000tp'
              corresponding = abap_true
              extensible = abap_false
              field_mappings = VALUE #(
                ( cds_view_field = 'Uuid'
                  dbtable_field  = 'uuid' )
                ( cds_view_field = 'SalesOrderNo'
                  dbtable_field  = 'sales_order_no' )
                ( cds_view_field = 'SalesOrderType'
                  dbtable_field  = 'sales_order_type' )
                ( cds_view_field = 'SalesOrderTypeDescr'
                  dbtable_field  = 'sales_order_type_descr' )
                ( cds_view_field = 'Description'
                  dbtable_field  = 'description' )
                ( cds_view_field = 'LocalCreatedBy'
                  dbtable_field  = 'local_created_by' )
                ( cds_view_field = 'LocalCreatedAt'
                  dbtable_field  = 'local_created_at' )
                ( cds_view_field = 'LocalLastChangedBy'
                  dbtable_field  = 'local_last_changed_by' )
                ( cds_view_field = 'LocalLastChangedAt'
                  dbtable_field  = 'local_last_changed_at' )
                ( cds_view_field = 'LastChangedAt'
                  dbtable_field  = 'last_changed_at' )
              )
            )

          )
        )
      )
    ).

    "Create
    DATA(lo_create_behavior_definition) = z_xco_cds_behavior_definition=>create_or_update_instance( ls_create ).

    cl_abap_unit_assert=>assert_not_initial( lo_create_behavior_definition ).

  ENDMETHOD.


ENDCLASS.
