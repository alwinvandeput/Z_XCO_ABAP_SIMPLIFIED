CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create
      FOR TESTING
      RAISING cx_static_check.


ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create.

    DATA(ls_create) = VALUE z_xco_cds_behavior_definition=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      behavior_definition = VALUE #(
        name                      = 'ZXCO_UNIT_TEST_SB'
        description               = 'Unit Test'
        implementation_type       = xco_cp_behavior_definition=>implementation_type->managed
        implementation_class_name = ''
        draft_enabled             = abap_true
        extensible                = abap_false
        strict_mode               = zdmo_cl_rap_node=>strict_mode_2
        behaviors = value #(
          ( cds_entity_name           = 'ZSDR_SLSORDERTP'
            alias_name                = 'SalesOrder'
            implementation_class_name = ''
            extensible                = abap_false
            lock_master               = abap_true
            draft_table_name          = 'ZSDSLSORDER_DTP'

            standard_operations = value #(
              ( xco_cp_behavior_definition=>standard_operation->create )
              ( xco_cp_behavior_definition=>standard_operation->update )
              ( xco_cp_behavior_definition=>standard_operation->delete )
            )

            actions = value #(

              "Draft Actions
              ( name = 'Edit'
                draft = abap_true )
              ( name = 'Activate'
                draft = abap_true
                optimized = abap_true )
              ( name = 'Discard'
                draft = abap_true )
              ( name = 'Resume'
                draft = abap_true )
              ( name = 'Prepare'
                draft = abap_true
                determine = abap_true )
              ( name = 'Prepare'
                draft = abap_true )


              "Custom Actions
              ( name = 'Edit'
                features_instance = abap_true
                parameter_Entity_name = ''
              )

            )

          )
        )
      )
    ).
*
*    "Create
**    DATA(lo_create_behavior_definition) = z_xco_cds_behavior_definition=>create_or_update_instance( ls_create ).
*
*    "Get Instance
*    DATA(lo_create_behavior_definition) = z_xco_cds_behavior_definition=>get_instance(
*        iv_behavior_definition_name = ls_create-behavior_definition-name
*    ).
*
*    "Get Data
*    DATA(ls_act_data) = lo_create_behavior_definition->get_data( ).
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = ls_act_data
*        exp                  = ls_create-behavior_definition ).

  ENDMETHOD.


ENDCLASS.
