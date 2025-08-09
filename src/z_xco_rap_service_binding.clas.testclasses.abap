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

    DATA(ls_create) = VALUE z_xco_rap_service_binding=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      service_binding = VALUE #(
        name = 'ZXCO_UNIT_TEST_SB'
        description = 'Unit Test'
        binding_type = xco_cp_service_binding=>binding_type->odata_v4_ui
        services = VALUE #(
          (
            "When Service Name is not filled, it will be automatically filled with Service Binding Name
            name = ''
            versions = VALUE #(
              ( version = '0001' service_definition_name = 'ZSDUI_SALESORDER_O4TP' )
           )
         )
        )
      )
    ).

    "Create
*    DATA(lo_create_service_binding) = z_xco_rap_service_binding=>create_or_update_instance( ls_create ).

    "Get Instance
    DATA(lo_create_service_binding) = z_xco_rap_service_binding=>get_instance(
        iv_service_binding_name = ls_create-service_binding-name
    ).

    "Get Data
    DATA(ls_act_data) = lo_create_service_binding->get_data( ).

    "Change Expected data:
    "- If Service value is not filled, then SAP fills it with the service binding name.
    LOOP AT ls_create-service_binding-services
      ASSIGNING FIELD-SYMBOL(<ls_service>).
      IF <ls_service>-name IS INITIAL.
        <ls_service>-name = ls_create-service_binding-name.
      ENDIF.
    ENDLOOP.

    "Validate
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = ls_act_data
        exp                  = ls_create-service_binding ).

  ENDMETHOD.


ENDCLASS.
