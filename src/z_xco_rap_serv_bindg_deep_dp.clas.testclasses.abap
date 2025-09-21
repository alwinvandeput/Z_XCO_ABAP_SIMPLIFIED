CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS deep_read_service_binding
      "FOR TESTING
      RAISING cx_static_check.

    METHODS deep_read_service_binding_2 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD deep_read_service_binding.

    DATA(ls_data) = NEW z_xco_rap_serv_bindg_deep_dp(
      )->deep_read_service_binding(
        iv_service_binding_name      = 'ZZAPUI_SALESORDER_O4'
        iv_sd_root_entity_alias_name = 'SalesOrder' ).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).

  ENDMETHOD.

  METHOD deep_read_service_binding_2.


    DATA(ls_data) = NEW z_xco_rap_serv_bindg_deep_dp(
      )->deep_read_service_binding_2(
        iv_service_binding_name      = 'ZAP2UI_INVOICE_O4'
        iv_sd_root_entity_alias_name = 'Invoice' ).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).

  ENDMETHOD.

ENDCLASS.
