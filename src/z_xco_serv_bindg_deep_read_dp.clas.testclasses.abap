CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      deep_read FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD deep_read.

    DATA(ls_data) = NEW z_xco_serv_bindg_deep_read_dp(
      )->deep_read_service_binding(
       iv_service_binding_name      = 'ZSDUI_SALESORDER_O4TP'
       iv_sd_root_entity_alias_name = 'SalesOrder' ).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).

  ENDMETHOD.

ENDCLASS.
