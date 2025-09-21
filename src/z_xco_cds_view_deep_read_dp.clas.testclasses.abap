CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS deep_read_cds_view
      FOR TESTING
      RAISING cx_static_check.

    METHODS read_in_layers
      "FOR TESTING
      RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD deep_read_cds_view.

    DATA(lo_data_provider) = NEW z_xco_cds_view_deep_read_dp( ).

    DATA(ls_cds_view_deep_data) = lo_data_provider->deep_read_cds_view(
      'ZAP2C_INVOICE' ).

    cl_abap_unit_assert=>assert_not_initial( ls_cds_view_deep_data ).

  ENDMETHOD.

  METHOD read_in_layers.

    DATA(lo_data_provider) = NEW z_xco_cds_view_deep_read_dp( ).

    DATA(ls_cds_view_deep_data) = lo_data_provider->read_in_layers(
      'ZZAPC_SALESORDER' ).

    cl_abap_unit_assert=>assert_not_initial( ls_cds_view_deep_data ).

  ENDMETHOD.

ENDCLASS.
