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

    DATA(ls_create) = VALUE z_xco_rap_service_definition=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      service_definition = VALUE #(
        name = 'ZXCO_UNIT_TEST_SD'
        description = 'Unit Test'
        exposures = value #(
          ( name = 'ZSDC_SLSORDERTP' alias_name = 'SalesOrder' )
        )
      )
    ).

    "Create
    DATA(lo_create_service_definition) = z_xco_rap_service_definition=>create_or_update_instance( ls_create ).

    DATA(ls_act_data) = lo_create_service_definition->get_data( ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = ls_act_data
        exp                  = ls_create-service_definition ).

  ENDMETHOD.


ENDCLASS.
