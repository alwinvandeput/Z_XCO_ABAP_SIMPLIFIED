CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_data.
      "FOR TESTING.

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

  METHOD get_data.

**    DATA(lo_object) = xco_cp_abap_repository=>object->for(
**        iv_type   = 'SRVD'
**        iv_name   = 'ZSDUI_SALESORDER_O4TP' ).
**
**
**    DATA(lo_ddls_object) = xco_cp_abap_repository=>object->for(
**        iv_type   = 'DDLS'
**        iv_name   = 'ZSDR_SLSORDERTP' ).
*
*    DATA(lo_service_binding) = xco_cp_abap_repository=>object->srvd->for( 'ZSDUI_SALESORDER_O4TP' ).
*    DATA(ls_srvb_content) = lo_service_binding->content( )->get( ).
*    DATA(lt_services) = lo_service_binding->services->all->get( ).
*    LOOP AT lt_services
*      ASSIGNING FIELD-SYMBOL(<ls_service>).
*
*      DATA(lo_text) = <ls_service>->if_xco_printable~get_text( ).
*
*    ENDLOOP.
*



  ENDMETHOD.

ENDCLASS.
