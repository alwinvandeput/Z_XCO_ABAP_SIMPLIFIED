CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS view_entity FOR TESTING RAISING cx_static_check.
    METHODS projection_view FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD view_entity.

    DATA(lo_view_entity) = z_xco_cds_dd_object_factory=>get_factory( )->get_cds_object_by_name( 'ZSDR_SLSORDERTP' ).

    IF lo_view_entity IS INSTANCE OF z_xco_cds_view_entity.
    ELSE.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD projection_view.

    DATA(lo_view_entity) = z_xco_cds_dd_object_factory=>get_factory( )->get_cds_object_by_name( 'ZSDC_SLSORDERTP' ).

    IF lo_view_entity IS INSTANCE OF z_xco_cds_projection_view.
    ELSE.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
