CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD first_test.

    DATA(factory) = z_xco_cds_data_definition_fact=>get_factory( ).

    DATA(cds_view) = factory->get_instance( 'ZZAPR_SALESORDER' ).

  ENDMETHOD.

ENDCLASS.
