CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "For a test of all built-in-types, see Unit tests of Z_XCO_DATABASE_TABLE.

    METHODS create_char25_domain FOR TESTING
      RAISING cx_static_check.

    METHODS create_fixed_value_domain FOR TESTING
      RAISING cx_static_check.

    METHODS create_case_sensitve_domain FOR TESTING
      RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Helper methods
    METHODS _create_domain
      IMPORTING is_create TYPE z_xco_domain=>ts_create.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create_char25_domain.

    _create_domain( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      domain_data = VALUE #(
        name              = 'ZXCO_UT_DOMAIN_1'
        short_description = 'Unit Test Domain 1'
        built_in_type     = VALUE #(
          type     = z_xco_built_in_type_factory=>cs_data_type-character
          length   = 25
          decimals = 0 )
        case_sensitive    = abap_false
        fixed_values      = VALUE #( ) ) ) ).

  ENDMETHOD.

  METHOD create_fixed_value_domain.

    _create_domain( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      domain_data = VALUE #(
        name              = 'ZXCO_UT_DOMAIN_2'
        short_description = 'Unit Test Domain 2'
        built_in_type     = VALUE #(
          type     = z_xco_built_in_type_factory=>cs_data_type-character
          length   = 8
          decimals = 0 )
        case_sensitive    = abap_false
        fixed_values = VALUE #(
        ( lower_limit = 'A'
          description = 'Alpha' )
        ( lower_limit = 'B'
          description = 'Beta' )
      ) )
      ) ).

  ENDMETHOD.

  METHOD create_case_sensitve_domain.

    _create_domain( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      domain_data = VALUE #(
        name              = 'ZXCO_UT_DOMAIN_3'
        short_description = 'Unit Test Domain 3'
        built_in_type     = VALUE #(
          type     = z_xco_built_in_type_factory=>cs_data_type-character
          length   = 26
          decimals = 0 )
        case_sensitive    = abap_true
        fixed_values      = VALUE #( ) ) ) ).

  ENDMETHOD.

  METHOD _create_domain.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Create
    DATA(lo_create_domain) = z_xco_domain=>create_or_update_instance( is_create ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Validate
    DATA(lv_exists) = lo_create_domain->check_exists( ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_exists
        exp = abap_true ).

    DATA(ls_act_data) = lo_create_domain->get_data( ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = ls_act_data
        exp                  = is_create-domain_data ).

  ENDMETHOD.


ENDCLASS.
