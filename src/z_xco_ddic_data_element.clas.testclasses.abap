CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "For a test of all built-in-types, see Unit tests of Z_XCO_DATABASE_TABLE.


    METHODS get_data " FOR TESTING
      RAISING cx_static_check.

    METHODS
      create_char25
        "FOR TESTING
        RAISING cx_static_check.

    METHODS
      create_with_domain
        FOR TESTING RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Helper methods
    METHODS _create_data_element
      IMPORTING is_create TYPE z_xco_ddic_data_element=>ts_create.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD get_data.

    DATA(lo_data_element) = z_xco_ddic_data_element=>get_instance( 'ZRGD_ABAP_NAMESPACE' ).

    DATA(ls_data_element_data) = lo_data_element->get_data( ).

    cl_abap_unit_assert=>assert_not_initial( ls_data_element_data ).

  ENDMETHOD.


  METHOD create_char25.

    _create_data_element( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      data_element_data = VALUE #(
        name              = 'ZXCO_UT_DATA_ELEMENT_1'
        short_description = 'Unit Test Data Element 1'

        type_category = z_xco_ddic_data_element=>cs_type_category-built_in_type
        built_in_type     = VALUE #(
          type     = z_xco_built_in_type_factory=>cs_data_type-character
          length   = 25
          decimals = 0 ) )
    ) ).

  ENDMETHOD.


  METHOD create_with_domain.

    _create_data_element( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      data_element_data = VALUE #(
        name              = 'ZXCO_UT_DATA_ELEMENT_2'
        short_description = 'Unit Test Data Element 2'

        type_category = z_xco_ddic_data_element=>cs_type_category-domain
        domain_name   = 'ZXCO_UT_DOMAIN_1' )
    ) ).

  ENDMETHOD.


  METHOD _create_data_element.

    DATA(lo_data_element) = z_xco_ddic_data_element=>create_or_update_instance( is_create ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(ls_data_element_data) = lo_data_element->get_data( ).

    DATA(ls_act_data) = lo_data_element->get_data( ).
    cl_abap_unit_assert=>assert_equals(
        exp = is_create-data_element_data
        act = ls_act_data ).

  ENDMETHOD.


ENDCLASS.
