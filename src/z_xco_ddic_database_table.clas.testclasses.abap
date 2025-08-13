CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_data "FOR TESTING
      RAISING cx_static_check.

    METHODS create_w_built_in_types
      FOR TESTING
      RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Helper methods
    METHODS _create_database_table
      IMPORTING is_create TYPE z_xco_ddic_database_table=>ts_create.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD get_data.

    DATA(lo_db_table) = z_xco_ddic_database_table=>get_instance( 'ZZAP_SALES_ORDER' ).

    DATA(ls_data) = lo_db_table->get_data( ).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).

  ENDMETHOD.

  METHOD create_w_built_in_types.

    DATA(ls_data_element_data) = VALUE z_xco_ddic_data_element=>ts_create(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      data_element_data = VALUE #(
        name              = 'ZXCO_UT_DB_TABLE_DTEL_1'
        short_description = 'Unit Test Data Element 1'

        type_category = z_xco_ddic_data_element=>cs_type_category-built_in_type
        built_in_type     = VALUE #(
          type     = z_xco_ddic_built_in_type_fct=>cs_data_type-character
          length   = 25
          decimals = 0 ) )
    ).
    z_xco_ddic_data_element=>create_or_update_instance( ls_data_element_data ).


    DATA(built_in) = z_xco_ddic_database_table=>cs_type_category-built_in_type.

    _create_database_table( VALUE #(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      database_table = VALUE #(
        name              = 'ZXCO_DB_1'
        fields = VALUE #(
          ( name = 'CLIENT'
            key_indicator = abap_true
            not_null = abap_true
            type_category = z_xco_ddic_database_table=>cs_type_category-built_in_type
            built_in_type = VALUE #(
              type = z_xco_ddic_built_in_type_fct=>cs_data_type-client
              length = 3 )
          )
          ( name = 'FIELD_1'
            key_indicator = abap_false
            type_category = z_xco_ddic_database_table=>cs_type_category-built_in_type
            built_in_type = VALUE #(
              type = z_xco_ddic_built_in_type_fct=>cs_data_type-character
              length = 10 )
          )
          ( name = 'FIELD_2'
            key_indicator = abap_false
            type_category = z_xco_ddic_database_table=>cs_type_category-data_element
            data_element_name = 'ZXCO_UT_DB_TABLE_DTEL_1'
            short_description = ls_data_element_data-data_element_data-short_description
          )
          ( name = 'FIELD_3'
            key_indicator = abap_false
            type_category = z_xco_ddic_database_table=>cs_type_category-built_in_type
            built_in_type = VALUE #(
              type = z_xco_ddic_built_in_type_fct=>cs_data_type-number_character
              length = 5 )
          )

          ( name = 'FIELD_01' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-character ) )
          ( name = 'FIELD_02' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 0 type = z_xco_ddic_built_in_type_fct=>cs_data_type-string ) )
          ( name = 'FIELD_03' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-short_string ) )
*          ( name = 'FIELD_04' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 0 type = z_xco_built_in_type_factory=>cs_data_type-long_character ) )
          ( name = 'FIELD_05' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 1 type = z_xco_ddic_built_in_type_fct=>cs_data_type-language ) )
          ( name = 'FIELD_06' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-number_character ) )
          ( name = 'FIELD_07' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 16 type = z_xco_ddic_built_in_type_fct=>cs_data_type-currency  decimals = 2 )
            reference_field = VALUE #( table_name = 'ZXCO_DB_1' field_name = 'FIELD_08' ) )
          ( name = 'FIELD_08' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-currency_key ) )
          ( name = 'FIELD_09' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-quantity   decimals = 2 )
            reference_field = VALUE #( table_name = 'ZXCO_DB_1' field_name = 'FIELD_10' ) )
          ( name = 'FIELD_10' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 2 type = z_xco_ddic_built_in_type_fct=>cs_data_type-unit ) )
          ( name = 'FIELD_11' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 8 type = z_xco_ddic_built_in_type_fct=>cs_data_type-date ) )
          ( name = 'FIELD_12' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 6 type = z_xco_ddic_built_in_type_fct=>cs_data_type-time ) )
          ( name = 'FIELD_13' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 6 type = z_xco_ddic_built_in_type_fct=>cs_data_type-posting_period_yyyymm ) )
          ( name = 'FIELD_14' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 16 type = z_xco_ddic_built_in_type_fct=>cs_data_type-binary_float decimals = 16 ) )
          ( name = 'FIELD_15' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 16 type = z_xco_ddic_built_in_type_fct=>cs_data_type-binary_float_16 ) )
          ( name = 'FIELD_16' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 34 type = z_xco_ddic_built_in_type_fct=>cs_data_type-binary_float_34 ) )
          ( name = 'FIELD_17' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 3 type = z_xco_ddic_built_in_type_fct=>cs_data_type-integer_1 ) )
          ( name = 'FIELD_18' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-integer_2 ) )
          ( name = 'FIELD_19' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 10 type = z_xco_ddic_built_in_type_fct=>cs_data_type-integer_4 ) )
          ( name = 'FIELD_20' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 19 type = z_xco_ddic_built_in_type_fct=>cs_data_type-integer_8 ) )
          ( name = 'FIELD_21' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-decimals     decimals = 2 ) )
          ( name = 'FIELD_22' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-decimals_16  decimals = 2 ) )
          ( name = 'FIELD_23' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-decimals_34  decimals = 2 ) )
          ( name = 'FIELD_24' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 5 type = z_xco_ddic_built_in_type_fct=>cs_data_type-raw ) )
*          ( name = 'FIELD_25' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 0 type = z_xco_built_in_type_factory=>cs_data_type-long_binary ) )
          ( name = 'FIELD_26' key_indicator = abap_false type_category = built_in built_in_type = VALUE #( length = 0 type = z_xco_ddic_built_in_type_fct=>cs_data_type-rawstring ) )

        )
      )
    ) ).


  ENDMETHOD.

  METHOD _create_database_table.

    DATA(lo_database_table) = z_xco_ddic_database_table=>create_or_update_instance( is_create ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(ls_database_table_data) = lo_database_table->get_data( ).
    cl_abap_unit_assert=>assert_equals(
        exp = is_create-database_table
        act = ls_database_table_data ).

  ENDMETHOD.

ENDCLASS.
