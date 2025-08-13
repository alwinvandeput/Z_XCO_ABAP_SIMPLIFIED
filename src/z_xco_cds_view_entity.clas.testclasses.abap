CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_cds_view_1
      FOR TESTING
      RAISING cx_static_check.


    METHODS create_cds_view_1
      "FOR TESTING
      RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Helper methods
    METHODS _create_cds_view
      IMPORTING is_create TYPE z_xco_cds_view_entity=>ts_create.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create_cds_view_1.

    DATA(lc_type) = z_xco_cds_annotation_converter=>cs_value_item_type.

    DATA(ls_create_cds_view_data) = VALUE z_xco_cds_view_entity=>ts_create(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      cds_view_data = VALUE #(
        name              = 'ZXCO_UT_SalesOrderTP'
        short_description = 'Unit Test Data Element 1'

        annotations = VALUE #(
          ( name = 'AccessControl.authorizationCheck' value_items = VALUE #(
            ( type    =  lc_type-enum_value
              value   = |NOT_REQUIRED| ) ) )
          ( name = 'Metadata.allowExtensions'
            value_items = VALUE #(
              ( type  =  lc_type-boolean_value
                value = |true| ) ) )
          ( name = 'EndUserText.label'
            value_items = VALUE #(
              ( type  = lc_type-string_value
                value = |Sales Order| ) ) )
          ( name = 'ObjectModel.semanticKey'
            value_items = VALUE #(
              ( type  = lc_type-begin_array )
              ( type  = lc_type-string_value
                value = |SalesOrderNo| )
              ( type  = lc_type-end_array )
          ) )
          ( name = 'ObjectModel.sapObjectNodeType.name'
             value_items = VALUE #(
              ( type  =  lc_type-string_value value = |ZSDSalesOrder000TP| ) ) )
        )

        root_indicator = abap_true
*        data_definition_type = z_xco_view_entity=>cs_data_definition_type-view_entity

        data_source = VALUE #(
          name = 'ZSDSlsOrder000TP'
          alias_name = 'SalesOrder'
        )

        compositions = VALUE #(
          ( entity_name = 'ZSDR_SLSRDRTMTP2'
            alias_name = '_SalesOrderItem3'
            cardinality = VALUE #(
              min = 0
              max = 500 )
          )
        )

        associations = VALUE #(
          ( entity_name = 'ZSDR_SLSRDRTMTP2'
            alias_name = '_SalesOrderItem6'
            cardinality = VALUE #(
              min = 0
              max = 500 )
            condition_text = '_SalesOrderItem6.ParentUuid = $projection.Uuid' )
        )

        fields = VALUE #(
*          ( key_indicator = abap_true
*            name = 'client'
*            alias_name = 'Client'
*          )
          ( key_indicator = abap_true
            name = 'uuid'
            alias_name = 'Uuid'
          )
          ( name = 'sales_order_no'
            alias_name = 'SalesOrderNo'
          )
          ( name = 'sales_order_type'
            alias_name = 'SalesOrderType'
            annotations = VALUE #(
              ( name = 'ObjectModel.text.element' value_items = VALUE #(
                ( type = lc_type-begin_array )
                ( type = lc_type-string_value value = |SalesOrderTypeDescr| )
                ( type = lc_type-end_array )
              ) )
            )
          )
          ( name = 'sales_order_type_descr'
            alias_name = 'SalesOrderTypeDescr'
          )

          "Associations
          ( name = '_SalesOrderItem3' )
          ( name = '_SalesOrderItem6' )
        )

        where_condition_text = |( sales_order_type = sales_order_type ) { cl_abap_char_utilities=>newline }or { cl_abap_char_utilities=>newline }sales_order_type = sales_order_type|

      )
    ).

    _create_cds_view( ls_create_cds_view_data ).

  ENDMETHOD.

  METHOD _create_cds_view.

    DATA(ls_create) = is_create.

    DATA(lo_cds_view_entity) = z_xco_cds_view_entity=>create_or_update_instance( is_create ).

    DATA(ls_view_entity_data) = lo_cds_view_entity->get_data( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Correct Create data
    "- GET_DATA is does not fully support CamelCase
    "- GET_DATA does not support annotations
    "- GET_DATA removes all spaces and "new line" in the WHERE condition text.
    ls_create-cds_view_data-name = to_upper( ls_create-cds_view_data-name ).
    CLEAR ls_create-cds_view_data-annotations.

    LOOP AT ls_create-cds_view_data-associations
      ASSIGNING FIELD-SYMBOL(<ls_assocation>).

      DATA(lv_input)  = <ls_assocation>-condition_text.
      DATA(lv_output) = lv_input.

      " Find all matches after a dot
      DATA(mt_matches) = VALUE match_result_tab( ).

      FIND ALL OCCURRENCES OF REGEX '\.(\w+)' IN lv_input RESULTS mt_matches.

      LOOP AT mt_matches INTO DATA(ls_match).
        " Extract the matched word after the dot
        DATA(lv_index) = ls_match-offset + 1.
        DATA(lv_length) = ls_match-length - 1.
        DATA(lv_matched) = lv_input+lv_index(lv_length).
        DATA(lv_upper)   = to_upper( lv_matched ).

        " Replace in the original string
        REPLACE FIRST OCCURRENCE OF |.{ lv_matched }| IN lv_output WITH |.{ lv_upper }|.
      ENDLOOP.

      <ls_assocation>-condition_text = lv_output.

    ENDLOOP.

    LOOP AT ls_create-cds_view_data-fields
      ASSIGNING FIELD-SYMBOL(<ls_field>).
      CLEAR <ls_field>-annotations.
    ENDLOOP.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN ls_create-cds_view_data-where_condition_text WITH ''.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    cl_abap_unit_assert=>assert_equals(
        exp = ls_create-cds_view_data
        act = ls_view_entity_data ).

  ENDMETHOD.


  METHOD get_cds_view_1.

    DATA(lo_cds_view_entity) = z_xco_cds_view_entity=>get_instance(
*      'ZXCO_UT_SalesOrderTP'
      'ZSDC_SLSORDERTP'
      ).

    DATA(ls_view_entity_data) = lo_cds_view_entity->get_data( ).

    cl_abap_unit_assert=>assert_not_initial( ls_view_entity_data ).

  ENDMETHOD.

ENDCLASS.
