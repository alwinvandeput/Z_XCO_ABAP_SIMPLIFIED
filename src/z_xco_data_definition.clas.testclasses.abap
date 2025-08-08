CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_cds_view_1
      FOR TESTING
      RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Helper methods
    METHODS _create_cds_view
      IMPORTING is_create TYPE z_xco_data_definition=>ts_create.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create_cds_view_1.

    DATA(ls_create_cds_view_data) = VALUE z_xco_data_definition=>ts_create(
      transport_request = 'TRLK920710'
      package           = 'ZXCO_UNIT_TEST'
      cds_view_data = VALUE #(
        name              = 'ZXCO_UT_SalesOrderTP'
        short_description = 'Unit Test Data Element 1'

        annotations = VALUE #(
          ( name = 'AccessControl.authorizationCheck' value_items = VALUE #(
            ( type =  z_xco_data_definition=>cs_value_item_type-enum_value value = |NOT_REQUIRED| ) ) )
          ( name = 'Metadata.allowExtensions'         value_items = VALUE #(
            ( type =  z_xco_data_definition=>cs_value_item_type-boolean_value value = |true| ) ) )
          ( name = 'EndUserText.label'                value_items = VALUE #(
            ( type =  z_xco_data_definition=>cs_value_item_type-string_value value = |Sales Order| ) ) )
          ( name = 'ObjectModel.semanticKey' value_items = VALUE #(
            ( type = z_xco_data_definition=>cs_value_item_type-begin_array )
            ( type =  z_xco_data_definition=>cs_value_item_type-string_value value = |SalesOrderNo| )
            ( type = z_xco_data_definition=>cs_value_item_type-end_array )
          ) )
          ( name = 'ObjectModel.sapObjectNodeType.name' value_items = VALUE #(
            ( type =  z_xco_data_definition=>cs_value_item_type-string_value value = |ZSDSalesOrder000TP| ) ) )
        )

        root_ind = abap_true
        data_definition_type = z_xco_data_definition=>cs_data_definition_type-view_entity

        data_source = VALUE #(
          name = 'ZSDSlsOrder000TP'
          alias_name = 'SalesOrder'
        )

        fields = VALUE #(
          ( key_indicator = abap_true
            name = 'client'
            alias_name = 'Client'
          )
          ( key_indicator = abap_true
            name = 'uuid'
            alias_name = 'Uuid'
          )
          ( key_indicator = abap_false
            name = 'sales_order_no'
            alias_name = 'SalesOrderNo'
          )
          ( key_indicator = abap_false
            name = 'sales_order_type'
            alias_name = 'SalesOrderType'
            annotations = VALUE #(
              ( name = 'ObjectModel.text.element' value_items = VALUE #(
                ( type = z_xco_data_definition=>cs_value_item_type-begin_array )
                ( type = z_xco_data_definition=>cs_value_item_type-string_value value = |SalesOrderTypeDescr| )
                ( type = z_xco_data_definition=>cs_value_item_type-end_array )
              ) )
            )
          )
          ( key_indicator = abap_false
            name = 'sales_order_type_descr'
            alias_name = 'SalesOrderTypeDescr'
          )
        )

      )
    ).

    _create_cds_view( ls_create_cds_view_data ).

  ENDMETHOD.

  METHOD _create_cds_view.

    DATA(lo_cds_view) = z_xco_data_definition=>create_or_update_instance( is_create ).

*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(ls_database_table_data) = lo_database_table->get_data( ).
*    cl_abap_unit_assert=>assert_equals(
*        exp = is_create-database_table
*        act = ls_database_table_data ).

  ENDMETHOD.

ENDCLASS.
