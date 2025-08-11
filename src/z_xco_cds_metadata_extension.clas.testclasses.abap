CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_minimal
      FOR TESTING
      RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD create_minimal.

    DATA(lc_type) = z_xco_annotation_converter=>cs_value_item_type.

    DATA(ls_create) = VALUE z_xco_cds_metadata_extension=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'
      metadata_extension = VALUE #(
        name                 = 'ZXCO_METADATA_2'
        short_description    = 'Unit Test Metadata Extension'
        layer                = xco_cp_metadata_extension=>layer->customer
        view                 = 'ZSDR_SLSORDERTP2'

        fields = VALUE #(
          ( name = 'Uuid'
            annotations = VALUE #(
              (
                name = 'UI.lineItem'
                value_items = VALUE #(
                  ( type = lc_type-begin_array )
                  ( type = lc_type-begin_record )

                  ( type = lc_type-member    value = 'position' )
                  ( type = lc_type-number_value   value = '10' )

                  ( type = lc_type-member      value = 'importance' )
                  ( type = lc_type-enum_value   value = 'HIGH' )

                  ( type = lc_type-member       value = 'label' )
                  ( type = lc_type-string_value   value = 'Test Label' )

                  ( type = lc_type-end_record )
                  ( type = lc_type-end_array )
                )
              )
            )
          )
        )
      )
    ).

    "Create
    DATA(lo_create_metadata_extension) = z_xco_cds_metadata_extension=>create_or_update_instance( ls_create ).

    cl_abap_unit_assert=>assert_not_initial( lo_create_metadata_extension ).

  ENDMETHOD.



ENDCLASS.
