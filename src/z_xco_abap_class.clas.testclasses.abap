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

    DATA(ls_create) = VALUE z_xco_abap_class=>ts_create(
      transport_request  = 'TRLK920710'
      package            = 'ZXCO_UNIT_TEST'

      class = VALUE #(

        short_description         = 'Unit Test Behavior Definition'

        main_class = VALUE #(

          name                      = 'Z_XCO_UT_TEST_CLASS'
          behavior_definition_name  = 'ZSDR_SLSORDERTP2'


*          interfaces = VALUE #(
*            ( name = 'IF_OO_ADT_CLASSRUN' )
*          )
*
*          public_section = VALUE #(
*            types = VALUE #(
*              ( name = 'TestType'  source = |c LENGTH 60| )
*            )

*            method_definitions = VALUE #(
*              ( name = 'test_method_1'
*                importing_parameters = VALUE #(
*                  ( name = 'test_import_paramater_1'
*                    type_category = z_xco_abap_class=>cs_data_type_category-type
*                    type_name = 'abap_boolean'
*                    default_value = 'abap_true' )
*                  ( name = 'test_import_paramater_2'
*                    type_category = z_xco_abap_class=>cs_data_type_category-interface
*                    interface_name = 'IF_OO_ADT_CLASSRUN' )
*                )
*
*              )
*            )
*
*          )
*
*          protected_section = VALUE #( )
*
*          private_section = VALUE #(
*            types = VALUE #(
*              ( name = 'Private_Test_Type'  source = |c LENGTH 60| )
*            )
*          )

*          method_implementations = VALUE #(
*            ( name = 'test_method_1'
*              source_lines = VALUE #(
*                ( |    DATA lv_test TYPE c LENGTH 4.| )
*              )
*            )
*          )
        )

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Local Classes
        local_classes = VALUE #(
          (
            name             = 'LHC_ZSDR_SLSORDERTP2'
            super_class_name = 'CL_ABAP_BEHAVIOR_HANDLER'

*            public_section = VALUE #(
*              types = VALUE #(
*                ( name = 'Local_class_public_Test_Type'  source = |c LENGTH 60| )
*              )
*            )

            protected_section = VALUE #( )

            private_section = VALUE #(
              types = VALUE #(
                ( name = 'Local_class_private_Test_Type'  source = |c LENGTH 60| )
              )

              method_definitions = VALUE #(
                ( name = 'GET_GLOBAL_AUTHORIZATIONS'
                  behavior = VALUE #(
                    global_authorization = abap_true
                    result_parameter_name = 'result'
                  )
                  importing_parameters = VALUE #(
                    ( name = ''
                      behavior = VALUE #(
                        request_name = 'requested_authorizations'
                        for_entity_alias_name = 'SalesOrder'
                      )

                    )
                  )
                )
              )

            )

            method_implementations = VALUE #(
              ( name = 'GET_GLOBAL_AUTHORIZATIONS'
                source_lines = VALUE #(
                  ( |    | )
                  ( |    DATA lv_test_1 TYPE c LENGTH 1.| )
                  ( |    | )
                )
              )
            )
          )
        )

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Test Classes
        test_classes = VALUE #(
          (
            name                      = 'LCL_TEST_CLASS_1'

            public_section = VALUE #(
              types = VALUE #(
                ( name = 'Test_class_public_Test_Type'  source = |c LENGTH 60| )
              )
            )

            protected_section = VALUE #( )

            private_section = VALUE #(
              types = VALUE #(
                ( name = 'Test_class_private_Test_Type'  source = |c LENGTH 60| )
              )

              method_definitions = VALUE #(
                ( name = 'test_method_1'
                  importing_parameters = VALUE #(
                    ( name = 'parameter_1'
                      type_category = z_xco_abap_class=>cs_data_type_category-type
                      type_name = 'abap_boolean'
                    )
                  )
                )
              )

            )

            method_implementations = VALUE #(
              ( name = 'TEST_METHOD_1'
                source_lines = VALUE #(
                  ( |    DATA lv_test_1 TYPE c LENGTH 1.| )
                )
              )
            )
          )
        )

      )

    ).

    "Create
    DATA(lo_create_class) = z_xco_abap_class=>create_or_update_instance( ls_create ).

    cl_abap_unit_assert=>assert_not_initial( lo_create_class ).

  ENDMETHOD.

ENDCLASS.
