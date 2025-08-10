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


          interfaces = VALUE #(
            ( name = 'IF_OO_ADT_CLASSRUN' )
          )

          public_section = VALUE #(
            types = VALUE #(
              ( name = 'TestType'  source = |c LENGTH 60| )
            )
          )

          protected_section = VALUE #( )

          private_section = VALUE #(
            types = VALUE #(
              ( name = 'Private_Test_Type'  source = |c LENGTH 60| )
            )
          )

          implementation_methods = VALUE #( )
        )

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Local Classes
        local_classes = VALUE #(
          (
            name                      = 'LCL_LOCAL_CLASS_1'

            public_section = VALUE #(
              types = VALUE #(
                ( name = 'Local_class_public_Test_Type'  source = |c LENGTH 60| )
              )
            )

            protected_section = VALUE #( )

            private_section = VALUE #(
              types = VALUE #(
                ( name = 'Local_class_private_Test_Type'  source = |c LENGTH 60| )
              )
            )

            implementation_methods = VALUE #( )
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
            )

            implementation_methods = VALUE #( )
          )
        )

      )

    ).

    "Create
    DATA(lo_create_class) = z_xco_abap_class=>create_or_update_instance( ls_create ).

    cl_abap_unit_assert=>assert_not_initial( lo_create_class ).

  ENDMETHOD.

ENDCLASS.
