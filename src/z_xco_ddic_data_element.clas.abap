CLASS z_xco_ddic_data_element DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    TYPES tv_data_element_name TYPE c LENGTH 30.

    TYPES tv_type_category TYPE c LENGTH 20.
    CONSTANTS:
      BEGIN OF cs_type_category,
        built_in_type TYPE tv_type_category VALUE 'BUILT_IN_TYPE',
        domain        TYPE tv_type_category VALUE 'DOMAIN',

        "TODO:
*        CLASS_REF           TYPE tv_category VALUE 'CLASS_REF',
*        INTERFACE_REF           TYPE tv_category VALUE 'INTERFACE_REF',
*
*        TYPE_REF           TYPE tv_category VALUE 'TYPE_REF',   "TYPE REF TO bukrs
*        DATA_TYPE_REF           TYPE tv_category VALUE 'DATA_TYPE_REF',  "TYPE REF TO data

      END OF cs_type_category.

    TYPES:
      BEGIN OF ts_built_in_type,
        type     TYPE z_xco_built_in_type_factory=>tv_data_type,
        length   TYPE cl_xco_ad_built_in_type=>tv_length,
        decimals TYPE cl_xco_ad_built_in_type=>tv_decimals,
      END OF ts_built_in_type.

    TYPES:
      BEGIN OF ts_data,
        name              TYPE sxco_ad_object_name,
        short_description TYPE if_xco_cp_gen_dtel_s_form=>tv_short_description,

        type_category     TYPE tv_type_category,
        built_in_type     TYPE ts_built_in_type,

        domain_name       TYPE z_xco_ddic_domain=>tv_domain_name,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        data_element_data TYPE ts_data,
      END OF ts_create.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                 TYPE ts_create
      RETURNING VALUE(ro_data_element_bo) TYPE REF TO z_xco_ddic_data_element.

    CLASS-METHODS get_instance
      IMPORTING iv_data_element_name      TYPE tv_data_element_name
      RETURNING VALUE(ro_data_element_bo) TYPE REF TO z_xco_ddic_data_element.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_data_element_name TYPE tv_data_element_name.

ENDCLASS.


CLASS z_xco_ddic_data_element IMPLEMENTATION.

  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_data_element_put_object) = lo_operation->for-dtel->add_object( iv_name = is_create-data_element_data-name ).

    lo_data_element_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_data_element_put_object->create_form_specification( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Description
    lo_specification->set_short_description( is_create-data_element_data-short_description ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Type
    CASE is_create-data_element_data-type_category.

      WHEN cs_type_category-built_in_type.

        DATA lo_data_element_data_type TYPE REF TO if_xco_gen_dtel_data_type.

        lo_data_element_data_type = NEW z_xco_built_in_type_factory( )->get_built_in_type(
          iv_data_type     = is_create-data_element_data-built_in_type-type
          iv_length        = is_create-data_element_data-built_in_type-length
          iv_decimals      = is_create-data_element_data-built_in_type-decimals ).

        lo_specification->set_data_type(
            io_data_type = lo_data_element_data_type ).

      WHEN cs_type_category-domain.

        DATA(lo_domain_data_type) = xco_cp_abap_dictionary=>domain( iv_name = is_create-data_element_data-domain_name ).

        lo_specification->set_data_type(
            io_data_type = lo_domain_data_type ).

      WHEN OTHERS.

        ASSERT 1 = 0.

    ENDCASE.

    lo_specification->field_label-short->set_length( 5 ).
    lo_specification->field_label-short->set_text( 'Test' ).

    lo_specification->field_label-medium->set_length( 20 ).
    lo_specification->field_label-medium->set_text( 'Test medium' ).

    lo_specification->field_label-long->set_length( 20 ).
    lo_specification->field_label-long->set_text( 'Test long' ).

    lo_specification->field_label-heading->set_length( 20 ).
    lo_specification->field_label-heading->set_text( 'Test heading' ).

    DATA(lo_result) = lo_operation->execute( ).

    IF lo_result->findings->contain_errors( ) = abap_true.
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(lt_findings) = lo_result->findings->get( ).
      ASSERT 1 = 0.
    ENDIF.

    ro_data_element_bo = NEW #( ).
    ro_data_element_bo->gv_data_element_name = is_create-data_element_data-name.

  ENDMETHOD.

  METHOD get_instance.

    ro_data_element_bo = NEW #( ).

    ro_data_element_bo->gv_data_element_name = iv_data_element_name.

  ENDMETHOD.


  METHOD get_data.

    DATA(lo_xco_data_element) = xco_cp_abap_dictionary=>data_element(
      gv_data_element_name ).

    IF lo_xco_data_element->exists( ) = abap_false.
      RETURN.
    ENDIF.

    rs_data-name = gv_data_element_name.

    IF gv_data_element_name = 'MANDT'.
      RETURN.
    ENDIF.

    DATA(ls_xco_data_element_content) = lo_xco_data_element->content( )->get( ).
    rs_data-short_description = ls_xco_data_element_content-short_description.

    DATA(lo_data_type) = ls_xco_data_element_content-data_type.

    DATA(lo_type) = lo_data_type->get_built_in_type( ).
    IF lo_type IS NOT INITIAL.
      rs_data-type_category = cs_type_category-built_in_type.
      rs_data-built_in_type-type = lo_type->type.
      rs_data-built_in_type-length = lo_type->length.
      rs_data-built_in_type-decimals = lo_type->decimals.
      RETURN.
    ENDIF.

    DATA(lo_domain) = lo_data_type->get_domain( ).
    IF lo_domain IS NOT INITIAL.
      rs_data-type_category = cs_type_category-domain.
      rs_data-domain_name = lo_domain->if_xco_ar_object~name->value.
      RETURN.
    ENDIF.

    "TODO:
*    DATA(lo_type_reference) = lo_data_type->get_built_in_type_reference( ).
    "rs_data-category = cs_category-type_reference.
*
*    DATA(lo_class_reference) = lo_data_type->get_class( ).
    "rs_data-category = cs_category-class_reference.
*
*    DATA(lo_data_type_reference) = lo_data_type->get_data_type_reference( ).
    ""rs_data-category = cs_category-data_type_reference.
*
*    DATA(lo_interface_reference) = lo_data_type->get_interface( ).
    "rs_data-category = cs_category-interface_reference.

    ASSERT 1 = 0.

  ENDMETHOD.


ENDCLASS.
