CLASS z_xco_database_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_db_table_name TYPE sxco_dbt_object_name.
    TYPES tv_data_element_name TYPE sxco_ad_object_name.
    TYPES tv_type_category TYPE c LENGTH 15.

    TYPES:
      BEGIN OF ts_built_in_type,
        type     TYPE z_xco_built_in_type_factory=>tv_data_type,
        length   TYPE cl_xco_ad_built_in_type=>tv_length,
        decimals TYPE cl_xco_ad_built_in_type=>tv_decimals,
      END OF ts_built_in_type.

    TYPES:
      BEGIN OF ts_reference_field,
        table_name TYPE if_xco_gen_tabl_s_fo_curr_quan=>tv_reference_table,
        field_name TYPE if_xco_gen_tabl_s_fo_curr_quan=>tv_reference_field,
      END OF ts_reference_field.

    TYPES:
      BEGIN OF ts_db_field,
        name              TYPE sxco_ad_field_name,
        key_indicator     TYPE abap_boolean,
        short_description TYPE if_xco_dbt_field_content=>tv_short_description,
        not_null          TYPE abap_boolean,

        type_category     TYPE tv_type_category,
        data_element_name TYPE tv_data_element_name,
        built_in_type     TYPE ts_built_in_type,

        reference_field   TYPE ts_reference_field,

      END OF ts_db_field,
      tt_db_fields TYPE STANDARD TABLE OF ts_db_field WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name              TYPE sxco_dbt_object_name,
        short_description TYPE if_xco_cp_gen_tabl_dbt_s_form=>tv_short_description,
        fields            TYPE tt_db_fields,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        database_table    TYPE ts_data,
      END OF ts_create.

    CONSTANTS:
      BEGIN OF cs_type_category,
        built_in_type TYPE tv_type_category VALUE 'BUILT_IN_TYPE',
        data_element  TYPE tv_type_category VALUE 'DATA_ELEMENT',
      END OF cs_type_category.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                TYPE ts_create
      RETURNING VALUE(ro_database_table) TYPE REF TO z_xco_database_table.

    CLASS-METHODS get_instance
      IMPORTING iv_db_table_name      TYPE tv_db_table_name
      RETURNING VALUE(ro_db_table_bo) TYPE REF TO z_xco_database_table.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_db_table_name TYPE tv_db_table_name.

ENDCLASS.


CLASS z_xco_database_table IMPLEMENTATION.

  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-tabl-for-database_table->add_object( is_create-database_table-name ).

    lo_put_object->set_package( is_create-package ).

    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-database_table-short_description ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Fields
    LOOP AT is_create-database_table-fields
         ASSIGNING FIELD-SYMBOL(<ls_field>).

      " Add Field
      DATA(lo_field) = lo_specification->add_field( to_upper( <ls_field>-name ) ).

      " Set Key Indicator
      IF <ls_field>-key_indicator = abap_true.
        lo_field->set_key_indicator( ).
      ENDIF.

      IF <ls_field>-not_null = abap_true.
        lo_field->set_not_null( ).
      ENDIF.

      " Set Data Type
      CASE <ls_field>-type_category.

        WHEN cs_type_category-built_in_type.

          DATA(lo_built_in_type) = NEW z_xco_built_in_type_factory( )->get_built_in_type(
            iv_data_type     = <ls_field>-built_in_type-type
            iv_length        = <ls_field>-built_in_type-length
            iv_decimals      = <ls_field>-built_in_type-decimals ).

          lo_field->set_type( lo_built_in_type ).

        WHEN cs_type_category-data_element.

          DATA(lo_data_element_type) = xco_cp_abap_dictionary=>data_element( iv_name = <ls_field>-data_element_name ).

          lo_field->set_type( lo_data_element_type ).

        WHEN OTHERS.
          ASSERT 1 = 0.

      ENDCASE.

      " Set Reference Field
      IF <ls_field>-reference_field IS NOT INITIAL.
        lo_field->currency_quantity->set_reference_table( <ls_field>-reference_field-table_name ).
        lo_field->currency_quantity->set_reference_field( <ls_field>-reference_field-field_name ).
      ENDIF.

    ENDLOOP.

    " ----------------------------------------------------------------------
    " B egin of deletion 2020
    " ----------------------------------------------------------------------
    " IF is_draft_table = abap_true.
    "   lo_specification->add_include( )->set_structure( iv_structure = CONV #( to_upper( 'sych_bdl_draft_admin_inc' ) )  )->set_group_name( to_upper( '%admin' )  ).
    " ENDIF.
    " ----------------------------------------------------------------------
    " E nd of deletion 2020
    " ----------------------------------------------------------------------

    " Set Extensible
*    IF io_rap_bo_node->is_extensible(  ) = abap_true.
*      lo_specification->add_include( )->set_structure( iv_structure = CONV #( to_upper( io_rap_bo_node->rap_node_objects-extension_include ) )  ).
*      set_table_enhancement_cat_any( lo_specification  ).
*    ENDIF.

    lo_operation->execute( ).

    ro_database_table = NEW #( ).
    ro_database_table->gv_db_table_name = is_create-database_table-name.

  ENDMETHOD.

  METHOD get_instance.

    ro_db_table_bo = NEW #( ).

    ro_db_table_bo->gv_db_table_name = iv_db_table_name.

  ENDMETHOD.


  METHOD get_data.

    rs_data-name = gv_db_table_name.

    DATA(lo_xco_database_table) = xco_cp_abap_dictionary=>database_table(
      gv_db_table_name ).

    " Set Content
    DATA(lo_active_content) = lo_xco_database_table->content( ).
    "rs_data-content = lo_active_content->get( ).

    DATA(lo_fields_factory) = lo_xco_database_table->fields.
    DATA(lt_field_names) = lo_fields_factory->all->get_names( ).

    LOOP AT lt_field_names
         ASSIGNING FIELD-SYMBOL(<lv_field_name>).

      DATA(lo_field) = lo_xco_database_table->field( <lv_field_name> ).

      APPEND INITIAL LINE TO rs_data-fields
             ASSIGNING FIELD-SYMBOL(<ls_field>).

      <ls_field>-name = <lv_field_name>.

      DATA(lo_field_content) = lo_field->content( ).
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(ls_field_content) = lo_field_content->get( ).

*      short_description  TYPE tv_short_description,
*      key_indicator      TYPE abap_bool,
*      not_null           TYPE abap_bool,
*      type               TYPE REF TO if_xco_dbt_field_type,
*      language_indicator TYPE abap_bool,
*      currency_quantity  TYPE if_xco_tab_field_content=>ts_currency_quantity,
*      geodata            TYPE if_xco_tab_field_content=>ts_geodata,

      " Get each attribute individually.
      <ls_field>-short_description = ls_field_content-short_description.
      <ls_field>-key_indicator     = lo_field_content->get_key_indicator( ).
      <ls_field>-not_null          = lo_field_content->get_not_null( ).

      DATA(lo_type) = lo_field_content->get_type( ).

      IF lo_type->is_data_element( ) = abap_true.
        <ls_field>-type_category = cs_type_category-data_element.

        DATA(lo_data_element) = lo_type->get_data_element( ).
        <ls_field>-data_element_name = lo_data_element->if_xco_ar_object~name->value.

      ELSE.

        <ls_field>-type_category = cs_type_category-built_in_type.

        DATA(lo_built_in_type) = lo_type->get_built_in_type( ).
        <ls_field>-built_in_type-type     = lo_built_in_type->type.
        <ls_field>-built_in_type-length   = lo_built_in_type->length.
        <ls_field>-built_in_type-decimals = lo_built_in_type->decimals.

      ENDIF.

      DATA(ls_reference_field) = lo_field_content->get_currency_quantity( ).
      <ls_field>-reference_field-table_name = ls_reference_field-reference_table.
      <ls_field>-reference_field-field_name = ls_reference_field-reference_field.

    ENDLOOP.

  ENDMETHOD.



ENDCLASS.
