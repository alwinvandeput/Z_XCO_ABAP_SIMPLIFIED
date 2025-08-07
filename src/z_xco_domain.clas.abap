CLASS z_xco_domain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_domain_name TYPE sxco_ad_object_name.

    TYPES:
      BEGIN OF ts_fixed_value,
        lower_limit TYPE if_xco_gen_doma_s_fo_fxd_value=>tv_lower_limit,
        upper_limit TYPE if_xco_gen_doma_s_fo_fxd_value=>tv_upper_limit,
        description TYPE if_xco_gen_doma_s_fo_fxd_value=>tv_description,
      END OF ts_fixed_value,
      tt_fixed_values TYPE STANDARD TABLE OF ts_fixed_value WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_built_in_type,
        type     TYPE cl_xco_ad_built_in_type=>tv_type,
        length   TYPE cl_xco_ad_built_in_type=>tv_length,
        decimals TYPE cl_xco_ad_built_in_type=>tv_decimals,
      END OF ts_built_in_type.

    TYPES:
      BEGIN OF ts_data,
        name              TYPE sxco_ad_object_name,
        short_description TYPE if_xco_domain_content=>ts_content-short_description,
        built_in_type     TYPE ts_built_in_type,
        fixed_values      TYPE tt_fixed_values,
        case_sensitive    TYPE abap_boolean,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        domain_data       TYPE ts_data,
      END OF ts_create.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "TODO: verplaatsen naar een generieke klasse ZZAP_DDIC_DATA_TYPE
    TYPES tv_data_type TYPE c LENGTH 4.
    CONSTANTS:
      BEGIN OF cs_data_type,
        char TYPE tv_data_type VALUE 'CHAR',
        dec  TYPE tv_data_type VALUE 'DEC',

*    DATA:
*      int1       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      int2       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      int4       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      int8       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      decfloat16 TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      df16_raw   TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      decfloat34 TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      df34_raw   TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      fltp       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      datn       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      dats       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      timn       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      tims       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      accp       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      utclong    TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      clnt       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      lang       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      geom_ewkb  TYPE REF TO cl_xco_ad_built_in_type READ-ONLY,
*      cuky       TYPE REF TO cl_xco_ad_built_in_type READ-ONLY.

      END OF cs_data_type.

    CLASS-METHODS get_instance
      IMPORTING iv_domain_name      TYPE tv_domain_name
      RETURNING VALUE(ro_domain_bo) TYPE REF TO z_xco_domain.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create           TYPE ts_create
      RETURNING VALUE(ro_domain_bo) TYPE REF TO z_xco_domain.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

    METHODS check_exists
      RETURNING VALUE(rv_exists) TYPE abap_boolean.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_domain_name TYPE tv_domain_name.

    METHODS delete
      IMPORTING iv_transport_request TYPE sxco_transport.

ENDCLASS.



CLASS z_xco_domain IMPLEMENTATION.

  METHOD get_instance.

    ro_domain_bo = NEW #( ).

    ro_domain_bo->gv_domain_name = iv_domain_name.

  ENDMETHOD.

  METHOD get_data.

    DATA(lo_xco_domain) = xco_cp_abap_dictionary=>domain(
      gv_domain_name ).

    IF lo_xco_domain->exists( ) = abap_false.
      RETURN.
    ENDIF.

    rs_data-name = gv_domain_name.

    IF gv_domain_name = 'MANDT'.
      RETURN.
    ENDIF.

    DATA(ls_xco_domain_content) = lo_xco_domain->content( )->get( ).
    rs_data-short_description = ls_xco_domain_content-short_description.

    "DATA(lo_blueprint) = lo_format->if_xco_gen_doma_format~get_blueprint( ).

    DATA(lo_format) = ls_xco_domain_content-format.
    IF lo_format->is_built_in_type( ) = abap_false.
      ASSERT 1 = 0.
    ENDIF.

    DATA(lo_type) = lo_format->get_built_in_type( ).
    rs_data-built_in_type-type = lo_type->type.
    rs_data-built_in_type-length = lo_type->length.
    rs_data-built_in_type-decimals = lo_type->decimals.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Output Characters
    rs_data-case_sensitive = ls_xco_domain_content-output_characteristics-case_sensitive.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Fixed Values
    DATA(lo_fixed_values) = lo_xco_domain->fixed_values.
    IF lo_fixed_values IS NOT INITIAL.

      DATA(lo_all_fixed_values) = lo_fixed_values->all.
      DATA(lt_fixed_values) = lo_all_fixed_values->get( ).

      LOOP AT lt_fixed_values
        ASSIGNING FIELD-SYMBOL(<lo_fixed_value>). "CL_XCO_DOMAIN_FIXED_VALUE

        APPEND INITIAL LINE TO rs_data-fixed_values
          ASSIGNING FIELD-SYMBOL(<ls_fixed_value>).

        DATA(lo2) = CAST if_xco_domain_fixed_value( <lo_fixed_value> ).
        <ls_fixed_value>-lower_limit = lo2->lower_limit.
        DATA(lo_fixed_value_content) = <lo_fixed_value>->content( ).
        DATA(ls_fixed_value_content) = lo_fixed_value_content->get( ).
        <ls_fixed_value>-upper_limit = ls_fixed_value_content-upper_limit.
        <ls_fixed_value>-description = ls_fixed_value_content-description.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_domain_put_object) = lo_operation->for-doma->add_object( iv_name = is_create-domain_data-name ).

    lo_domain_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_domain_put_object->create_form_specification( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Description
    lo_specification->set_short_description( is_create-domain_data-short_description ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Type
    DATA lo_domain_format TYPE REF TO if_xco_gen_doma_format.

    CASE is_create-domain_data-built_in_type-type.

      WHEN cs_data_type-dec.
        lo_domain_format = xco_cp_abap_dictionary=>built_in_type->dec(
           iv_length   = is_create-domain_data-built_in_type-length
           iv_decimals = is_create-domain_data-built_in_type-decimals ).

      WHEN cs_data_type-char.
        lo_domain_format = xco_cp_abap_dictionary=>built_in_type->char(
          iv_length = is_create-domain_data-built_in_type-length ).

      WHEN OTHERS.

        " TODO: implement other data types
        ASSERT 1 = 0.
        " lo_domain_format = xco_cp_abap_dictionary=>built_in_type->???

    ENDCASE.

    lo_specification->set_format( lo_domain_format ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Output Characteristics
    lo_specification->output_characteristics->set_case_sensitive( is_create-domain_data-case_sensitive ).
*    lo_specification->output_characteristics->set_am_pm_format
*    lo_specification->output_characteristics->set_conversion_routine
*    lo_specification->output_characteristics->set_output_length
*    lo_specification->output_characteristics->set_output_style
*    lo_specification->output_characteristics->set_sign

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Fixed Values
    LOOP AT is_create-domain_data-fixed_values
         ASSIGNING FIELD-SYMBOL(<ls_fixed_value>).

      DATA(lo_fixed_value) = lo_specification->fixed_values->add_fixed_value(
          iv_lower_limit = <ls_fixed_value>-lower_limit ).

      lo_fixed_value->set_upper_limit( <ls_fixed_value>-upper_limit ).
      lo_fixed_value->set_description( <ls_fixed_value>-description ).

    ENDLOOP.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lo_result) = lo_operation->execute( ).

    IF lo_result->findings->contain_errors( ) = abap_true.
      DATA(lt_findings) = lo_result->findings->get( ).
      ASSERT 1 = 0.
    ENDIF.

    ro_domain_bo = NEW #( ).
    ro_domain_bo->gv_domain_name = is_create-domain_data-name.

  ENDMETHOD.

  METHOD delete.

    "There is no code for deleting domains.

  ENDMETHOD.

  METHOD check_exists.

    DATA(lo_xco_domain) = xco_cp_abap_dictionary=>domain(
      gv_domain_name ).

    rv_exists = lo_xco_domain->exists( ).

  ENDMETHOD.

ENDCLASS.
