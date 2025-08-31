CLASS z_xco_cds_view_entity DEFINITION
  INHERITING FROM z_xco_cds_data_definition
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z_xco_generic_cds_view_if.

    TYPES tv_dummy TYPE c LENGTH 40.

    TYPES tv_data_definition_type TYPE c LENGTH 20.

    TYPES tv_value_item_type TYPE c LENGTH 20.

    TYPES tv_condition_type TYPE c LENGTH 20.

    TYPES:
      BEGIN OF ts_condition_line,
        line_type            TYPE string,
        left_alias           TYPE string,
        left_field           TYPE string,
        left_literal_string  TYPE string,
        left_literal_number  TYPE string,
        operator             TYPE string,
        right_alias          TYPE string,
        right_field          TYPE string,
        right_literal_string TYPE string,
        right_literal_number TYPE string,
        grouping             TYPE c LENGTH 1,
        logical_operator     TYPE c LENGTH 3,
      END OF ts_condition_line,
      tt_condition_lines TYPE STANDARD TABLE OF ts_condition_line WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_select_data,
        header            TYPE abap_boolean,
        underlying_object TYPE abap_boolean,
        compositions      TYPE abap_boolean,
        associations      TYPE abap_boolean,
      END OF ts_select_data.

    TYPES:
      BEGIN OF ts_data_source,
        type       TYPE sxco_ar_object_type,
        name       TYPE tv_cds_object_name,
        alias_name TYPE sxco_ddef_alias_name,
      END OF ts_data_source.

    TYPES:
      BEGIN OF ts_field,
        key_indicator TYPE abap_boolean,
        name          TYPE sxco_cds_field_name,
        alias_name    TYPE sxco_ddef_alias_name,
        annotations   TYPE z_xco_cds_annotation_converter=>tt_annotations,
      END OF ts_field,
      tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_condition,
        type  TYPE string,
        value TYPE string,
      END OF ts_condition,
      tt_conditions TYPE STANDARD TABLE OF ts_condition WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_cardinality,
        min TYPE i,
        max TYPE i,
      END OF ts_cardinality.

    TYPES:
      BEGIN OF ts_composition,
        entity_name TYPE tv_cds_object_name,
        alias_name  TYPE sxco_ddef_alias_name,
        cardinality TYPE ts_cardinality,
      END OF ts_composition,
      tt_compositions TYPE STANDARD TABLE OF ts_composition WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_association,
        entity_name    TYPE tv_cds_object_name,
        alias_name     TYPE sxco_ddef_alias_name,
        cardinality    TYPE ts_cardinality,
        condition_text TYPE string,
      END OF ts_association,
      tt_associations TYPE STANDARD TABLE OF ts_association WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name                 TYPE tv_cds_object_name,
        short_description    TYPE sxco_ar_short_description,
        annotations          TYPE z_xco_cds_annotation_converter=>tt_annotations,

*        data_definition_type TYPE tv_data_definition_type,

        root_indicator       TYPE abap_boolean,


        data_source          TYPE ts_data_source,
        compositions         TYPE tt_compositions,
        associations         TYPE tt_associations,

        fields               TYPE tt_fields,

        where_condition_text TYPE string,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        cds_view_data     TYPE ts_data,
      END OF ts_create.

    CONSTANTS:
      BEGIN OF cs_data_definition_type,
        view_entity TYPE tv_data_definition_type VALUE 'VIEW_ENTITY',
      END OF cs_data_definition_type.

    CONSTANTS:
      BEGIN OF cs_condition_type,
        add_parenthesis    TYPE tv_condition_type VALUE 'ADD_PARENTHESIS',
        remove_parenthesis TYPE tv_condition_type VALUE 'REMOVE_PARENTHESIS',
        variable           TYPE tv_condition_type VALUE 'VARIABLE',
        operator           TYPE tv_condition_type VALUE 'OPERATOR',
        string_value       TYPE tv_condition_type VALUE 'STRING_VALUE',
        numeric_value      TYPE tv_condition_type VALUE 'NUMERIC_VALUE',
        boolean_value      TYPE tv_condition_type VALUE 'BOOLEAN_VALUE',
        logical            TYPE tv_condition_type VALUE 'LOGICAL',
      END OF cs_condition_type.

    TYPES tv_field_type TYPE c LENGTH 20.

    CONSTANTS:
      BEGIN OF cs_field_type,
        expression           TYPE tv_field_type VALUE 'EXPRESSION',
        redirected_to        TYPE tv_field_type VALUE 'REDIRECTED_TO',
        redirected_to_parent TYPE tv_field_type VALUE 'REDIRECTED_TO_PARENT',
        composition          TYPE tv_field_type VALUE 'COMPOSITION',
        association          TYPE tv_field_type VALUE 'ASSOCIATION',
      END OF cs_field_type.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create          TYPE ts_create
      RETURNING VALUE(ro_cds_view) TYPE REF TO z_xco_cds_view_entity.

    CLASS-METHODS get_instance
      IMPORTING iv_cds_view_name   TYPE tv_cds_object_name
      RETURNING VALUE(ro_cds_view) TYPE REF TO z_xco_cds_view_entity.

    METHODS get_data
      IMPORTING is_select_data TYPE ts_select_data OPTIONAL
      RETURNING VALUE(rs_data) TYPE ts_data.

    METHODS: get_key REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES tv_cds_name      TYPE sxco_ar_object_name.
    TYPES tv_source_type   TYPE c LENGTH 10.

    TYPES:
      BEGIN OF ts_object_key,
        type TYPE tv_source_type,
        name TYPE tv_cds_name,
      END OF ts_object_key,
      tt_object_keys TYPE STANDARD TABLE OF ts_object_key WITH EMPTY KEY.

    DATA gv_cds_view_name TYPE tv_cds_object_name.

    METHODS _get_data_source_object
      IMPORTING is_data_source              TYPE if_xco_cds_view_entity_content=>ts_content-data_source
      RETURNING VALUE(rs_underlying_object) TYPE ts_data_source.

    METHODS _get_compositions
      IMPORTING iv_layer_no            TYPE i OPTIONAL
                io_view_entity         TYPE REF TO if_xco_cds_view_entity
      RETURNING VALUE(rt_compositions) TYPE tt_compositions.

    METHODS _get_associations
      IMPORTING io_view_entity         TYPE REF TO if_xco_cds_view_entity
      RETURNING VALUE(rt_associations) TYPE tt_associations.

    METHODS _get_fields
      IMPORTING io_view_entity   TYPE REF TO if_xco_cds_view_entity
      RETURNING VALUE(rt_fields) TYPE tt_fields.

    METHODS _get_metadata_extension
      IMPORTING iv_cds_view_name TYPE tv_cds_object_name.



    METHODS _get_condition_lines
      IMPORTING iv_condition_string       TYPE string
      RETURNING VALUE(rt_condition_lines) TYPE tt_condition_lines.

    METHODS _convert_condition_to_tokens
      IMPORTING iv_expr          TYPE string
      RETURNING VALUE(rt_tokens) TYPE string_table.

    METHODS _set_metadata_extension
      IMPORTING iv_cds_view_name TYPE tv_cds_object_name.

    METHODS _get_cardinality
      IMPORTING
        is_cardinality TYPE if_xco_cds_composition_content=>ts_content-cardinality
      CHANGING
        co_cardinality TYPE REF TO cl_xco_cds_cardinality_cn.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Create

    CLASS-METHODS _set_annotations
      IMPORTING it_annotations       TYPE z_xco_cds_annotation_converter=>tt_annotations
                io_annotation_target TYPE REF TO if_xco_gen_cds_s_fo_ann_target.

    CLASS-METHODS _set_compositions
      IMPORTING it_compositions        TYPE tt_compositions
                io_cds_data_definition TYPE REF TO if_xco_gen_ddls_s_fo_view_e.

    CLASS-METHODS _set_associations
      IMPORTING it_associations        TYPE tt_associations
                io_cds_data_definition TYPE REF TO if_xco_gen_ddls_s_fo_view_e.

    CLASS-METHODS _set_fields
      IMPORTING
        it_fields              TYPE tt_fields
        io_cds_data_definition TYPE REF TO if_xco_gen_ddls_s_fo_view_e.

    CLASS-METHODS _set_data_def_type
      IMPORTING
        io_specification              TYPE REF TO if_xco_cp_gen_ddls_s_form
        is_create                     TYPE z_xco_cds_view_entity=>ts_create
      RETURNING
        VALUE(ro_cds_data_definition) TYPE REF TO if_xco_gen_ddls_s_fo_view_e.

ENDCLASS.



CLASS z_xco_cds_view_entity IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system(
      is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-ddls->add_object( is_create-cds_view_data-name ).

    lo_put_object->set_package( is_create-package ).

    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-cds_view_data-short_description ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set CDS Definition Type = View Entity
*    DATA(lo_cds_data_definition) = _set_data_def_type(
*      io_specification = lo_specification
*      is_create        = is_create ).

    DATA(lo_cds_data_definition) = lo_specification->add_view_entity( ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Header Level Annotations
    _set_annotations(
      it_annotations       = is_create-cds_view_data-annotations
      io_annotation_target = lo_cds_data_definition ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Root Indicator
    IF is_create-cds_view_data-root_indicator = abap_true.
      lo_cds_data_definition->set_root( ).
    ENDIF.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Projection View
*    IF is_create-cds_view_data-projection_view_ind = abap_true.
*      DATA(lo_projection_view) = lo_specification->add_projection_view( ).
*
*      IF is_create-cds_view_data-provider_contract IS NOT INITIAL.
*        lo_projection_view->set_provider_contract( is_create-cds_view_data-provider_contract ).
*      ENDIF.
*    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Data Source - Main Source
    lo_cds_data_definition->data_source->set_view_entity( is_create-cds_view_data-data_source-name ).
    lo_cds_data_definition->data_source->set_alias( is_create-cds_view_data-data_source-alias_name ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Compositions
    _set_compositions(
      it_compositions = is_create-cds_view_data-compositions
      io_cds_data_definition = lo_cds_data_definition ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Associations
    _set_associations(
      it_associations        = is_create-cds_view_data-associations
      io_cds_data_definition = lo_cds_data_definition ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Fields
    _set_fields(
      it_fields              = is_create-cds_view_data-fields
      io_cds_data_definition = lo_cds_data_definition ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Set Where Clause
    DATA(lo_where_condition) = xco_cp_ddl=>expression->for_condition( is_create-cds_view_data-where_condition_text ).
    lo_cds_data_definition->set_where( lo_where_condition ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Create / Update
    lo_operation->execute( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Return Data Definition Instance
    ro_cds_view = NEW #( ).
    ro_cds_view->gv_cds_view_name = is_create-cds_view_data-name.

  ENDMETHOD.


  METHOD get_data.

    DATA(lo_view_entity) = xco_cp_cds=>view_entity( gv_cds_view_name ).


    DATA(lo_content) = lo_view_entity->content( ).
    DATA(ls_content) = lo_content->get( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Get Annotations
    " TODO

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CDS View itself
    " CL_XCO_CDS_VIEW_ENTITY - if_xco_cds_view_entity
    "TODO: The name is in capitals, not camel case.
    IF is_select_data IS INITIAL OR
       is_select_data-header = abap_true.
      rs_data-name              = lo_view_entity->name.
      rs_data-short_description = ls_content-short_description.
      rs_data-root_indicator    = ls_content-root_indicator.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Annotations
    "TODO: how can annotations be read?

*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Underlying Object
    IF is_select_data IS INITIAL OR
       is_select_data-underlying_object = abap_true.
      rs_data-data_source = _get_data_source_object(
        is_data_source = ls_content-data_source ).
    ENDIF.

*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Compositions
    IF is_select_data IS INITIAL OR
       is_select_data-compositions = abap_true.
      rs_data-compositions = _get_compositions(
        io_view_entity   = lo_view_entity ).
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Associations
    rs_data-associations = _get_associations(
      EXPORTING
        io_view_entity = lo_view_entity ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Fields
    rs_data-fields = _get_fields(
      io_view_entity = lo_view_entity ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Where - TODO: can it have more than one line?
    IF ls_content-where IS NOT INITIAL.
      DATA(lo_lines) = ls_content-where->if_xco_text~get_lines( ).
      DATA(lo_line) = lo_lines->get( 1 ).
      DATA(lv_line) = CAST if_xco_string( lo_line ).
      rs_data-where_condition_text = lv_line->value.
    ENDIF.

*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Metadata Extension
*    _set_metadata_extension(
*        iv_cds_view_name = gv_cds_view_name ).

*    DATA(ls_data_source) = ls_view_entity_content-data_source.
*    DATA(lo_data_def) = lo_view_entity->get_data_definition( ).

  ENDMETHOD.


  METHOD get_instance.

    ro_cds_view = NEW #( ).
    ro_cds_view->gv_cds_view_name = iv_cds_view_name.

  ENDMETHOD.


  METHOD get_key.

    rs_key = VALUE #(
      type = 'DDLS'
      name = gv_cds_view_name ).

  ENDMETHOD.


  METHOD z_xco_generic_cds_view_if~get_data.

    DATA(ls_data) = get_data( ).

    rs_view_abstract_data = VALUE #(
      name = ls_data-name
      data_source = VALUE #(
        type = ls_data-data_source-type
        name = ls_data-data_source-name
        alias_name = ls_data-data_source-alias_name
      )
      compositions = VALUE #(
        FOR <ls_composition> IN ls_data-compositions
          (  entity_name     = <ls_composition>-entity_name
             alias_name      = <ls_composition>-alias_name
             cardinality-min = <ls_composition>-cardinality-min
             cardinality-max = <ls_composition>-cardinality-max
          )
      )
      fields = VALUE #(
        FOR <ls_field> IN ls_data-fields
          ( name       = <ls_field>-name
            alias_name = <ls_field>-alias_name )
      )
    ).

  ENDMETHOD.


  METHOD _convert_condition_to_tokens.

    DATA lv_char  TYPE string.
    DATA lv_next  TYPE c LENGTH 1.
    DATA lv_token TYPE string.
    DATA i        TYPE i.

    DATA(lv_length) = strlen( iv_expr ).

    i = 0.
    WHILE i < lv_length.

      lv_char = iv_expr+i(1).

      i += 1.

      " Skip spaces
      IF lv_char = | |.
        CONTINUE.
      ENDIF.

      " Handle string literals
      IF lv_char = ''''.
        CLEAR lv_token.
        DO.

          CONCATENATE lv_token lv_char INTO lv_token.

          IF i = lv_length.
            EXIT.
          ENDIF.

          lv_char = iv_expr+i(1).
          IF i < lv_length.
            i += 1.
            lv_next = iv_expr+i(1).
            i -= 1.
          ELSE.
            lv_next = ''.
          ENDIF.

          IF lv_char = '\\'.
            ASSERT 1 = 1.
          ENDIF.

          IF lv_char = ''''.
            CONCATENATE lv_token lv_char INTO lv_token.
            i += 1.
            EXIT.
          ELSEIF lv_char = '\' AND lv_next = ''''.
            i += 1.
            lv_char = '\'''.
            i += 1.
          ELSE.
            i += 1.
          ENDIF.

        ENDDO.
        APPEND lv_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      " Handle two-character operators
      IF i < lv_length.
        lv_next = iv_expr+i(1).
      ELSE.
        lv_next = ''.
      ENDIF.
      IF     ( lv_char = '<' OR lv_char = '>' )
         AND ( lv_next = '=' OR lv_next = '>' ).
        CONCATENATE lv_char lv_next INTO lv_token.
        i += 1.
        APPEND lv_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      " Handle single-char operators or brackets
      IF    lv_char = '=' OR lv_char = '(' OR lv_char = ')'
         OR lv_char = '<' OR lv_char = '>' OR lv_char = '.'.
        APPEND lv_char TO rt_tokens.
        CONTINUE.
      ENDIF.

      " Accumulate word/identifier
      CLEAR lv_token.
      DO.
        IF lv_char = | |.
          EXIT.
        ENDIF.
        CASE lv_char.
          WHEN '<' OR '>' OR '=' OR '.'.
            EXIT.
        ENDCASE.
        CONCATENATE lv_token lv_char INTO lv_token.
        IF i >= lv_length.
          i += 1.
          EXIT.
        ENDIF.
        lv_char = iv_expr+i(1).
        i += 1.
      ENDDO.
      i -= 1. " Step back one character
      IF lv_token IS NOT INITIAL.
        APPEND lv_token TO rt_tokens.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD _get_associations.

    LOOP AT io_view_entity->associations->all->get( )
      INTO DATA(lo_association).

      APPEND INITIAL LINE TO rt_associations
        ASSIGNING FIELD-SYMBOL(<ls_association>).

      DATA(ls_association) = lo_association->content( )->get( ).

      <ls_association>-entity_name = ls_association-target.
*      <ls_association>-to_parent_indicator = ls_association-to_parent_indicator.
      <ls_association>-alias_name = ls_association-alias.
      <ls_association>-cardinality = VALUE #(
          min = ls_association-cardinality-min
          max = ls_association-cardinality-max ).

      DATA(lo_condition) = ls_association-condition.

      DATA(lo_expression_blueprint) = lo_condition->if_xco_gen_ddls_ddl_expression~get_blueprint( ).
      DATA(lo_condition_lines) = lo_condition->if_xco_text~get_lines( ).

      DATA(lo_condition_iterator) = lo_condition_lines->if_xco_string_iterable~get_iterator( ).
      WHILE lo_condition_iterator->has_next( ) = abap_true.
        lo_condition_iterator->next( ).

        DATA(lo_condition_string) = lo_condition_iterator->current_string.
        DATA(lv_condition_string) = lo_condition_string->value.

*        <ls_association>-filter_condition_lines = _get_condition_lines( lv_condition_string ).
        <ls_association>-condition_text = lv_condition_string.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_cardinality.

    IF is_cardinality-min = 0 AND
       is_cardinality-max = 2147483647.
      co_cardinality = xco_cp_cds=>cardinality->zero_to_n.
    ENDIF.

  ENDMETHOD.


  METHOD _get_compositions.
*
*    " The "compositions->all->get( )" read the compositions, but also the
*    " sub entities of the compositions.
*    " So we have to filter out the sub entities.
*    " TODO:
*    " ASSERT 1 = 0.
*    " TODO: DATA(lt_composition_records) = _get_composition_keys( io_view_entity ).
*
    DATA lt_temp_compositions LIKE rt_compositions.

    DATA(lt_xco_composition_objects) = io_view_entity->compositions->all->get( ).

    " TODO: Loop at lt_composition_keys
    LOOP AT lt_xco_composition_objects
         INTO DATA(lo_xco_composition).

      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(lv_index) = sy-tabix.

      " Is it from Parent to Child or Child to Parent?
      DATA(lo_entity) = lo_xco_composition->entity.

      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(lv_parent_cds_name) = lo_entity->if_xco_ar_object~name->value.

      DATA(lo_composition_content) = lo_xco_composition->content( ).

      DATA(ls_composition_content) = lo_composition_content->get( ).

      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(ls_cardinality) = ls_composition_content-cardinality.

*      <ls_output_composition>-entity_name = <lo_composition>->target.
*      <ls_output_composition>-alias_name = ls_composition_content-alias.

      APPEND INITIAL LINE TO lt_temp_compositions
             ASSIGNING FIELD-SYMBOL(<ls_temp_composition>).
*      <ls_temp_composition>-type         = |PARENT_TO_CHILD|.
      <ls_temp_composition>-entity_name  = lo_xco_composition->target.
      <ls_temp_composition>-alias_name   = ls_composition_content-alias.

      "TODO: add all cardinalities
      <ls_temp_composition>-cardinality = VALUE #(
        min = ls_composition_content-cardinality-min
        max = ls_composition_content-cardinality-max ).

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Add only direct compositions
    " The XCO Library also reads of the composition object of the composition objects.
    " That is not logical. We only want it's own composition objects.

    " Get composition underlying object names

    DATA(lo_repo_object_fact) = z_xco_repo_object_factory=>get_instance( iv_buffering_ind = abap_true ).

    TYPES:
      BEGIN OF ts_info_composition,
        object_name            TYPE c LENGTH 30,
        underlying_object_name TYPE c LENGTH 30,
      END OF ts_info_composition.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_info_compositions TYPE STANDARD TABLE OF ts_info_composition WITH EMPTY KEY.

    LOOP AT lt_temp_compositions
         ASSIGNING <ls_temp_composition>.

      DATA(lo_repo_object) = lo_repo_object_fact->get_repository_object(
          iv_object_type = 'DDLS'
          iv_object_name = CONV #( <ls_temp_composition>-entity_name ) ).

      DATA(lo_cds_view) = CAST z_xco_generic_cds_view_if( lo_repo_object ).

      DATA(ls_cds_view_data) = lo_cds_view->get_data(
        select_data = VALUE #( underlying_object = abap_true ) ).

      APPEND INITIAL LINE TO lt_info_compositions
             ASSIGNING FIELD-SYMBOL(<ls_info_composition>).

      <ls_info_composition>-object_name            = <ls_temp_composition>-entity_name.
      <ls_info_composition>-underlying_object_name = ls_cds_view_data-data_source-name.

    ENDLOOP.

    " Add Temporary compositions to Compositions
    LOOP AT lt_temp_compositions
      ASSIGNING <ls_temp_composition>.

      READ TABLE lt_info_compositions
        WITH KEY underlying_object_name = <ls_temp_composition>-entity_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      APPEND <ls_temp_composition> TO rt_compositions.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_condition_lines.

*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    "_Test.ITEMNO = '1' and ( _Test.ITEMNO = '2' or _Test.ITEMNO = '3' )
*    "
*    "----------------------------------------
*    " left_alias left_field operator  right_alias  right_field right_literal logical_pperator grouping
*    "_Test      itemno     =                                  '1'
*    "                                                                       and
*    "                                                                                        (
*    "_Test      itemno     =                                  '2'
*    "                                                                       or
*    "_Test      itemno     =                                  '3'
*    "                                                                                        )
*
*    DATA(lt_tokens) = _convert_condition_to_tokens( iv_condition_string ).
*
*    DATA(lv_token_count) = lines( lt_tokens ).
*
*    DATA lv_next_token TYPE string.
*
*    DATA(lv_counter) = 0.
*
*    DO.
*
*      lv_counter += 1.
*
*      IF lv_counter > lv_token_count.
*        EXIT.
*      ENDIF.
*
*      ASSIGN lt_tokens[ lv_counter ] TO FIELD-SYMBOL(<lv_token>).
*      DATA(lv_lower_case_token) = to_lower( <lv_token> ).
*
*      IF lv_lower_case_token = '(' OR lv_lower_case_token = ')'.
*        APPEND
*          VALUE #( line_type = cs_condition_line_type-grouping grouping = <lv_token> )
*          TO rt_condition_lines.
*        CONTINUE.
*      ENDIF.
*
*      IF lv_lower_case_token = 'and' OR lv_lower_case_token = 'or'.
*        APPEND
*          VALUE #( line_type = cs_condition_line_type-logical_operator logical_operator = lv_lower_case_token )
*          TO rt_condition_lines.
*        CONTINUE.
*      ENDIF.
*
*      APPEND
*        VALUE #( line_type = cs_condition_line_type-comparison )
*        TO rt_condition_lines
*        ASSIGNING FIELD-SYMBOL(<ls_comparison>).
*
*      DATA lv_status TYPE c LENGTH 15.
*      lv_status = 'Left Alias'.
*
*      DO.
*
*        CLEAR lv_next_token.
*        IF lv_counter < lv_token_count.
*          lv_next_token = lt_tokens[ lv_counter + 1 ].
*        ENDIF.
*
*        DATA(lv_first_char) = lv_lower_case_token+0(1).
*
*        CASE lv_status.
*          WHEN 'Left Alias'.
*
*            IF lv_first_char CA '0123456789'.
*              <ls_comparison>-left_literal_number = <lv_token>.
*              lv_status = 'Operator'.
*
*            ELSEIF lv_first_char CA ''''.
*              <ls_comparison>-left_literal_string = <lv_token>.
*              lv_status = 'Operator'.
*
*            ELSEIF lv_next_token = '.'.
*              <ls_comparison>-left_alias = <lv_token>.
*              lv_status = 'Left .'.
*            ELSE.
*              <ls_comparison>-left_field = <lv_token>.
*              lv_status = 'Operator'.
*            ENDIF.
*
*          WHEN 'Left .'.
*            lv_status = 'Left Field'.
*
*          WHEN 'Left Field'.
*            <ls_comparison>-left_field = <lv_token>.
*            lv_status = 'Operator'.
*
*          WHEN 'Operator'.
*            <ls_comparison>-operator = <lv_token>.
*            lv_status = 'Right Alias'.
*
*          WHEN 'Right Alias'.
*
*            IF lv_first_char CA '0123456789'.
*              <ls_comparison>-right_literal_number = <lv_token>.
*              EXIT.
*
*            ELSEIF lv_first_char CA ''''.
*              <ls_comparison>-right_literal_string = <lv_token>.
*              EXIT.
*
*            ELSEIF lv_next_token = '.'.
*              <ls_comparison>-right_alias = <lv_token>.
*              lv_status = 'Right .'.
*            ELSE.
*              <ls_comparison>-right_field = <lv_token>.
*              EXIT.
*            ENDIF.
*
*          WHEN 'Right .'.
*            lv_status = 'Right Field'.
*
*          WHEN 'Right Field'.
*            <ls_comparison>-right_field = <lv_token>.
*            EXIT.
*
*        ENDCASE.
*
*        lv_counter += 1.
*        ASSIGN lt_tokens[ lv_counter ] TO <lv_token>.
*        lv_lower_case_token = to_lower( <lv_token> ).
*
*      ENDDO.
*
*    ENDDO.

  ENDMETHOD.


  METHOD _get_data_source_object.

    rs_underlying_object-type = _get_repository_object_type( is_data_source-view_entity ).
    rs_underlying_object-name = is_data_source-view_entity.
    rs_underlying_object-alias_name = is_data_source-alias.

  ENDMETHOD.


  METHOD _get_fields.

    LOOP AT io_view_entity->fields->all->get( )
      INTO DATA(lo_field).  "CL_XCO_CDS_FIELD

      APPEND INITIAL LINE TO rt_fields
        ASSIGNING FIELD-SYMBOL(<ls_field>).

      DATA(ls_field) = lo_field->content( )->get( ).

      DATA lv_field_type TYPE tv_field_type.

      IF ls_field-expression IS NOT INITIAL.

        lv_field_type = cs_field_type-expression.

        CASE TYPE OF ls_field-expression.

          WHEN TYPE if_xco_ddl_expr_field.

            DATA(lo_xco_ddl_expr_field) = CAST if_xco_ddl_expr_field( ls_field-expression ).

            DATA(lo_expression) = CAST if_xco_gen_ddls_ddl_expression( ls_field-expression ).
            DATA(lo_blueprint) = lo_expression->get_blueprint( ).

            DATA lo_strings TYPE REF TO if_xco_strings.
            lo_strings = ls_field-expression->if_xco_text~get_lines( ).
            IF lo_strings IS NOT INITIAL.
              DATA(lt_source_field_names) = lo_strings->value.
              DATA(lt_expression_lines) = lt_source_field_names.
              IF lines( lt_source_field_names ) = 1.
                DATA(lv_source_field_name) = lt_source_field_names[ 1 ].
              ELSE.
                "TODO
                ASSERT 1 = 1.
              ENDIF.
            ENDIF.

          WHEN OTHERS.

            lo_blueprint = ls_field-expression->if_xco_gen_ddls_ddl_expression~get_blueprint( ).

            lo_strings = ls_field-expression->if_xco_text~get_lines( ).
            IF lo_strings IS NOT INITIAL.
              lt_source_field_names = lo_strings->value.
              lt_expression_lines = lt_source_field_names.
              IF lines( lt_source_field_names ) = 1.
                lv_source_field_name = lt_source_field_names[ 1 ].
              ELSE.
                "TODO
                ASSERT 1 = 1.
              ENDIF.
            ENDIF.

        ENDCASE.

      ENDIF.

      IF ls_field-redirected_to IS NOT INITIAL.
        lv_field_type = cs_field_type-redirected_to.
        "TODO
        ASSERT 1 = 1.
      ENDIF.

      IF ls_field-redirected_to_parent IS NOT INITIAL.
        lv_field_type = cs_field_type-redirected_to_parent.
        "TODO
        ASSERT 1 = 1.
      ENDIF.

      IF ls_field-composition IS NOT INITIAL.
        lv_field_type = cs_field_type-composition.
        "TODO
        ASSERT 1 = 1.
      ENDIF.

      IF ls_field-association IS NOT INITIAL.
        lv_field_type = cs_field_type-association.
        "TODO
        ASSERT 1 = 1.

      ENDIF.

      <ls_field>-name = lv_source_field_name.
      <ls_field>-key_indicator = ls_field-key_indicator.
      <ls_field>-alias_name = ls_field-alias.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_metadata_extension.

    "CL_XCO_METADATA_EXTENSION - if_xco_metadata_extension
    DATA(lo_metadata_extension) = xco_cp_cds=>metadata_extension( iv_cds_view_name ).
    IF lo_metadata_extension->exists( ) = abap_true.

      "if_xco_metadata_extension has no method CONTENT
      "data(lo_md_content) = lo_metadata_extension->content( ).

      "Name:
      DATA lo TYPE REF TO if_xco_ar_object.
      lo = CAST #( lo_metadata_extension ).
      DATA(lv_name) = lo->name.
      DATA(lt_lines) = lo_metadata_extension->if_xco_printable~get_text( )->get_lines( )->value.
    ENDIF.

  ENDMETHOD.


  METHOD _set_annotations.

    NEW z_xco_cds_annotation_converter( )->convert_annotations(
      it_annotations       = it_annotations
      io_annotation_target = io_annotation_target ).

  ENDMETHOD.


  METHOD _set_associations.

    LOOP AT it_associations
      ASSIGNING FIELD-SYMBOL(<ls_assocation>).

      DATA(lo_association) = io_cds_data_definition->add_association( <ls_assocation>-entity_name ).
      lo_association->set_alias( <ls_assocation>-alias_name ).

      DATA(lo_cardinality) = xco_cp_cds=>cardinality->range(
        iv_min        = <ls_assocation>-cardinality-min
        iv_max        = <ls_assocation>-cardinality-max ).
      lo_association->set_cardinality( lo_cardinality ).

      DATA(lo_association_condition) = xco_cp_ddl=>expression->for_condition( <ls_assocation>-condition_text ).
      lo_association->set_condition( lo_association_condition ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_compositions.

    LOOP AT it_compositions
      ASSIGNING FIELD-SYMBOL(<ls_composition>).

      DATA(lo_composition) = io_cds_data_definition->add_composition( <ls_composition>-entity_name ).
      lo_composition->set_alias( <ls_composition>-alias_name ).


      DATA(lo_cardinality) = xco_cp_cds=>cardinality->range(
        iv_min        = <ls_composition>-cardinality-min
        iv_max        = <ls_composition>-cardinality-max ).

      lo_composition->set_cardinality( lo_cardinality ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_data_def_type.

*    CASE is_create-cds_view_data-data_definition_type.
*
*      WHEN cs_data_definition_type-view_entity.
*        ro_cds_data_definition  = io_specification->add_view_entity( ).
*
*      WHEN OTHERS.
*        ASSERT 1 = 0.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD _set_fields.

    LOOP AT it_fields
         ASSIGNING FIELD-SYMBOL(<ls_field>).

      IF to_upper( <ls_field>-name ) = 'CLIENT'.
        CONTINUE.
      ENDIF.

      DATA(lo_field) = io_cds_data_definition->add_field( xco_cp_ddl=>field( <ls_field>-name ) ).

      IF <ls_field>-key_indicator = abap_true.
        lo_field->set_key( ).
      ENDIF.

      IF <ls_field>-alias_name IS NOT INITIAL.
        lo_field->set_alias(  <ls_field>-alias_name  ).
      ENDIF.

      _set_annotations(
        it_annotations       = <ls_field>-annotations
        io_annotation_target = lo_field ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_metadata_extension.

    "CL_XCO_METADATA_EXTENSION - if_xco_metadata_extension
    DATA(lo_metadata_extension) = xco_cp_cds=>metadata_extension( iv_cds_view_name ).
    IF lo_metadata_extension->exists( ) = abap_true.

      "if_xco_metadata_extension has no method CONTENT
      "data(lo_md_content) = lo_metadata_extension->content( ).

      "Name:
      DATA lo TYPE REF TO if_xco_ar_object.
      lo = CAST #( lo_metadata_extension ).
      DATA(lv_name) = lo->name.
      DATA(lt_lines) = lo_metadata_extension->if_xco_printable~get_text( )->get_lines( )->value.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
