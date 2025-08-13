CLASS z_xco_cds_projection_view DEFINITION
  INHERITING FROM z_xco_cds_view_abstract
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_data_source,
        type       TYPE sxco_ar_object_type,
        name       TYPE tv_cds_view_name,
        alias_name TYPE sxco_ddef_alias_name,
      END OF ts_data_source.

    TYPES:
      BEGIN OF ts_association,
        name                TYPE string,
        alias_name          TYPE sxco_ddef_alias_name,
        cardinality         TYPE if_xco_cds_association_content=>ts_cardinality,
        condition           TYPE REF TO if_xco_ddl_expr_condition,
        target              TYPE sxco_cds_object_name,
        to_parent_indicator TYPE abap_bool,
      END OF ts_association,
      tt_associations TYPE STANDARD TABLE OF ts_association WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_field,
        key_indicator TYPE abap_bool,
        name          TYPE sxco_cds_object_name,
        alias         TYPE sxco_ddef_alias_name,
        expression    TYPE REF TO if_xco_ddl_expression,
      END OF ts_field,
      tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name              TYPE tv_cds_view_name,
        short_description TYPE sxco_ar_short_description,

        annotations       TYPE z_xco_cds_annotation_converter=>tt_annotations,

        root_indicator    TYPE abap_boolean,
        provider_contract TYPE REF TO if_xco_gen_ddls_prvdr_cntrct,

        data_source       TYPE ts_data_source,
        associations      TYPE tt_associations,
        fields            TYPE tt_fields,
      END OF ts_data.

    CLASS-METHODS get_instance
      IMPORTING iv_projection_view_name   TYPE tv_cds_view_name
      RETURNING VALUE(ro_projection_view) TYPE REF TO z_xco_cds_projection_view.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.
    METHODS: get_key REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_projection_view_name TYPE tv_cds_view_name.

ENDCLASS.



CLASS z_xco_cds_projection_view IMPLEMENTATION.

  METHOD get_instance.

    ro_projection_view = NEW #( ).
    ro_projection_view->gv_projection_view_name = iv_projection_view_name.

  ENDMETHOD.

  METHOD get_data.

    DATA(lo_projection_view) = xco_cp_cds=>projection_view( gv_projection_view_name ).

    DATA(lo_data_definition) = lo_projection_view->get_data_definition( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Annotations
    "TODO: how can annotations be read?
*    DATA(lo_ann_selection_builder) = lo_projection_view->if_xco_cds_ann_target~create_selection_builder(
*      xco_cp_cds=>annotations->direct ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Header
    DATA(lo_content) = lo_projection_view->content( ).
    DATA(ls_content) = lo_content->get( ).

    rs_data-name              = lo_projection_view->name.
    rs_data-short_description = ls_content-short_description.
    rs_data-root_indicator    = ls_content-root_indicator.
    rs_data-provider_contract = lo_content->get_provider_contract( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Data Source
    DATA(ls_data_source) = ls_content-data_source.
    rs_data-data_source-type = _get_object_type( ls_data_source-view_entity ).
    rs_data-data_source-name = ls_data_source-view_entity.
    rs_data-data_source-alias_name = ls_data_source-alias.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Associations
    DATA(lt_read_assocations) = lo_projection_view->associations->all->get( ).
    LOOP AT lt_read_assocations
      ASSIGNING FIELD-SYMBOL(<lo_read_association>).

      APPEND
        VALUE #(
          name = <lo_read_association>->entity->name )
        TO rs_data-associations
        ASSIGNING FIELD-SYMBOL(<ls_target_association>).

      DATA(ls_association_content) = <lo_read_association>->content( )->get( ).

      <ls_target_association>-alias_name = ls_association_content-alias.
      <ls_target_association>-cardinality = ls_association_content-cardinality.
      <ls_target_association>-condition = ls_association_content-condition.
      <ls_target_association>-target = ls_association_content-target.
      <ls_target_association>-to_parent_indicator = ls_association_content-to_parent_indicator.

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Associations
    DATA(lt_fields) = lo_projection_view->fields->all->get( ).
    LOOP AT lt_fields
      ASSIGNING FIELD-SYMBOL(<lo_field>).

      DATA(ls_field_content) = <lo_field>->content( )->get( ).

      APPEND
        VALUE #(
          name = <lo_field>->name )
        TO rs_data-fields
        ASSIGNING FIELD-SYMBOL(<ls_target_field>).

      <ls_target_field>-alias = ls_field_content-alias.
      <ls_target_field>-key_indicator = ls_field_content-key_indicator.
*ls_field_content-association
*ls_field_content-composition
      <ls_target_field>-expression = ls_field_content-expression.
*ls_field_content-localized_indicator
*ls_field_content-original_name
*ls_field_content-redirected_to
*ls_field_content-redirected_to_compos_child
*ls_field_content-redirected_to_parent
*ls_field_content-type
*ls_field_content-virtual_indicator

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Parameters
    "lo_view_entity->parameters


  ENDMETHOD.

  METHOD get_key.

    rs_key = VALUE #(
      type = 'DDLS'
      name = gv_projection_view_name ).

  ENDMETHOD.

ENDCLASS.
