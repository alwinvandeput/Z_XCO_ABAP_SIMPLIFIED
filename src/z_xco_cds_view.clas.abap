CLASS z_xco_cds_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_data_definition_type TYPE c LENGTH 20.
    TYPES tv_value_item_type TYPE c LENGTH 20.

    TYPES:
      BEGIN OF ts_value_item,
        type  TYPE tv_value_item_type,
        value TYPE string,
      END OF ts_value_item,
      tt_value_items TYPE STANDARD TABLE OF ts_value_item WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_annotation,
        name        TYPE sxco_cds_ann_property,
        value_items TYPE tt_value_items,
      END OF ts_annotation,
      tt_annotations TYPE STANDARD TABLE OF ts_annotation WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data_source,
        name       TYPE sxco_cds_object_name,
        alias_name TYPE sxco_ddef_alias_name,
      END OF ts_data_source.

    TYPES:
      BEGIN OF ts_field,
        key_indicator TYPE abap_boolean,
        name          TYPE sxco_cds_field_name,
        alias_name    TYPE sxco_ddef_alias_name,
        annotations   TYPE tt_annotations,
      END OF ts_field,
      tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name                 TYPE sxco_cds_object_name,
        short_description    TYPE sxco_ar_short_description,
        annotations          TYPE tt_annotations,

        data_definition_type TYPE tv_data_definition_type,

        root_ind             TYPE abap_boolean,

        data_source          TYPE ts_data_source,

        fields               TYPE tt_fields,
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
      BEGIN OF cs_value_item_type,
        begin_array  TYPE tv_value_item_type VALUE 'begin_array',
        end_array    TYPE tv_value_item_type VALUE 'end_array',
        begin_record TYPE tv_value_item_type VALUE 'begin_array',
        end_record   TYPE tv_value_item_type VALUE 'end_array',
        value        TYPE  tv_value_item_type VALUE '',
      END OF cs_value_item_type.


    CLASS-METHODS create_or_update_instance
      IMPORTING is_create          TYPE ts_create
      RETURNING VALUE(ro_cds_view) TYPE REF TO z_xco_cds_view.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_ddls_name TYPE string.

ENDCLASS.



CLASS z_xco_cds_view IMPLEMENTATION.

  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system(
      is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-ddls->add_object( is_create-cds_view_data-name ).

    lo_put_object->set_package( is_create-package ).

    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set CDS Definition Type = View Entity
    CASE is_create-cds_view_data-data_definition_type.

      WHEN cs_data_definition_type-view_entity.
        DATA(lo_view) = lo_specification->add_view_entity( ).

      WHEN OTHERS.
        ASSERT 1 = 0.

    ENDCASE.

    lo_specification->set_short_description( is_create-cds_view_data-short_description ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Header Level Annotations
    LOOP AT is_create-cds_view_data-annotations
      ASSIGNING FIELD-SYMBOL(<ls_annotation>).

      DATA(lo_annotation) = lo_view->add_annotation( <ls_annotation>-name ).

      data(lo_build) = lo_annotation->value->build( ).

      LOOP AT <ls_annotation>-value_items
        ASSIGNING FIELD-SYMBOL(<lv_value_item>).

        CASE <lv_value_item>-type.

          WHEN cs_value_item_type-begin_array.

            lo_build->begin_array( ).

          WHEN cs_value_item_type-end_array.

            lo_build->end_array( ).

          WHEN cs_value_item_type-begin_record.

            lo_build->begin_record( ).

          WHEN cs_value_item_type-end_record.

            lo_build->end_record( ).

          WHEN cs_value_item_type-value.

            IF <lv_value_item>-value+0(1) = '#'.
              "Enum

              DATA(lv_enum_value) = <lv_value_item>-value+1.
              lo_build->add_enum( lv_enum_value ).

            ELSEIF <lv_value_item>-value = 'true' OR <lv_value_item>-value = 'false'.
              "Boolean

              DATA(lv_boolean_value) = COND abap_boolean(
                WHEN <lv_value_item>-value = 'true' THEN abap_true
                WHEN <lv_value_item>-value = 'false' THEN abap_false ).
              lo_build->add_boolean( lv_boolean_value ).

            ELSEIF <lv_value_item>-value+0(1) <> |'|.

              DATA(lv_number_value) = CONV decfloat34( <lv_value_item>-value ).
              lo_build->add_number( lv_number_value ).

            ELSE.
              "String

              "TODO: can a string contain ' itself?
              DATA(lv_string_value) = <lv_value_item>-value.
              REPLACE ALL OCCURRENCES OF |'| IN lv_string_value WITH ||.
              lo_build->add_string( lv_string_value ).

            ENDIF.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Root Indicator
    IF is_create-cds_view_data-root_ind = abap_true.
      lo_view->set_root( ).
    ENDIF.

*    ELSE.
*
*      CASE io_rap_bo_node->get_implementation_type(  ) .
*        WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*
*          DATA(parent_uuid_cds_field_name) = io_rap_bo_node->lt_fields[ name = io_rap_bo_node->field_name-parent_uuid ]-cds_view_field.
*          DATA(uuid_cds_field_name_in_parent) = io_rap_bo_node->parent_node->lt_fields[ name = io_rap_bo_node->parent_node->field_name-uuid ]-cds_view_field.
*
*          DATA(lo_condition) = xco_cp_ddl=>field( parent_uuid_cds_field_name )->of_projection( )->eq(
*            xco_cp_ddl=>field( uuid_cds_field_name_in_parent )->of( '_' && io_rap_bo_node->parent_node->rap_node_objects-alias ) ).
*
*
*
*
*        WHEN  ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic OR ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*
*          CLEAR ls_condition_components.
*          CLEAR lt_condition_components.
*
*          LOOP AT io_rap_bo_node->parent_node->semantic_key INTO DATA(ls_semantic_key).
*            ls_condition_components-association_name = '_' && io_rap_bo_node->parent_node->rap_node_objects-alias.
*            ls_condition_components-association_field = ls_semantic_key-cds_view_field.
*            ls_condition_components-projection_field = ls_semantic_key-cds_view_field.
*            APPEND ls_condition_components TO lt_condition_components.
*          ENDLOOP.
*
*          lo_condition = create_condition( lt_condition_components ).
*
*      ENDCASE.
*
*      "@todo - raise an exception when being initial
*      IF lo_condition IS NOT INITIAL.
*
*        lo_view->add_association( io_rap_bo_node->parent_node->rap_node_objects-cds_view_r )->set_to_parent(
*    )->set_alias( '_' && io_rap_bo_node->parent_node->rap_node_objects-alias
*    )->set_condition( lo_condition ).
*
*      ENDIF.
*
*
*
*      IF io_rap_bo_node->is_grand_child_or_deeper(  ).
*
*        CASE io_rap_bo_node->get_implementation_type(  ) .
*          WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*
*            DATA(root_uuid_cds_field_name) = io_rap_bo_node->lt_fields[ name = io_rap_bo_node->field_name-root_uuid ]-cds_view_field.
*            DATA(uuid_cds_field_name_in_root) = io_rap_bo_node->root_node->lt_fields[ name = io_rap_bo_node->root_node->field_name-uuid ]-cds_view_field.
*
*            lo_condition = xco_cp_ddl=>field( root_uuid_cds_field_name )->of_projection( )->eq(
*              xco_cp_ddl=>field( uuid_cds_field_name_in_root )->of( '_' && io_rap_bo_node->root_node->rap_node_objects-alias ) ).
*
*
*
*          WHEN  ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic OR ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*
*            CLEAR ls_condition_components.
*            CLEAR lt_condition_components.
*
*            LOOP AT io_rap_bo_node->ROOT_node->semantic_key INTO DATA(ls_root_semantic_key).
*              ls_condition_components-association_name = '_' && io_rap_bo_node->root_node->rap_node_objects-alias.
*              ls_condition_components-association_field = ls_root_semantic_key-cds_view_field.
*              ls_condition_components-projection_field = ls_root_semantic_key-cds_view_field.
*              APPEND ls_condition_components TO lt_condition_components.
*            ENDLOOP.
*
*            lo_condition = create_condition( lt_condition_components ).
*
*        ENDCASE.
*
*        IF lo_condition IS NOT INITIAL.
*          lo_view->add_association( io_rap_bo_node->root_node->rap_node_objects-cds_view_r
*            )->set_alias( '_' && io_rap_bo_node->root_node->rap_node_objects-alias
*            )->set_cardinality(  xco_cp_cds=>cardinality->one
*            )->set_condition( lo_condition ).
*        ENDIF.
*
*        "@todo add an association that uses the singleton_id
*        "association [1]    to zfcal_I_CALM_ALL    as _HolidayAll     on $projection.HolidayAllID = _HolidayAll.HolidayAllID
*
*
*      ENDIF.
*
*    ENDIF.
*
*

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Data Source - Main Source

    lo_view->data_source->set_view_entity( is_create-cds_view_data-data_source-name ).
    lo_view->data_source->set_alias( is_create-cds_view_data-data_source-alias_name ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Fields
    LOOP AT is_create-cds_view_data-fields
      ASSIGNING FIELD-SYMBOL(<ls_field>).

      IF to_upper( <ls_field>-name ) = 'CLIENT'.
        CONTINUE.
      ENDIF.

      DATA(lo_field) = lo_view->add_field( xco_cp_ddl=>field( <ls_field>-name ) ).

      IF <ls_field>-key_indicator = abap_true.
        lo_field->set_key( ).
      ENDIF.

      IF <ls_field>-alias_name IS NOT INITIAL.
        lo_field->set_alias(  <ls_field>-alias_name  ).
      ENDIF.

      LOOP AT <ls_field>-annotations
        ASSIGNING FIELD-SYMBOL(<ls_field_annotation>).
*      "add @Semantics annotation for currency code
*      IF ls_header_fields-currencycode IS NOT INITIAL.
*        READ TABLE io_rap_bo_node->lt_fields INTO DATA(ls_field) WITH KEY name = to_upper( ls_header_fields-currencycode ).
*        IF sy-subrc = 0.
*          "for example @Semantics.amount.currencyCode: 'CurrencyCode'
*          lo_field->add_annotation( 'Semantics.amount.currencyCode' )->value->build( )->add_string( CONV #( ls_field-cds_view_field ) ).
*        ENDIF.
*      ENDIF.

      ENDLOOP.

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Create / Update
    lo_operation->execute( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Return Data Definition Instance
    ro_cds_view = NEW #( ).

    ro_cds_view->gv_ddls_name = is_create-cds_view_data-name.

  ENDMETHOD.

ENDCLASS.
