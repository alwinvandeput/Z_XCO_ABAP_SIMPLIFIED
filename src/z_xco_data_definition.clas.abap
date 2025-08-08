CLASS z_xco_data_definition DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_data_definition_type TYPE c LENGTH 20.

    TYPES tv_value_item_type TYPE c LENGTH 20.

    TYPES tv_condition_type TYPE c LENGTH 20.

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
      BEGIN OF ts_condition,
        type  TYPE string,
        value TYPE string,
      END OF ts_condition,
      tt_conditions TYPE STANDARD TABLE OF ts_condition WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_composition,
        entity_name TYPE sxco_cds_object_name,
        alias_name  TYPE sxco_ddef_alias_name,
        cardinality TYPE REF TO cl_xco_cds_cardinality_cn,
      END OF ts_composition,
      tt_compositions TYPE STANDARD TABLE OF ts_composition WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_association,
        entity_name    TYPE sxco_cds_object_name,
        alias_name     TYPE sxco_ddef_alias_name,
        cardinality    TYPE REF TO cl_xco_cds_cardinality_cn,
        condition_text TYPE string,
      END OF ts_association,
      tt_associations TYPE STANDARD TABLE OF ts_association WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name                 TYPE sxco_cds_object_name,
        short_description    TYPE sxco_ar_short_description,
        annotations          TYPE tt_annotations,

        data_definition_type TYPE tv_data_definition_type,

        root_ind             TYPE abap_boolean,

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
      BEGIN OF cs_value_item_type,
        begin_array   TYPE tv_value_item_type VALUE 'BEGIN_ARRAY',
        end_array     TYPE tv_value_item_type VALUE 'END_ARRAY',
        begin_record  TYPE tv_value_item_type VALUE 'BEGIN_RECORD',
        end_record    TYPE tv_value_item_type VALUE 'END_RECORD',
        enum_value    TYPE tv_value_item_type VALUE 'ENUM_VALUE',
        boolean_value TYPE tv_value_item_type VALUE 'BOOLEAN_VALUE',
        number_value  TYPE tv_value_item_type VALUE 'NUMBER_VALUE',
        string_value  TYPE tv_value_item_type VALUE 'STRING_VALUE',
      END OF cs_value_item_type.


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

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create          TYPE ts_create
      RETURNING VALUE(ro_cds_view) TYPE REF TO z_xco_data_definition.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_ddls_name TYPE string.

    CLASS-METHODS _add_annotations
      IMPORTING it_annotations       TYPE tt_annotations
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
        is_create                     TYPE z_xco_data_definition=>ts_create
      RETURNING
        VALUE(ro_cds_data_definition) TYPE REF TO if_xco_gen_ddls_s_fo_view_e.

ENDCLASS.


CLASS z_xco_data_definition IMPLEMENTATION.

  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system(
      is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-ddls->add_object( is_create-cds_view_data-name ).

    lo_put_object->set_package( is_create-package ).

    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-cds_view_data-short_description ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set CDS Definition Type = View Entity
    DATA(lo_cds_data_definition) = _set_data_def_type(
      io_specification = lo_specification
      is_create        = is_create ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Header Level Annotations
    _add_annotations(
      it_annotations       = is_create-cds_view_data-annotations
      io_annotation_target = lo_cds_data_definition ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set Root Indicator
    IF is_create-cds_view_data-root_ind = abap_true.
      lo_cds_data_definition->set_root( ).
    ENDIF.

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
    ro_cds_view->gv_ddls_name = is_create-cds_view_data-name.

  ENDMETHOD.


  METHOD _add_annotations.

    LOOP AT it_annotations
      ASSIGNING FIELD-SYMBOL(<ls_annotation>).

      DATA(lo_annotation) = io_annotation_target->add_annotation( <ls_annotation>-name ).

      DATA(lo_build) = lo_annotation->value->build( ).

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

          WHEN cs_value_item_type-enum_value.

            lo_build->add_enum( <lv_value_item>-value ).

          WHEN cs_value_item_type-boolean_value.

            DATA(lv_boolean_value) = COND abap_boolean(
              WHEN <lv_value_item>-value = 'true' THEN abap_true
              WHEN <lv_value_item>-value = 'false' THEN abap_false ).
            lo_build->add_boolean( lv_boolean_value ).

          WHEN cs_value_item_type-number_value.

            DATA(lv_number_value) = CONV decfloat34( <lv_value_item>-value ).
            lo_build->add_number( lv_number_value ).

          WHEN cs_value_item_type-string_value.
            "String

            "TODO: can a string contain ' itself?
            DATA(lv_string_value) = <lv_value_item>-value.
            REPLACE ALL OCCURRENCES OF |'| IN lv_string_value WITH ||.
            lo_build->add_string( lv_string_value ).

          WHEN OTHERS.
            ASSERT 1 = 0.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_compositions.

    LOOP AT it_compositions
      ASSIGNING FIELD-SYMBOL(<ls_composition>).

      DATA(lo_composition) = io_cds_data_definition->add_composition( <ls_composition>-entity_name ).
      lo_composition->set_alias( <ls_composition>-alias_name ).
      lo_composition->set_cardinality( <ls_composition>-cardinality ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _set_associations.

    LOOP AT it_associations
      ASSIGNING FIELD-SYMBOL(<ls_assocation>).

      DATA(lo_association) = io_cds_data_definition->add_association( <ls_assocation>-entity_name ).
      lo_association->set_alias( <ls_assocation>-alias_name ).
      lo_association->set_cardinality( <ls_assocation>-cardinality ).
      DATA(lo_association_condition) = xco_cp_ddl=>expression->for_condition( <ls_assocation>-condition_text ).
      lo_association->set_condition( lo_association_condition ).

    ENDLOOP.

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

      _add_annotations(
        it_annotations       = <ls_field>-annotations
        io_annotation_target = lo_field ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_data_def_type.

    CASE is_create-cds_view_data-data_definition_type.

      WHEN cs_data_definition_type-view_entity.
        ro_cds_data_definition  = io_specification->add_view_entity( ).

      WHEN OTHERS.
        ASSERT 1 = 0.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
