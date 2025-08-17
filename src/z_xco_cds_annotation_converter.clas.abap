CLASS z_xco_cds_annotation_converter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.

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


    CONSTANTS:
      BEGIN OF cs_value_item_type,
        begin_array   TYPE tv_value_item_type VALUE 'BEGIN_ARRAY',
        end_array     TYPE tv_value_item_type VALUE 'END_ARRAY',
        begin_record  TYPE tv_value_item_type VALUE 'BEGIN_RECORD',
        end_record    TYPE tv_value_item_type VALUE 'END_RECORD',
        member        TYPE tv_value_item_type VALUE 'MEMBER',
        enum_value    TYPE tv_value_item_type VALUE 'ENUM_VALUE',
        boolean_value TYPE tv_value_item_type VALUE 'BOOLEAN_VALUE',
        number_value  TYPE tv_value_item_type VALUE 'NUMBER_VALUE',
        string_value  TYPE tv_value_item_type VALUE 'STRING_VALUE',
      END OF cs_value_item_type.

    METHODS convert_annotations
      IMPORTING it_annotations       TYPE tt_annotations
                io_annotation_target TYPE REF TO if_xco_gen_cds_s_fo_ann_target.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z_XCO_CDS_ANNOTATION_CONVERTER IMPLEMENTATION.


  METHOD convert_annotations.

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

          WHEN cs_value_item_type-member.

            lo_build->add_member( <lv_value_item>-value ).

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
ENDCLASS.
