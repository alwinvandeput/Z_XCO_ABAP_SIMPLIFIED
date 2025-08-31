CLASS z_xco_cds_data_definition DEFINITION
  INHERITING FROM z_xco_repo_abstract_object
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_cds_object_name TYPE sxco_cds_object_name.

  PROTECTED SECTION.

    METHODS _get_repository_object_type
      IMPORTING iv_object_name        TYPE sxco_cds_object_name
      RETURNING VALUE(rv_object_type) TYPE sxco_ar_object_type.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z_XCO_CDS_DATA_DEFINITION IMPLEMENTATION.


  METHOD _get_repository_object_type.

    DATA(lo_name_filter) = xco_cp_abap_repository=>object_name->get_filter(
      xco_cp_abap_sql=>constraint->equal( to_upper( iv_object_name ) ) ).

    DATA(lt_objects) = xco_cp_abap_repository=>objects->where( VALUE #(
*      ( lo_software_component_filter )
      ( lo_name_filter ) )
    )->in( xco_cp_abap=>repository )->get( ).

    DATA lo_object TYPE REF TO if_xco_ar_object.

    TYPES:
      BEGIN OF ts_output_object,
        object_type TYPE sxco_ar_object_type,
        object_name TYPE sxco_ar_object_name,
        namespace   TYPE if_xco_ar_object=>tv_namespace,
        package     TYPE sxco_package,
      END OF ts_output_object.
    DATA lt_output_objects TYPE STANDARD TABLE OF ts_output_object WITH EMPTY KEY.

    LOOP AT lt_objects INTO lo_object.
      APPEND
        VALUE #(
          object_type = lo_object->type->value
          object_name = lo_object->name->value
          namespace   = lo_object->get_namespace( )
          package     = lo_object->get_package( )->name )
        TO lt_output_objects.
    ENDLOOP.

    SORT lt_output_objects BY object_type
                              object_name.
    LOOP AT lt_output_objects ASSIGNING FIELD-SYMBOL(<ls_output_object>).
      IF <ls_output_object>-object_type = 'DDLS'.
        rv_object_type = 'DDLS'.
        RETURN.
      ELSEIF <ls_output_object>-object_type = 'TABL'.
        rv_object_type = 'TABL'.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
