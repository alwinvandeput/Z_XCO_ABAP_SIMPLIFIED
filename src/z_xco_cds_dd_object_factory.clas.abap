CLASS z_xco_cds_dd_object_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_type TYPE cl_xco_ddef_type=>tv_value.



    CONSTANTS:
      BEGIN OF cs_type,
        view_entity     TYPE tv_type VALUE 'W',
        projection_view TYPE tv_type VALUE 'P',
      END OF cs_type.

    CLASS-METHODS get_factory
      RETURNING VALUE(ro_factory) TYPE REF TO z_xco_cds_dd_object_factory.

    METHODS get_cds_object_by_name
      IMPORTING iv_name              TYPE sxco_cds_object_name
      RETURNING VALUE(ro_cds_object) TYPE REF TO z_xco_cds_dd_abstract_object
      RAISING
                zcx_xco_error.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_factory TYPE REF TO z_xco_cds_dd_object_factory.

ENDCLASS.



CLASS z_xco_cds_dd_object_factory IMPLEMENTATION.

  METHOD get_factory.

    IF go_factory IS INITIAL.
      go_factory = NEW #( ).
    ENDIF.

    ro_factory = go_factory.

  ENDMETHOD.


  METHOD get_cds_object_by_name.

    DATA(lo_data_definition) = xco_cp_cds=>data_definition( iv_name ).

    DATA(lv_type) = lo_data_definition->get_type( )->value.

    CASE lv_type.

      WHEN cs_type-view_entity.

        ro_cds_object = z_xco_cds_view_entity=>get_instance( iv_name ).

      WHEN cs_type-projection_view.

        ro_cds_object = z_xco_cds_projection_view=>get_instance( iv_name ).


      WHEN OTHERS.

        " CDS DDLS Object type &1 is unknown.
        RAISE EXCEPTION TYPE zcx_xco_error
              MESSAGE e003
              WITH lv_type.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
