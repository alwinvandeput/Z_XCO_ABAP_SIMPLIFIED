CLASS z_xco_cds_data_definition_fact DEFINITION
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
      RETURNING VALUE(factory) TYPE REF TO z_xco_cds_data_definition_fact.

    "TODO specifiy type
    METHODS get_instance
      IMPORTING data_definition_name            TYPE sxco_cds_object_name
      RETURNING VALUE(data_definition) TYPE REF TO z_xco_cds_data_definition
      RAISING
        zcx_xco_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS z_xco_cds_data_definition_fact IMPLEMENTATION.

  METHOD get_factory.

    factory = NEW #( ).

  ENDMETHOD.

  METHOD get_instance.

    DATA(lo_data_definition) = xco_cp_cds=>data_definition( data_definition_name ).

    DATA(lv_type) = lo_data_definition->get_type( )->value.

    CASE lv_type.

      WHEN cs_type-view_entity.

        data_definition = z_xco_cds_view_entity=>get_instance( data_definition_name ).

      WHEN cs_type-projection_view.

        data_definition = z_xco_cds_projection_view=>get_instance( data_definition_name ).


      WHEN OTHERS.

        " CDS DDLS Object type &1 is unknown.
        RAISE EXCEPTION TYPE zcx_xco_error
              MESSAGE e003
              WITH lv_type.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
