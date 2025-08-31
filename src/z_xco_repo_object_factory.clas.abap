CLASS z_xco_repo_object_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING iv_buffering_ind TYPE abap_boolean
      RETURNING VALUE(factory)   TYPE REF TO z_xco_repo_object_factory.

    METHODS get_repository_object
      "TODO specify data type
      IMPORTING iv_object_type     TYPE string
                iv_object_name     TYPE string
      RETURNING VALUE(repo_object) TYPE REF TO z_xco_repository_object
      RAISING
        zcx_xco_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA buffered_factory TYPE REF TO z_xco_repo_object_factory.
    CLASS-DATA not_buffered_factory TYPE REF TO z_xco_repo_object_factory.

    DATA buffering_ind TYPE abap_boolean.

ENDCLASS.



CLASS z_xco_repo_object_factory IMPLEMENTATION.

  METHOD get_instance.

    IF iv_buffering_ind = abap_true.

      IF buffered_factory IS INITIAL.
        buffered_factory = NEW #( ).
      ENDIF.

      factory = buffered_factory.
      factory->buffering_ind = abap_true.

    ELSE.

      IF not_buffered_factory IS INITIAL.
        not_buffered_factory = NEW #( ).
      ENDIF.

      factory = not_buffered_factory.

    ENDIF.

  ENDMETHOD.

  METHOD get_repository_object.

    CASE iv_object_type.

      WHEN 'DDLS'.

        DATA(data_definition_factory) = z_xco_CDS_data_definition_fact=>get_factory( ).

        repo_object = data_definition_factory->get_instance( CONV #( iv_object_name ) ).

      WHEN OTHERS.

        ASSERT 1 = 0.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
