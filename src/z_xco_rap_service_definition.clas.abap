CLASS z_xco_rap_service_definition DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_service_definition_name TYPE sxco_srvd_object_name.

    TYPES:
      BEGIN OF ts_exposure,
        name       TYPE sxco_cds_object_name,
        alias_name TYPE if_xco_gen_srvd_s_fo_exposure=>tv_alias,
      END OF ts_exposure,
      tt_exposures TYPE STANDARD TABLE OF ts_exposure WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name        TYPE tv_service_definition_name,
        description TYPE if_xco_cp_gen_srvd_s_form=>tv_short_description,
        exposures   TYPE tt_exposures,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request  TYPE sxco_transport,
        package            TYPE sxco_package,
        service_definition TYPE ts_data,
      END OF ts_create.

    CLASS-METHODS get_instance
      IMPORTING iv_service_definition_name   TYPE tv_service_definition_name
      RETURNING VALUE(ro_service_definition) TYPE REF TO z_xco_rap_service_definition.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                    TYPE ts_create
      RETURNING VALUE(ro_service_definition) TYPE REF TO z_xco_rap_service_definition.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS set_annotations
      IMPORTING
        io_specification TYPE REF TO if_xco_cp_gen_srvd_s_form.

    DATA gv_service_definition_name TYPE tv_service_definition_name.

ENDCLASS.



CLASS Z_XCO_RAP_SERVICE_DEFINITION IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_serv_def_put_object) = lo_operation->for-srvd->add_object( is_create-service_definition-name ).

    lo_serv_def_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_serv_def_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-service_definition-description ).

    set_annotations(
      io_specification = lo_specification ).

    LOOP AT is_create-service_definition-exposures
      ASSIGNING FIELD-SYMBOL(<ls_exposure>).

      DATA(lo_exposure) = lo_specification->add_exposure( <ls_exposure>-name ).
      lo_exposure->set_alias( <ls_exposure>-alias_name ).

    ENDLOOP.

    DATA(ls_result) = lo_operation->execute( ).

    ro_service_definition = NEW #( ).
    ro_service_definition->gv_service_definition_name = is_create-service_definition-name.

  ENDMETHOD.


  METHOD get_data.

    DATA(lo_service_definition) = xco_cp_abap_repository=>object->srvd->for( gv_service_definition_name ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Header
    rs_data-name        = gv_service_definition_name.
    rs_data-description = lo_service_definition->content( )->get_short_description( ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exposures

    DATA(lt_exposures) = lo_service_definition->exposures->all->get( ).

    LOOP AT lt_exposures
         ASSIGNING FIELD-SYMBOL(<ls_exposure>).

      DATA(ls_content) = <ls_exposure>->content( )->get( ).

      APPEND
        VALUE #(
          name       = ls_content-cds_entity->name
          alias_name = ls_content-alias )
        TO rs_data-exposures.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.

    ro_service_definition = NEW #( ).
    ro_service_definition->gv_service_definition_name = iv_service_definition_name.

  ENDMETHOD.


  METHOD set_annotations.

*    "valid as of 2023
*    IF io_rap_bo_node->is_extensible(  ) = abap_true.
*      "add @AbapCatalog.extensibility.extensible: true
*      io_specification->add_annotation( 'AbapCatalog.extensibility.extensible' )->value->build( )->add_boolean( abap_true ).
*    ENDIF.

*    "check if custom entities will be generated.
*    "if yes the leading entity is an r-view rather than a p-view
*    IF io_rap_bo_node->data_source_type = zdmo_cl_rap_node=>data_source_types-abstract_entity OR
*       io_rap_bo_node->data_source_type = zdmo_cl_rap_node=>data_source_types-abap_type .
*      io_specification->add_annotation( 'ObjectModel.leadingEntity.name' )->value->build( )->add_string( CONV #( io_rap_bo_node->rap_node_objects-cds_view_r ) ).
*    ELSE.
*      io_specification->add_annotation( 'ObjectModel.leadingEntity.name' )->value->build( )->add_string( CONV #( io_rap_bo_node->rap_node_objects-cds_view_p ) ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
