CLASS z_xco_rap_service_binding DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_service_binding_name TYPE sxco_srvb_object_name.

    TYPES:
      BEGIN OF ts_version,
        version                 TYPE sxco_srvb_service_version,
        service_definition_name TYPE sxco_srvd_object_name,
      END OF ts_version,
      tt_versions TYPE STANDARD TABLE OF ts_version WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_service,
        name     TYPE sxco_srvb_service_name,
        versions TYPE tt_versions,
      END OF ts_service,
      tt_services TYPE STANDARD TABLE OF ts_service WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name         TYPE tv_service_binding_name,
        description  TYPE if_xco_cp_gen_srvd_s_form=>tv_short_description,
        binding_type TYPE REF TO cl_xco_srvb_binding_type,
        services     TYPE tt_services,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        service_binding   TYPE ts_data,
      END OF ts_create.

    CLASS-METHODS get_instance
      IMPORTING iv_service_binding_name   TYPE tv_service_binding_name
      RETURNING VALUE(ro_service_binding) TYPE REF TO z_xco_rap_service_binding.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                 TYPE ts_create
      RETURNING VALUE(ro_service_binding) TYPE REF TO z_xco_rap_service_binding.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS set_annotations
      IMPORTING
        io_specification TYPE REF TO if_xco_cp_gen_srvd_s_form.

    DATA gv_service_binding_name TYPE tv_service_binding_name.

ENDCLASS.



CLASS z_xco_rap_service_binding IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-srvb->add_object( is_create-service_binding-name ).

    lo_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-service_binding-description ).

    lo_specification->set_binding_type( xco_cp_service_binding=>binding_type->odata_v4_ui ).

    LOOP AT is_create-service_binding-services
      ASSIGNING FIELD-SYMBOL(<ls_service>).

      DATA(lo_service) = lo_specification->add_service( <ls_service>-name ).

      LOOP AT <ls_service>-versions
        ASSIGNING FIELD-SYMBOL(<ls_version>).

        DATA(lo_version) = lo_service->add_version( <ls_version>-version ).
        lo_version->set_service_definition( <ls_version>-service_definition_name ).

      ENDLOOP.

    ENDLOOP.

    DATA(ls_result) = lo_operation->execute( ).

    ro_service_binding = NEW #( ).
    ro_service_binding->gv_service_binding_name = is_create-service_binding-name.

  ENDMETHOD.


  METHOD get_instance.

    ro_service_binding = NEW #( ).
    ro_service_binding->gv_service_binding_name = iv_service_binding_name.

  ENDMETHOD.


  METHOD get_data.

    DATA(lo_service_binding) = xco_cp_abap_repository=>object->srvb->for( gv_service_binding_name ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Header
    rs_data-name        = gv_service_binding_name.

    DATA(ls_content) = lo_service_binding->content( )->get( ).
    rs_data-description = ls_content-short_description.
    rs_data-binding_type = ls_content-binding_type.

    DATA(lt_services) = lo_service_binding->services->all->get( ).

    LOOP AT lt_services
      ASSIGNING FIELD-SYMBOL(<ls_service>).

      APPEND
        VALUE #(
          name = <ls_service>->name )
        TO rs_data-services
        ASSIGNING FIELD-SYMBOL(<ls_output_service>).

      DATA(lo_versions) = <ls_service>->versions.
      DATA(lt_versions) = lo_versions->all->get( ).


      LOOP AT lt_versions
        ASSIGNING FIELD-SYMBOL(<lv_version>).

        DATA(ls_version_content) = <lv_version>->content( )->get( ).

        APPEND
          VALUE #(
            service_definition_name = ls_version_content-service_definition->name
            version = <lv_version>->version )
          TO <ls_output_service>-versions.

      ENDLOOP.

    ENDLOOP.

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
