CLASS z_xco_cds_metadata_extension DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_metadata_extension_name TYPE sxco_cds_object_name.
    TYPES tv_value_item_type TYPE c LENGTH 15.

    TYPES:
      BEGIN OF ts_value_item,
        type  TYPE string,
        value TYPE string,
      END OF ts_value_item,
      tt_value_items TYPE STANDARD TABLE OF ts_value_item WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_annotation,
        name        TYPE sxco_cds_ann_property,
        value_items TYPE tt_value_items,
      END OF ts_annotation.

    TYPES:
      BEGIN OF ts_field,
        name        TYPE sxco_cds_field_name,
        annotations TYPE z_xco_cds_annotation_converter=>tt_annotations,
      END OF ts_field,
      tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name              TYPE tv_metadata_extension_name,
        short_description TYPE if_xco_cp_gen_srvd_s_form=>tv_short_description,
        "Values: xco_cp_metadata_extension=>layer->...
        layer             TYPE REF TO cl_xco_me_layer,
        view              TYPE sxco_cds_object_name,
        fields            TYPE tt_fields,
      END OF ts_data.
    TYPES:
      BEGIN OF ts_create,
        transport_request  TYPE sxco_transport,
        package            TYPE sxco_package,
        metadata_extension TYPE ts_data,
      END OF ts_create.

    CLASS-METHODS get_instance
      IMPORTING iv_metadata_extension_name   TYPE tv_metadata_extension_name
      RETURNING VALUE(ro_metadata_extension) TYPE REF TO z_xco_cds_metadata_extension.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                    TYPE ts_create
      RETURNING VALUE(ro_metadata_extension) TYPE REF TO z_xco_cds_metadata_extension.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_metadata_extension_name TYPE tv_metadata_extension_name.


ENDCLASS.


CLASS z_xco_cds_metadata_extension IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-ddlx->add_object( is_create-metadata_extension-name ).

    lo_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-metadata_extension-short_description ).

    lo_specification->set_layer( is_create-metadata_extension-layer ).

    lo_specification->set_view( is_create-metadata_extension-view ).

    LOOP AT is_create-metadata_extension-fields
      ASSIGNING FIELD-SYMBOL(<ls_field>).

      DATA(lo_field) = lo_specification->add_field( <ls_field>-name ).

      NEW z_xco_cds_annotation_converter( )->convert_annotations(
        it_annotations       = <ls_field>-annotations
        io_annotation_target = lo_field ).

    ENDLOOP.

    lo_operation->execute( ).

    ro_metadata_extension = NEW #( ).
    ro_metadata_extension->gv_metadata_extension_name = is_create-metadata_extension-name.

  ENDMETHOD.


  METHOD get_instance.

    ro_metadata_extension = NEW #( ).
    ro_metadata_extension->gv_metadata_extension_name = iv_metadata_extension_name.

  ENDMETHOD.


  METHOD get_data.

*    DATA(lo_metadata_extension) = xco_cp_abap_repository=>object->srvb->for( gv_metadata_extension_name ).
*
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Header
*    rs_data-name        = gv_metadata_extension_name.
*
*    DATA(ls_content) = lo_metadata_extension->content( )->get( ).
*    rs_data-description = ls_content-short_description.
*    rs_data-binding_type = ls_content-binding_type.
*
*    DATA(lt_services) = lo_metadata_extension->services->all->get( ).
*
*    LOOP AT lt_services
*      ASSIGNING FIELD-SYMBOL(<ls_service>).
*
*      DATA(lo_versions) = <ls_service>->versions.
*      DATA(lt_versions) = lo_versions->all->get( ).
*
*      LOOP AT lt_versions
*        ASSIGNING FIELD-SYMBOL(<lv_version>).
*
*        DATA(ls_version_content) = <lv_version>->content( )->get( ).
*
*        APPEND
*          VALUE #(
*            service_definition_name = ls_version_content-service_definition->name
*            version = <lv_version>->version )
*          TO rs_data-services.
*
*      ENDLOOP.
*
*    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
