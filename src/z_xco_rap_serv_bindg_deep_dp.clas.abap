CLASS z_xco_rap_serv_bindg_deep_dp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_service_binding,
        service_binding_name        TYPE z_xco_rap_service_binding=>tv_name,
        service_binding_description TYPE z_xco_rap_service_binding=>ts_data-description,
        service_binding_type        TYPE z_xco_rap_service_binding=>ts_data-binding_type,
        service_name                TYPE z_xco_rap_service_binding=>ts_service-name,
        version                     TYPE z_xco_rap_service_binding=>ts_version-version,
        service_definition_name     TYPE z_xco_rap_service_binding=>ts_version-service_definition_name,
      END OF ts_service_binding.

    TYPES:
      BEGIN OF ts_data,
        service_binding    TYPE ts_service_binding,
        service_definition TYPE z_xco_rap_service_definition=>ts_data,
        cds_view_deep_data TYPE z_xco_cds_view_deep_read_dp=>ts_cds_view_deep_data,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_data2,
        service_binding    TYPE ts_service_binding,
        service_definition TYPE z_xco_rap_service_definition=>ts_data,
        cds_views          TYPE STANDARD TABLE OF z_xco_cds_view_deep_read_dp=>ts_deep_read_cds_view WITH EMPTY KEY,
      END OF ts_data2.

    METHODS deep_read_service_binding
      IMPORTING iv_service_binding_name      TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name              TYPE sxco_srvb_service_name OPTIONAL
                iv_version                   TYPE sxco_srvb_service_version OPTIONAL
                iv_sd_root_entity_alias_name TYPE z_xco_rap_service_definition=>ts_exposure-alias_name
      RETURNING VALUE(rs_data)               TYPE ts_data
      RAISING
                zcx_xco_error.

    METHODS deep_read_service_binding_2
      IMPORTING iv_service_binding_name      TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name              TYPE sxco_srvb_service_name OPTIONAL
                iv_version                   TYPE sxco_srvb_service_version OPTIONAL
                iv_sd_root_entity_alias_name TYPE z_xco_rap_service_definition=>ts_exposure-alias_name
      RETURNING VALUE(rs_data_2)             TYPE ts_data2
      RAISING
                zcx_xco_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS _set_service_binding_version
      IMPORTING iv_service_binding_name          TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name                  TYPE sxco_srvb_service_name
                iv_version                       TYPE sxco_srvb_service_version
      RETURNING VALUE(rs_result_service_binding) TYPE ts_data-service_binding
      RAISING   zcx_xco_error.

    METHODS _set_service_definition
      IMPORTING iv_service_definition_name          TYPE sxco_srvd_object_name
      RETURNING VALUE(rs_result_service_definition) TYPE ts_data-service_definition.

    METHODS _down_deep_read_main_entity
      IMPORTING iv_service_binding_name      TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name              TYPE sxco_srvb_service_name
                iv_version                   TYPE sxco_srvb_service_version
                iv_sd_root_entity_alias_name TYPE z_xco_rap_service_definition=>ts_exposure-alias_name
      RETURNING VALUE(rs_data_2)             TYPE ts_data2
      RAISING
                zcx_xco_error.

    METHODS _get_bo_root_cds_view.
    METHODS _get_child_bo_entities.
    METHODS _down_deep_read_child_entities.
    METHODS _up_deep_read_child_entities.

ENDCLASS.



CLASS z_xco_rap_serv_bindg_deep_dp IMPLEMENTATION.


  METHOD deep_read_service_binding.

    rs_data-service_binding = _set_service_binding_version(
      iv_service_binding_name = iv_service_binding_name
      iv_service_name         = iv_service_name
      iv_version              = iv_version ).

    rs_data-service_definition = _set_service_definition(
      iv_service_definition_name = rs_data-service_binding-service_definition_name ).

    READ TABLE rs_data-service_definition-exposures
      WITH KEY alias_name = iv_sd_root_entity_alias_name
      ASSIGNING FIELD-SYMBOL(<ls_exposure>).

    IF sy-subrc <> 0.
      "Root Entity Alias Name &2 not found in Service Definition &1.
      RAISE EXCEPTION TYPE zcx_xco_error
            MESSAGE e002
            WITH
              rs_data-service_definition-name
              iv_sd_root_entity_alias_name.
    ENDIF.

    DATA(dp) = NEW z_xco_cds_view_deep_read_dp( ).
    rs_data-cds_view_deep_data = dp->read_in_layers( <ls_exposure>-name ).

  ENDMETHOD.


  METHOD _set_service_binding_version.

    " Set Service Name
    DATA lv_service_name TYPE sxco_srvb_service_name.

    IF iv_service_name IS INITIAL.
      lv_service_name = iv_service_binding_name.
    ELSE.
      lv_service_name = iv_service_name.
    ENDIF.

    " Set Version
    DATA lv_version TYPE sxco_srvb_service_version.
    IF iv_version IS INITIAL.
      lv_version = 0001.
    ELSE.
      lv_version = iv_version.
    ENDIF.

    DATA(lo_service_binding) = z_xco_rap_service_binding=>get_instance( iv_service_binding_name ).

    DATA(ls_serv_binding_data) = lo_service_binding->get_data( ).

    rs_result_service_binding-service_binding_name = ls_serv_binding_data-name.
    rs_result_service_binding-service_binding_description = ls_serv_binding_data-description.
    rs_result_service_binding-service_binding_type = ls_serv_binding_data-binding_type.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_found_ind) = abap_true.

    LOOP AT ls_serv_binding_data-services
         ASSIGNING FIELD-SYMBOL(<ls_service>)
         WHERE name = lv_service_name.

      rs_result_service_binding-service_name = <ls_service>-name.

      LOOP AT <ls_service>-versions
        " TODO: variable is assigned but never used (ABAP cleaner)
           ASSIGNING FIELD-SYMBOL(<ls_version>)
           WHERE version = lv_version.

        rs_result_service_binding-version = <ls_version>-version.
        rs_result_service_binding-service_definition_name = <ls_version>-service_definition_name.

        DATA(lv_found) = abap_true.

      ENDLOOP.

    ENDLOOP.

    IF lv_found = abap_false.
      "Service binding: &1 service: &2, version: &3 not found.
      RAISE EXCEPTION TYPE zcx_xco_error
            MESSAGE e001
            WITH
              iv_service_binding_name
              lv_service_name
              lv_version.
    ENDIF.

  ENDMETHOD.


  METHOD _set_service_definition.

    DATA(lo_service_definition) = z_xco_rap_service_definition=>get_instance( iv_service_definition_name ).

    rs_result_service_definition = lo_service_definition->get_data( ).

  ENDMETHOD.


  METHOD deep_read_service_binding_2.

    "Down Deep Read Main Entity (from Service Binding Layer to Database Layer)
    rs_data_2 = _down_deep_read_main_entity(
      iv_service_binding_name      = iv_service_binding_name
      iv_service_name              = iv_service_name
      iv_version                   = iv_version
      iv_sd_root_entity_alias_name = iv_sd_root_entity_alias_name ).

    "Get BO Root CDS View (can be CDS Classic View or CDS View Entity)
    _get_bo_root_cds_view( ).

    "Get Child BO Entities
    _get_child_bo_entities( ).

    "Down Deep Read Child Entities (from BO Layer to Database Layer)
    _down_deep_read_child_entities( ).

    "Up Deep Read Child Entities (from BO Layer to Service Binding Layer
    _up_deep_read_child_entities( ).

  ENDMETHOD.

  METHOD _down_deep_read_main_entity.

    rs_data_2-service_binding = _set_service_binding_version(
      iv_service_binding_name = iv_service_binding_name
      iv_service_name         = iv_service_name
      iv_version              = iv_version ).

    rs_data_2-service_definition = _set_service_definition(
      iv_service_definition_name = rs_data_2-service_binding-service_definition_name ).

    READ TABLE rs_data_2-service_definition-exposures
      WITH KEY alias_name = iv_sd_root_entity_alias_name
      ASSIGNING FIELD-SYMBOL(<ls_exposure>).

    IF sy-subrc <> 0.
      "Root Entity Alias Name &2 not found in Service Definition &1.
      RAISE EXCEPTION TYPE zcx_xco_error
            MESSAGE e002
            WITH
              rs_data_2-service_definition-name
              iv_sd_root_entity_alias_name.
    ENDIF.

    APPEND INITIAL LINE TO rs_data_2-cds_views
      ASSIGNING FIELD-SYMBOL(<ls_cds_view>).
    DATA(dp) = NEW z_xco_cds_view_deep_read_dp( ).
    <ls_cds_view> = dp->deep_read_cds_view( <ls_exposure>-name ).

  ENDMETHOD.


  METHOD _get_bo_root_cds_view.

  ENDMETHOD.


  METHOD _get_child_bo_entities.

  ENDMETHOD.


  METHOD _down_deep_read_child_entities.

  ENDMETHOD.


  METHOD _up_deep_read_child_entities.

  ENDMETHOD.

ENDCLASS.
