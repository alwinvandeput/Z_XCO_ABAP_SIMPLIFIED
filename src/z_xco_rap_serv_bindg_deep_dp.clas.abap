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
      END OF ts_data.

    METHODS deep_read_service_binding
      IMPORTING iv_service_binding_name      TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name              TYPE sxco_srvb_service_name OPTIONAL
                iv_version                   TYPE sxco_srvb_service_version OPTIONAL
                iv_sd_root_entity_alias_name TYPE z_xco_rap_service_definition=>ts_exposure-alias_name
      RETURNING VALUE(rs_data)               TYPE ts_data
      RAISING
                zcx_xco_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS _set_service_binding_version
      IMPORTING
                iv_service_binding_name          TYPE z_xco_rap_service_binding=>tv_name
                iv_service_name                  TYPE sxco_srvb_service_name
                iv_version                       TYPE sxco_srvb_service_version
      RETURNING VALUE(rs_result_service_binding) TYPE ts_data-service_binding
      RAISING
                zcx_xco_error.
    METHODS _set_service_definition
      IMPORTING
                iv_service_definition_name          TYPE sxco_srvd_object_name
      RETURNING VALUE(rs_result_service_definition) TYPE ts_data-service_definition.
    METHODS _deep_read_cds_view
      IMPORTING
        iv_exposure_name TYPE sxco_cds_object_name.

ENDCLASS.



CLASS Z_XCO_RAP_SERV_BINDG_DEEP_DP IMPLEMENTATION.


  METHOD deep_read_service_binding.

    rs_data-service_binding = _set_service_binding_version(
      iv_service_binding_name = iv_service_binding_name
      iv_service_name         = iv_service_name
      iv_version              = iv_version ).

    rs_data-service_definition = _set_service_definition(
      iv_service_definition_name = rs_data-service_binding-service_definition_name ).


    DATA(lv_found) = abap_false.
    LOOP AT rs_data-service_definition-exposures
      ASSIGNING FIELD-SYMBOL(<ls_exposure>)
      WHERE alias_name = iv_sd_root_entity_alias_name.


      _deep_read_cds_view( <ls_exposure>-name ).

      lv_found = abap_true.


    ENDLOOP.

    IF lv_found = abap_false.
      "Root Entity Alias Name &2 not found in Service Definition &1.
      RAISE EXCEPTION TYPE zcx_xco_error
            MESSAGE e002
            WITH
              rs_data-service_definition-name
              iv_sd_root_entity_alias_name.
    ENDIF.

  ENDMETHOD.


  METHOD _deep_read_cds_view.

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
ENDCLASS.
