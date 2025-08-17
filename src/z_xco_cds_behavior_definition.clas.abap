CLASS z_xco_cds_behavior_definition DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_behavior_definition_name TYPE sxco_cds_object_name.

    TYPES:
      BEGIN OF ts_field,
        name              TYPE sxco_cds_field_name,
        numbering_managed TYPE abap_bool,
        read_only         TYPE abap_bool,
      END OF ts_field,
      tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

    TYPES:
      tt_standard_operations TYPE STANDARD TABLE OF REF TO cl_xco_bdef_standard_operation WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_action,
        name                  TYPE sxco_bdef_action_name,
        draft                 TYPE abap_bool,
        determine             TYPE abap_bool,
        optimized             TYPE abap_bool,
        features_instance     TYPE abap_bool,

        parameter_entity_name TYPE sxco_cds_object_name,

        "Values: xco_cp_cds=>cardinality->...
        result_cardinality    TYPE REF TO if_xco_gen_bdef_rslt_cardnalty,
        result_self           TYPE abap_bool,
      END OF ts_action,
      tt_actions TYPE STANDARD TABLE OF ts_action WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_determination,
        name               TYPE sxco_bdef_evaluation_name,
        "Values: xco_cp_behavior_definition=>evaluation->time->...
        time               TYPE REF TO cl_xco_bdef_evaluation_time,
        "Values: xco_cp_behavior_definition=>evaluation->trigger_operation->...
        trigger_operations TYPE sxco_t_bdef_trigger_operations,
      END OF ts_determination,
      tt_determinations TYPE STANDARD TABLE OF ts_determination WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_association,
        alias_name     TYPE sxco_cds_association_name,
        create_enabled TYPE abap_bool,
        draft_enabled  TYPE abap_bool,
      END OF ts_association,
      tt_associations TYPE STANDARD TABLE OF ts_association WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_mapping,
        database_table_name TYPE sxco_dbt_object_name,
        corresponding       TYPE abap_bool,
        extensible          TYPE abap_bool,
        field_mappings      TYPE if_xco_gen_bdef_s_fo_b_mapping=>tt_field_mapping,
      END OF ts_mapping.

    TYPES:
      BEGIN OF ts_behavior,
        cds_entity_name              TYPE sxco_cds_object_name,
        alias_name                   TYPE c LENGTH 30,
        implementation_class_name    TYPE sxco_ao_object_name,
        extensible                   TYPE abap_bool,

        persistent_db_table_name     TYPE sxco_dbt_object_name,
        draft_table_name             TYPE sxco_dbt_object_name,

        lock_master                  TYPE abap_bool,
        lock_master_total_etag_field TYPE sxco_cds_field_name,
        lock_by_parent_alias_name    TYPE sxco_cds_association_name,

        etag_master_field_name       TYPE sxco_cds_field_name,
        etag_by_parent_alias_name    TYPE sxco_cds_association_name,

        authorization_master_global  TYPE abap_bool,

        fields                       TYPE tt_fields,
        standard_operations          TYPE tt_standard_operations,
        actions                      TYPE tt_actions,
        determinations               TYPE tt_determinations,

        associations                 TYPE tt_associations,

        mapping                      TYPE ts_mapping,


      END OF ts_behavior,
      tt_behaviors TYPE STANDARD TABLE OF ts_behavior WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        name                      TYPE tv_behavior_definition_name,
        description               TYPE if_xco_cp_gen_srvd_s_form=>tv_short_description,
        implementation_type       TYPE REF TO  cl_xco_bdef_implementation_typ,
        implementation_class_name TYPE sxco_ao_object_name,
        draft_enabled             TYPE abap_bool,
        extensible                TYPE abap_bool,
        "Strict mode values: zdmo_cl_rap_node=>strict_mode_2
        strict_mode               TYPE i,
        behaviors                 TYPE tt_behaviors,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request   TYPE sxco_transport,
        package             TYPE sxco_package,
        behavior_definition TYPE ts_data,
      END OF ts_create.

    CLASS-METHODS get_instance
      IMPORTING iv_behavior_definition_name   TYPE tv_behavior_definition_name
      RETURNING VALUE(ro_behavior_definition) TYPE REF TO z_xco_cds_behavior_definition.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create                     TYPE ts_create
      RETURNING VALUE(ro_behavior_definition) TYPE REF TO z_xco_cds_behavior_definition.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_behavior_definition_name TYPE tv_behavior_definition_name.

    CLASS-METHODS _set_behavior_header
      IMPORTING
        io_specification          TYPE REF TO if_xco_cp_gen_bdef_s_form
        is_behavior               TYPE ts_behavior
      RETURNING
        VALUE(ro_header_behavior) TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_fields
      IMPORTING
        is_behavior        TYPE ts_behavior
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_standard_operations
      IMPORTING
        is_behavior        TYPE ts_behavior
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_actions
      IMPORTING
        is_behavior        TYPE ts_behavior
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_beh_definition_header
      IMPORTING
        is_create        TYPE ts_create
        io_specification TYPE REF TO if_xco_cp_gen_bdef_s_form.

    CLASS-METHODS _set_determinations
      IMPORTING
        cs_behavior        TYPE ts_behavior
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_associations
      IMPORTING
        is_behavior        TYPE ts_behavior
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

    CLASS-METHODS _set_mapping
      IMPORTING
        is_mapping         TYPE ts_mapping
        io_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.

ENDCLASS.



CLASS Z_XCO_CDS_BEHAVIOR_DEFINITION IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-bdef->add_object( is_create-behavior_definition-name ).

    lo_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Behavior Definition Header
    _set_beh_definition_header(
      is_create = is_create
      io_specification = lo_specification ).

    LOOP AT is_create-behavior_definition-behaviors
      ASSIGNING FIELD-SYMBOL(<ls_behavior>).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Behavior Header
      DATA lo_header_behavior TYPE REF TO if_xco_gen_bdef_s_fo_behavior.
      lo_header_behavior = _set_behavior_header(
        io_specification = lo_specification
        is_behavior = <ls_behavior> ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Fields
      _set_fields(
        is_behavior        = <ls_behavior>
        io_header_behavior = lo_header_behavior ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Standard Operations
      _set_standard_operations(
        is_behavior        = <ls_behavior>
        io_header_behavior = lo_header_behavior ).


      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Actions
      _set_actions(
        is_behavior = <ls_behavior>
        io_header_behavior = lo_header_behavior ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Determinations
      _set_determinations(
        cs_behavior = <ls_behavior>
        io_header_behavior = lo_header_behavior ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Associations
      _set_associations(
        EXPORTING
          is_behavior        = <ls_behavior>
          io_header_behavior = lo_header_behavior ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Mapping
      _set_mapping(
        is_mapping         = <ls_behavior>-mapping
        io_header_behavior = lo_header_behavior ).

    ENDLOOP.

    DATA(ls_result) = lo_operation->execute( ).

    ro_behavior_definition = NEW #( ).
    ro_behavior_definition->gv_behavior_definition_name = is_create-behavior_definition-name.

  ENDMETHOD.


  METHOD get_data.

*    DATA(lo_behavior_definition) = xco_cp_abap_repository=>object->srvb->for( gv_behavior_definition_name ).
*
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Header
*    rs_data-name        = gv_behavior_definition_name.
*
*    DATA(ls_content) = lo_behavior_definition->content( )->get( ).
*    rs_data-description = ls_content-short_description.
*    rs_data-binding_type = ls_content-binding_type.
*
*    DATA(lt_services) = lo_behavior_definition->services->all->get( ).
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


  METHOD get_instance.

    ro_behavior_definition = NEW #( ).
    ro_behavior_definition->gv_behavior_definition_name = iv_behavior_definition_name.

  ENDMETHOD.


  METHOD _set_actions.


    LOOP AT is_behavior-actions
      ASSIGNING FIELD-SYMBOL(<ls_action>).

      DATA(lo_action) = io_header_behavior->add_action( <ls_action>-name ).

      lo_action->set_draft( <ls_action>-draft ).

      lo_action->set_determine( <ls_action>-determine ).

      lo_action->set_optimized( <ls_action>-optimized ).

      lo_action->set_features_instance( <ls_action>-features_instance ).

      IF <ls_action>-parameter_entity_name  IS NOT INITIAL.
        lo_action->parameter->set_entity( <ls_action>-parameter_entity_name ).
      ENDIF.

      IF <ls_action>-result_cardinality IS NOT INITIAL.
        lo_action->result->set_cardinality( <ls_action>-result_cardinality ).
        lo_action->result->set_self( <ls_action>-result_self ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_associations.

    LOOP AT is_behavior-associations
      ASSIGNING FIELD-SYMBOL(<ls_association>).

      DATA(lo_association) = io_header_behavior->add_association( <ls_association>-alias_name ).
      lo_association->set_create_enabled( <ls_association>-create_enabled ).
      lo_association->set_draft_enabled( <ls_association>-draft_enabled ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_behavior_header.

    ro_header_behavior  = io_specification->add_behavior( is_behavior-cds_entity_name ).

    " Characteristics.
    DATA(characteristics) = ro_header_behavior->characteristics.

    IF is_behavior-alias_name IS NOT INITIAL.
      characteristics->set_alias( is_behavior-alias_name ).
    ENDIF.

    IF is_behavior-implementation_class_name IS NOT INITIAL.
      characteristics->set_implementation_class( is_behavior-implementation_class_name ).
    ENDIF.

    IF is_behavior-extensible = abap_true.
      characteristics->set_extensible( is_behavior-extensible ).
    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Database Tables
    ro_header_behavior->characteristics->set_persistent_table( is_behavior-persistent_db_table_name ).

    DATA(lo_draft_table) = characteristics->set_draft_table( is_behavior-draft_table_name ).
*      lo_draft_table->set_query( iv_query = ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lock
    characteristics->lock->set_master( is_behavior-lock_master ).
    characteristics->lock->set_master_total_etag( is_behavior-lock_master_total_etag_field  ).
    characteristics->lock->set_dependent_by( is_behavior-lock_by_parent_alias_name ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-tag
    characteristics->etag->set_master( is_behavior-etag_master_field_name ).
    characteristics->etag->set_dependent_by( is_behavior-etag_by_parent_alias_name ).

    characteristics->authorization->set_master_global( is_behavior-authorization_master_global ).

  ENDMETHOD.


  METHOD _set_beh_definition_header.

    io_specification->set_short_description( is_create-behavior_definition-description ).

    io_specification->set_extensible( is_create-behavior_definition-extensible ).

    io_specification->set_implementation_type( is_create-behavior_definition-implementation_type ).

    io_specification->set_implementation_class( is_create-behavior_definition-implementation_class_name ).

    io_specification->set_draft_enabled( is_create-behavior_definition-draft_enabled ).

    io_specification->set_strict_n( is_create-behavior_definition-strict_mode ).

  ENDMETHOD.


  METHOD _set_determinations.

    LOOP AT cs_behavior-determinations
      ASSIGNING FIELD-SYMBOL(<ls_determination>).

      DATA(lo_determination) = io_header_behavior->add_determination( <ls_determination>-name ).

      IF <ls_determination>-time IS NOT INITIAL.
        lo_determination->set_time( <ls_determination>-time ).
      ENDIF.

      IF <ls_determination>-trigger_operations IS NOT INITIAL.
        lo_determination->set_trigger_operations( <ls_determination>-trigger_operations ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_fields.

    LOOP AT is_behavior-fields
      ASSIGNING FIELD-SYMBOL(<ls_field>).

      DATA(lo_field) = io_header_behavior->add_field( <ls_field>-name ).

      lo_field->set_numbering_managed( <ls_field>-numbering_managed ).

      lo_field->set_read_only( <ls_field>-read_only ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_mapping.

    IF is_mapping-database_table_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_mapping) = io_header_behavior->add_mapping_for( is_mapping-database_table_name ).
    lo_mapping->set_corresponding( is_mapping-corresponding ).
    lo_mapping->set_extensible( is_mapping-extensible ).
    lo_mapping->set_field_mapping( is_mapping-field_mappings ).

  ENDMETHOD.


  METHOD _set_standard_operations.

    LOOP AT is_behavior-standard_operations
      ASSIGNING FIELD-SYMBOL(<lo_standard_operation>).
      io_header_behavior->add_standard_operation( <lo_standard_operation> ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
