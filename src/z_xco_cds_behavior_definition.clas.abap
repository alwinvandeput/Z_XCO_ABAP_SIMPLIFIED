CLASS z_xco_cds_behavior_definition DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_behavior_definition_name TYPE sxco_cds_object_name.

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
      BEGIN OF ts_behavior,
        cds_entity_name           TYPE sxco_cds_object_name,
        alias_name                TYPE c LENGTH 30,
        implementation_class_name TYPE sxco_ao_object_name,
        extensible                TYPE abap_bool,
        persistent_db_table_name  TYPE sxco_dbt_object_name,
        draft_table_name          TYPE sxco_dbt_object_name,
        lock_master               TYPE abap_bool,
        etag_master_field_name    TYPE sxco_cds_field_name,

        standard_operations       TYPE tt_standard_operations,
        actions                   TYPE tt_actions,

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
    CLASS-METHODS set_annotations
      IMPORTING
        io_specification TYPE REF TO if_xco_cp_gen_srvd_s_form.

    DATA gv_behavior_definition_name TYPE tv_behavior_definition_name.

ENDCLASS.



CLASS z_xco_cds_behavior_definition IMPLEMENTATION.


  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-bdef->add_object( is_create-behavior_definition-name ).

    lo_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-behavior_definition-description ).

    lo_specification->set_extensible( is_create-behavior_definition-extensible ).

    lo_specification->set_implementation_type( is_create-behavior_definition-implementation_type ).

    lo_specification->set_implementation_class( is_create-behavior_definition-implementation_class_name ).

    lo_specification->set_draft_enabled( is_create-behavior_definition-draft_enabled ).

    lo_specification->set_strict_n( is_create-behavior_definition-strict_mode ).

    LOOP AT is_create-behavior_definition-behaviors
      ASSIGNING FIELD-SYMBOL(<ls_behavior>).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Behavior Header
      DATA(lo_header_behavior) = lo_specification->add_behavior( <ls_behavior>-cds_entity_name ).

      " Characteristics.
      DATA(characteristics) = lo_header_behavior->characteristics.

      IF <ls_behavior>-alias_name IS NOT INITIAL.
        characteristics->set_alias( <ls_behavior>-alias_name ).
      ENDIF.


      IF <ls_behavior>-implementation_class_name IS NOT INITIAL.
        characteristics->set_implementation_class( <ls_behavior>-implementation_class_name ).
      ENDIF.

      IF <ls_behavior>-extensible = abap_true.
        characteristics->set_extensible( <ls_behavior>-extensible ).
      ENDIF.

      lo_header_behavior->characteristics->set_persistent_table( <ls_behavior>-persistent_db_table_name ).

      DATA(lo_lock) = characteristics->lock.

      IF <ls_behavior>-lock_master = abap_true.
        lo_lock->set_master( <ls_behavior>-lock_master ).
      ENDIF.

      DATA(lo_draft_table) = characteristics->set_draft_table( <ls_behavior>-draft_table_name ).
*      lo_draft_table->set_query( iv_query = ).

      characteristics->etag->set_master( <ls_behavior>-etag_master_field_name ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Standard Operations
      LOOP AT <ls_behavior>-standard_operations
        ASSIGNING FIELD-SYMBOL(<lo_standard_operation>).
        lo_header_behavior->add_standard_operation( <lo_standard_operation> ).
      ENDLOOP.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Actions

      LOOP AT <ls_behavior>-actions
        ASSIGNING FIELD-SYMBOL(<ls_action>).

        DATA(lo_action) = lo_header_behavior->add_action( <ls_action>-name ).

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


*        lo_header_behavior->add_determination( CONV #( lv_determination_name ) "'CalculateSemanticKey'
*          )->set_time( xco_cp_behavior_definition=>evaluation->time->on_save
*          )->set_trigger_operations( VALUE #( ( xco_cp_behavior_definition=>evaluation->trigger_operation->create ) )  ).
*
*
*
*
*        LOOP AT lt_mapping_header INTO ls_mapping_header.
*          CASE ls_mapping_header-dbtable_field.
*            WHEN io_rap_bo_node->field_name-uuid.
*              lo_header_behavior->add_field( ls_mapping_header-cds_view_field
*                               )->set_numbering_managed( ).
*              "to do
*              "add a working dummy implementation to calculate the object id
*            WHEN  io_rap_bo_node->object_id .
*              lo_header_behavior->add_field( ls_mapping_header-cds_view_field
*                                 )->set_read_only( ).
*          ENDCASE.
*        ENDLOOP.
*
*      WHEN ZDMO_cl_rap_node=>implementation_type-managed_semantic .
*
*        LOOP AT io_rap_bo_node->lt_fields INTO DATA(key_field_root_node)
*               WHERE key_indicator = abap_true AND name <> io_rap_bo_node->field_name-client.
*
*          DATA(key_field_root_behavior) = lo_header_behavior->add_field( key_field_root_node-cds_view_field ).
*
*
*          method_exists_in_interface-interface_name = 'if_xco_gen_bdef_s_fo_b_field'.
*          method_exists_in_interface-method_name    = 'SET_READONLY_UPDATE'.
*
*          IF xco_api->method_exists_in_interface( interface_name = method_exists_in_interface-interface_name
*                                                  method_name    = method_exists_in_interface-method_name ).
*            CALL METHOD key_field_root_behavior->(method_exists_in_interface-method_name).
*          ENDIF.
*
*
*          "lo_item_behavior->add_field( ls_fields-cds_view_field )->set_readonly_update(  ).
*        ENDLOOP.
*
*
*      WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*
*        LOOP AT io_rap_bo_node->lt_fields INTO DATA(ls_field) WHERE name <> io_rap_bo_node->field_name-client.
*          IF ls_field-key_indicator = abap_true.
*            lo_header_behavior->add_field( ls_field-cds_view_field
*                               )->set_read_only(
*                               ).
*          ENDIF.
*        ENDLOOP.
*    ENDCASE.
*
*
**  make administrative fields read-only
**  field ( readonly )
**   CreatedAt,
**   CreatedBy,
**   LocalLastChangedAt,
**   LastChangedAt,
**   LastChangedBy;
*
*    LOOP AT io_rap_bo_node->lt_fields INTO ls_field
*      WHERE name <> io_rap_bo_node->field_name-client.
*      CASE ls_field-name .
*        WHEN io_rap_bo_node->field_name-created_at OR
*             io_rap_bo_node->field_name-created_by OR
*             io_rap_bo_node->field_name-local_instance_last_changed_at OR
*             io_rap_bo_node->field_name-local_instance_last_changed_by OR
*             io_rap_bo_node->field_name-last_changed_at OR
*             io_rap_bo_node->field_name-last_changed_by OR
*             io_rap_bo_node->field_name-uuid.
*          lo_header_behavior->add_field( ls_field-cds_view_field )->set_read_only( ).
*      ENDCASE.
*    ENDLOOP.
*
*
*
*    IF lt_mapping_header IS NOT INITIAL.
*      CASE io_rap_bo_node->get_implementation_type(  ).
*        WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*
*          "use conv #( ) since importing parameter iv_database_table
*          "was of type sxco_dbt_object_name and has been changed to clike
*          "to support structure names longer than 16 characters as of 2111
*
*          DATA(header_mapping) = lo_header_behavior->add_mapping_for( CONV #( io_rap_bo_node->persistent_table_name ) ).
*          header_mapping->set_field_mapping( it_field_mappings =  lt_mapping_header ).
*          IF io_rap_bo_node->is_extensible(  ) = abap_true.
*            "header_mapping->set_extensible(  )->set_corresponding(  ).
*            set_extensible_for_mapping( header_mapping ).
*          ENDIF.
*        WHEN ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*          header_mapping = lo_header_behavior->add_mapping_for( CONV #( io_rap_bo_node->persistent_table_name ) ).
*          header_mapping->set_field_mapping( it_field_mappings =  lt_mapping_header ).
*          IF io_rap_bo_node->is_extensible(  ) = abap_true.
*            "header_mapping->set_extensible(  )->set_corresponding(  ).
*            set_extensible_for_mapping( header_mapping ).
*          ENDIF.
*        WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*          "add control structure
**          lo_header_behavior->add_mapping_for( CONV sxco_dbt_object_name( io_rap_bo_node->persistent_table_name ) )->set_field_mapping( it_field_mappings =  lt_mapping_header )->set_control( io_rap_bo_node->rap_node_objects-control_structure ).
*
*          "add control structure
*          IF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-table.
*            header_mapping = lo_header_behavior->add_mapping_for( CONV #( io_rap_bo_node->persistent_table_name ) ).
*            header_mapping->set_field_mapping( it_field_mappings = lt_mapping_header )->set_control( io_rap_bo_node->rap_node_objects-control_structure ).
*          ELSEIF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-structure.
*            header_mapping = lo_header_behavior->add_mapping_for( CONV #( io_rap_bo_node->structure_name ) ).
*            header_mapping->set_field_mapping( it_field_mappings = lt_mapping_header )->set_control( io_rap_bo_node->rap_node_objects-control_structure ).
*          ELSEIF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-abap_type.
*            "structure name is added here since we only support abap types that are based on structures
*            header_mapping = lo_header_behavior->add_mapping_for( CONV #( io_rap_bo_node->structure_name ) ).
*            header_mapping->set_field_mapping( it_field_mappings = lt_mapping_header )->set_control( io_rap_bo_node->rap_node_objects-control_structure ).
*          ENDIF.
*          IF io_rap_bo_node->is_extensible(  ) = abap_true.
*            " header_mapping->set_extensible(  )->set_corresponding(  ).
*            set_extensible_for_mapping( header_mapping ).
*          ENDIF.
*      ENDCASE.
*    ENDIF.
*
*    IF io_rap_bo_node->has_childs(  ).
*      LOOP AT io_rap_bo_node->childnodes INTO DATA(lo_childnode).
*        DATA(assoc) = lo_header_behavior->add_association( '_' && lo_childnode->rap_node_objects-alias  ).
*        assoc->set_create_enabled(  ).
*        assoc->set_draft_enabled( io_rap_bo_node->draft_enabled ).
*      ENDLOOP.
*    ENDIF.
*
*
*
*    "define behavior for child entities
*
*    IF io_rap_bo_node->has_childs(  ).
*
*      LOOP AT io_rap_bo_node->all_childnodes INTO lo_childnode.
*
*        CLEAR lt_mapping_item.
*
*        lt_mapping_item = lo_childnode->lt_mapping.
*
*        DATA(lo_item_behavior) = lo_specification->add_behavior( lo_childnode->rap_node_objects-cds_view_r ).
*
*        " Characteristics.
*        DATA(item_characteristics) = lo_item_behavior->characteristics.
*
*        "add the draft table
*        IF io_rap_bo_node->draft_enabled = abap_true.
*          item_characteristics->set_draft_table( lo_childnode->draft_table_name ).
*        ENDIF.
*
*        IF lo_childnode->is_extensible(  ) = abap_true.
*          item_characteristics->set_extensible(  ).
*        ENDIF.
*
*        "@todo: Compare with code for root entity
*
*        CLEAR local_instance_last_changed_at.
*
*        IF line_exists( lo_childnode->lt_fields[ name = lo_childnode->field_name-local_instance_last_changed_at ] ).
*          local_instance_last_changed_at = lo_childnode->lt_fields[ name = lo_childnode->field_name-local_instance_last_changed_at ]-cds_view_field.
*        ELSEIF line_exists( lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-local_instance_last_changed_at ] ).
*          local_instance_last_changed_at = lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-local_instance_last_changed_at ]-cds_view_field.
*        ENDIF.
*
*
*        IF line_exists( lo_childnode->lt_fields[ name = lo_childnode->field_name-last_changed_at ] ).
*          last_changed_at = lo_childnode->lt_fields[ name = lo_childnode->field_name-last_changed_at ]-cds_view_field.
*        ELSEIF line_exists( lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-last_changed_at ] ).
*          last_changed_at = lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-last_changed_at ]-cds_view_field.
*        ENDIF.
*
*        CLEAR etag_master.
*        IF line_exists( lo_childnode->lt_fields[ name = lo_childnode->field_name-etag_master ] ).
*          etag_master = lo_childnode->lt_fields[ name = lo_childnode->field_name-etag_master ]-cds_view_field.
*        ELSEIF line_exists( lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-etag_master ] ).
*          etag_master = lo_childnode->lt_additional_fields[ name = lo_childnode->field_name-etag_master ]-cds_view_field.
*        ENDIF.
*
*        IF etag_master IS NOT INITIAL.
*          item_characteristics->etag->set_master( etag_master ).
*        ELSE.
*          item_characteristics->etag->set_dependent_by( '_' && lo_childnode->root_node->rap_node_objects-alias ).
*        ENDIF.
*
*        " Characteristics.
*        IF lo_childnode->is_grand_child_or_deeper(  ).
*
*          item_characteristics->set_alias( CONV #( lo_childnode->rap_node_objects-alias )
*            )->set_implementation_class( lo_childnode->rap_node_objects-behavior_implementation
*            )->lock->set_dependent_by( '_' && lo_childnode->root_node->rap_node_objects-alias  ).
*
*          "@todo add again once setting of
*          "authorization master(global)
*          "is allowed
*          "IF lo_childnode->root_node->is_virtual_root( ).
***********************************************************************
*** Begin of deletion 2108
***********************************************************************
*          "check if set authorization master(global) root node has been set to
*          "MASTER(global) or master(instance)
*          "only in this case child nodes can't be set as dependent_by
*          method_exists_in_interface-interface_name = 'if_xco_gen_bdef_s_fo_b_auth'.
*          method_exists_in_interface-method_name    = 'SET_MASTER_GLOBAL'.
*
*
*          IF xco_api->method_exists_in_interface(
*                     interface_name = method_exists_in_interface-interface_name
*                     method_name    = method_exists_in_interface-method_name
*                   ).
*            DATA(item_authorization) = item_characteristics->authorization.
*            DATA(authorization_association) =  |_{ lo_childnode->root_node->rap_node_objects-alias }|.
*            item_authorization->set_dependent_by( CONV sxco_cds_association_name( authorization_association ) ).
*            method_exists_in_interface-method_exists = abap_true.
*          ELSE.
*            method_exists_in_interface-method_exists = abap_false.
*          ENDIF.
*          APPEND method_exists_in_interface TO method_exists_in_interfaces.
*
*          "if setting authorization master(global) fails
*          "try authorization master(instance)
*
*          IF method_exists_in_interface-method_exists = abap_false.
*            method_exists_in_interface-interface_name = 'if_xco_gen_bdef_s_fo_b_auth'.
*            method_exists_in_interface-method_name    = 'SET_MASTER_INSTANCE'.
*            IF xco_api->method_exists_in_interface(
*                 interface_name = method_exists_in_interface-interface_name
*                 method_name    = method_exists_in_interface-method_name
*               ).
*
*              item_authorization = item_characteristics->authorization.
*              authorization_association =  |_{ lo_childnode->root_node->rap_node_objects-alias }|.
*              item_authorization->set_dependent_by( CONV sxco_cds_association_name( authorization_association ) ).
*
*              method_exists_in_interface-method_exists = abap_true.
*            ELSE.
*              method_exists_in_interface-method_exists = abap_false.
*            ENDIF.
*            APPEND method_exists_in_interface TO method_exists_in_interfaces.
*
*          ENDIF.
*
*
*
**          item_characteristics->authorization->set_dependent_by( '_' && lo_childnode->root_node->rap_node_objects-alias  ).
**          "ENDIF.
**
***********************************************************************
*** end of deletion 2108
***********************************************************************
*
*
*          CASE lo_childnode->get_implementation_type(  ).
*            WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*              item_characteristics->set_persistent_table( CONV sxco_dbt_object_name( lo_childnode->persistent_table_name ) ).
*            WHEN   ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*              item_characteristics->set_persistent_table( CONV sxco_dbt_object_name( lo_childnode->persistent_table_name ) ).
*            WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*              "nothing to do
*          ENDCASE.
*
*
*
*          "add association to parent node
*          assoc = lo_item_behavior->add_association( '_' && lo_childnode->parent_node->rap_node_objects-alias  ).
*          assoc->set_draft_enabled( io_rap_bo_node->draft_enabled ).
*
*          "add association to root node
*          assoc = lo_item_behavior->add_association( '_' && lo_childnode->root_node->rap_node_objects-alias  ).
*          assoc->set_draft_enabled( io_rap_bo_node->draft_enabled ).
*
*          IF lo_childnode->root_node->is_customizing_table = abap_true.
*            item_characteristics->set_with_additional_save( ).
*          ENDIF.
*
*        ELSEIF lo_childnode->is_child(  ).
*
*          item_characteristics->set_alias( CONV #( lo_childnode->rap_node_objects-alias )
*                   )->set_implementation_class( lo_childnode->rap_node_objects-behavior_implementation
*                   )->lock->set_dependent_by( '_' && lo_childnode->parent_node->rap_node_objects-alias  ).
*
*
*          "@todo add again once setting of
*          "authorization master(global)
*          "is allowed
***********************************************************************
*** Begin of deletion 2108
***********************************************************************
*
*          "check if set authorization master(global) root node has been set to
*          "MASTER(global) or master(instance)
*          "only in this case child nodes can't be set as dependent_by
*
*          method_exists_in_interface-interface_name = 'if_xco_gen_bdef_s_fo_b_auth'.
*          method_exists_in_interface-method_name    = 'SET_MASTER_GLOBAL'.
*          "if root node can't be set as MASTER GLOBAL child nodes can't be set as dependent_by
*          IF xco_api->method_exists_in_interface(
*                     interface_name = method_exists_in_interface-interface_name
*                     method_name    = method_exists_in_interface-method_name
*                   ).
*            item_authorization = item_characteristics->authorization.
*            authorization_association =  |_{ lo_childnode->root_node->rap_node_objects-alias }|.
*            item_authorization->set_dependent_by( CONV sxco_cds_association_name( authorization_association ) ).
*            method_exists_in_interface-method_exists = abap_true.
*          ELSE.
*            method_exists_in_interface-method_exists = abap_false.
*          ENDIF.
*          APPEND method_exists_in_interface TO method_exists_in_interfaces.
*
*
*          IF method_exists_in_interface-method_exists = abap_false.
*            method_exists_in_interface-interface_name = 'if_xco_gen_bdef_s_fo_b_auth'.
*            method_exists_in_interface-method_name    = 'SET_MASTER_INSTANCE'.
*            IF xco_api->method_exists_in_interface(
*                 interface_name = method_exists_in_interface-interface_name
*                 method_name    = method_exists_in_interface-method_name
*               ).
*
*              item_authorization = item_characteristics->authorization.
*              authorization_association =  |_{ lo_childnode->root_node->rap_node_objects-alias }|.
*              item_authorization->set_dependent_by( CONV sxco_cds_association_name( authorization_association ) ).
*
*              method_exists_in_interface-method_exists = abap_true.
*            ELSE.
*              method_exists_in_interface-method_exists = abap_false.
*            ENDIF.
*            APPEND method_exists_in_interface TO method_exists_in_interfaces.
*
*          ENDIF.
*
*
**          item_characteristics->authorization->set_dependent_by( '_' && lo_childnode->parent_node->rap_node_objects-alias  ).
*
***********************************************************************
*** end of deletion 2108
***********************************************************************
*
*          IF lo_childnode->root_node->is_customizing_table = abap_true.
*            item_characteristics->set_with_additional_save( ).
*          ENDIF.
*
*
*          CASE lo_childnode->get_implementation_type(  ).
*            WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*              item_characteristics->set_persistent_table( CONV sxco_dbt_object_name( lo_childnode->persistent_table_name ) ).
*            WHEN   ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*              item_characteristics->set_persistent_table( CONV sxco_dbt_object_name( lo_childnode->persistent_table_name  ) ).
*            WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*              "set no persistent table
*          ENDCASE.
*
*
*          "add association to parent node
*          assoc = lo_item_behavior->add_association( '_' && lo_childnode->parent_node->rap_node_objects-alias  ).
*          assoc->set_draft_enabled( io_rap_bo_node->draft_enabled ).
*
*        ELSE.
*          "should not happen
*
*          RAISE EXCEPTION TYPE ZDMO_cx_rap_generator
*            MESSAGE ID 'ZDMO_CM_RAP_GEN_MSG' TYPE 'E' NUMBER '001'
*            WITH lo_childnode->entityname lo_childnode->root_node->entityname.
*
*        ENDIF.
*
*
*        IF lo_childnode->has_childs(  ).
*          LOOP AT lo_childnode->childnodes INTO DATA(lo_grandchildnode).
*            assoc = lo_item_behavior->add_association( '_' && lo_grandchildnode->rap_node_objects-alias  ).
*            assoc->set_create_enabled(  ).
*            assoc->set_draft_enabled( io_rap_bo_node->draft_enabled ).
*          ENDLOOP.
*        ENDIF.
*
*        "child nodes only offer update and delete and create by assocation
*        lo_item_behavior->add_standard_operation( xco_cp_behavior_definition=>standard_operation->update ).
*        lo_item_behavior->add_standard_operation( xco_cp_behavior_definition=>standard_operation->delete ).
*
*
*
***********************************************************************
*** Begin of deletion 2020
***********************************************************************
*        IF io_rap_bo_node->is_customizing_table = abap_true.
*
*          lv_validation_name = |val_transport| .
*
*          DATA(item_validation) = lo_item_behavior->add_validation( CONV #( lv_validation_name ) ).
*          "'val_transport'
*          item_validation->set_time( xco_cp_behavior_definition=>evaluation->time->on_save ).
*
**          trigger_operations = VALUE #(
**                                        ( xco_cp_behavior_definition=>evaluation->trigger_operation->create )
**                                        ( xco_cp_behavior_definition=>evaluation->trigger_operation->update )
**                                        "( xco_cp_behavior_definition=>evaluation->trigger_operation->delete )
**                                       ).
*
*          CLEAR trigger_operations.
*          ASSIGN xco_cp_behavior_definition=>evaluation->trigger_operation->('CREATE') TO <fs_create>.
*          ASSIGN xco_cp_behavior_definition=>evaluation->trigger_operation->('UPDATE') TO <fs_update>.
*
*
*          IF <fs_create> IS ASSIGNED.
*            APPEND <fs_create> TO trigger_operations.
*          ENDIF.
*          IF <fs_update> IS ASSIGNED.
*            APPEND <fs_update> TO trigger_operations.
*          ENDIF.
*
*          IF xco_api->method_exists_in_interface(
*            interface_name = 'if_xco_gen_bdef_s_fo_b_validtn'
*            method_name    = 'SET_TRIGGER_OPERATIONS'
*          ).
*
** todo: dynamic call currently fails
**              CALL METHOD item_validation->('SET_TRIGGER_OPERATIONS')
**                IMPORTING
**                  it_trigger_operations = trigger_operations.
*
*            item_validation->set_trigger_operations( trigger_operations ).
*
*          ENDIF.
*        ENDIF.
***********************************************************************
*** End of deletion 2020
***********************************************************************
*
*
*        CASE lo_childnode->get_implementation_type(  ).
*          WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*            "determination CalculateSemanticKey on modify { create; }
*            lv_determination_name = 'Calculate' && lo_childnode->object_id_cds_field_name.
*
*            lo_item_behavior->add_determination( CONV #( lv_determination_name )
*              )->set_time( xco_cp_behavior_definition=>evaluation->time->on_save
*              )->set_trigger_operations( VALUE #( ( xco_cp_behavior_definition=>evaluation->trigger_operation->create ) )  ).
*
*            LOOP AT lt_mapping_item INTO ls_mapping_item.
*              CASE ls_mapping_item-dbtable_field.
*                WHEN lo_childnode->field_name-uuid.
*                  lo_item_behavior->add_field( ls_mapping_item-cds_view_field
*                                 )->set_numbering_managed( )->set_read_only( ).
*                WHEN lo_childnode->field_name-parent_uuid OR
*                     lo_childnode->field_name-root_uuid.
*                  lo_item_behavior->add_field( ls_mapping_item-cds_view_field )->set_read_only( ).
*
*                WHEN  lo_childnode->object_id.
*                  lo_item_behavior->add_field( ls_mapping_item-cds_view_field )->set_read_only( ).
*
*              ENDCASE.
*            ENDLOOP.
*
*          WHEN ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*
*            "key field is not set as read only since at this point we assume
*            "that the key is set externally
*
*            IF lo_childnode->root_node->is_virtual_root(  ).
*              lo_item_behavior->add_field( lo_childnode->singleton_field_name )->set_read_only( ).
*            ENDIF.
*
*
*
*            LOOP AT lo_childnode->lt_fields INTO DATA(ls_fields)
*                   WHERE key_indicator = abap_true AND name <> lo_childnode->field_name-client.
*
*              DATA(key_field_behavior) = lo_item_behavior->add_field( ls_fields-cds_view_field ).
*
*              "sematic key fields that are set via cba have to be read only.
*              "This are these key fields that are not part of the semantic key
*              "of the parent entity
*              "the remaining key fields have to be set as readonly:update
*
*              IF line_exists( lo_childnode->parent_node->semantic_key[ name = ls_fields-name ] ).
*
*                key_field_behavior->set_read_only( ).
*
*              ELSE.
*
*                IF xco_api->method_exists_in_interface( interface_name = 'if_xco_gen_bdef_s_fo_b_field'
*                                                        method_name    = 'SET_READONLY_UPDATE' ).
*                  CALL METHOD key_field_behavior->('SET_READONLY_UPDATE').
*                ENDIF.
*
*              ENDIF.
*
*              "lo_item_behavior->add_field( ls_fields-cds_view_field )->set_readonly_update(  ).
*            ENDLOOP.
*
*          WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*            "make the key fields read only in the child entities
*            "Otherwise you get the warning
*            "The field "<semantic key of root node>" is used for "lock" dependency (in the ON clause of
*            "the association "_Travel"). This means it should be flagged as
*            "readonly / readonly:update".
*
*            "LOOP AT lo_childnode->root_node->lt_fields INTO DATA(ls_fields)
*            LOOP AT lo_childnode->lt_fields INTO ls_fields
*              WHERE key_indicator = abap_true AND name <> lo_childnode->field_name-client.
*              lo_item_behavior->add_field( ls_fields-cds_view_field )->set_read_only( ).
*            ENDLOOP.
*
*
*
*        ENDCASE.
*
*
**  make administrative fields read-only
**  field ( readonly )
**   CreatedAt,
**   CreatedBy,
**   LocalLastChangedAt,
**   LastChangedAt,
**   LastChangedBy;
*
*        LOOP AT lo_childnode->lt_fields INTO ls_fields
*          WHERE name <> lo_childnode->field_name-client.
*          CASE ls_fields-name .
*            WHEN lo_childnode->field_name-created_at OR
*                 lo_childnode->field_name-created_by OR
*                 lo_childnode->field_name-local_instance_last_changed_at OR
*                 lo_childnode->field_name-local_instance_last_changed_by OR
*                 lo_childnode->field_name-last_changed_at OR
*                 lo_childnode->field_name-last_changed_by.
*              lo_item_behavior->add_field( ls_fields-cds_view_field )->set_read_only( ).
*          ENDCASE.
*        ENDLOOP.
*
*        IF lt_mapping_item IS NOT INITIAL.
*          CASE io_rap_bo_node->get_implementation_type(  ).
*            WHEN ZDMO_cl_rap_node=>implementation_type-managed_uuid.
*              "use conv #( ) since importing parameter iv_database_table
*              "was of type sxco_dbt_object_name and has been changed to clike
*              "to support structure names longer than 16 characters as of 2111
*              DATA(item_mapping) = lo_item_behavior->add_mapping_for( CONV #( lo_childnode->persistent_table_name ) ).
*              item_mapping->set_field_mapping( it_field_mappings =  lt_mapping_item ).
*              IF io_rap_bo_node->is_extensible(  ) = abap_true.
*                "item_mapping->set_extensible(  )->set_corresponding(  ).
*                set_extensible_for_mapping( item_mapping ).
*              ENDIF.
*            WHEN ZDMO_cl_rap_node=>implementation_type-managed_semantic.
*              item_mapping = lo_item_behavior->add_mapping_for( CONV #( lo_childnode->persistent_table_name ) ).
*              item_mapping->set_field_mapping( it_field_mappings =  lt_mapping_item ).
*              IF io_rap_bo_node->is_extensible(  ) = abap_true.
*                "item_mapping->set_extensible(  )->set_corresponding(  ).
*                set_extensible_for_mapping( item_mapping ).
*              ENDIF.
*            WHEN ZDMO_cl_rap_node=>implementation_type-unmanaged_semantic.
*              "add control structure
*              IF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-table.
*                item_mapping = lo_item_behavior->add_mapping_for( CONV #( lo_childnode->persistent_table_name ) ).
*                item_mapping->set_field_mapping( it_field_mappings = lt_mapping_item )->set_control( lo_childnode->rap_node_objects-control_structure ).
*              ELSEIF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-structure.
*                item_mapping = lo_item_behavior->add_mapping_for( CONV #( lo_childnode->structure_name ) ).
*                item_mapping->set_field_mapping( it_field_mappings = lt_mapping_item )->set_control( lo_childnode->rap_node_objects-control_structure ).
*              ELSEIF io_rap_bo_node->data_source_type = io_rap_bo_node->data_source_types-abap_type.
*                "structure name is added here since we only support abap types that are based on structures
*                item_mapping = lo_item_behavior->add_mapping_for( CONV #( lo_childnode->structure_name ) ).
*                item_mapping->set_field_mapping( it_field_mappings = lt_mapping_item )->set_control( lo_childnode->rap_node_objects-control_structure ).
*              ENDIF.
*
*              IF io_rap_bo_node->is_extensible(  ) = abap_true.
*                "item_mapping->set_extensible(  )->set_corresponding(  ).
*                set_extensible_for_mapping( item_mapping ).
*              ENDIF.
*
*          ENDCASE.
*        ENDIF.
*
*
*
*      ENDLOOP.
*
*    ENDIF.


    ENDLOOP.



*
*    DATA(ls_result) = lo_operation->execute( ).
*
*    ro_behavior_definition = NEW #( ).
*    ro_behavior_definition->gv_behavior_definition_name = is_create-behavior_definition-name.

  ENDMETHOD.


  METHOD get_instance.

    ro_behavior_definition = NEW #( ).
    ro_behavior_definition->gv_behavior_definition_name = iv_behavior_definition_name.

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
