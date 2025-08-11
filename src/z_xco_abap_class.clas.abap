CLASS z_xco_abap_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_parm_data_type_category TYPE c LENGTH 10.

    TYPES:
      BEGIN OF ts_interface,
        name TYPE sxco_ao_object_name,
      END OF ts_interface,
      tt_interfaces TYPE STANDARD TABLE OF ts_interface WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_type,
        name   TYPE  sxco_ao_component_name,
        source TYPE string,
      END OF ts_type,
      tt_types TYPE STANDARD TABLE OF ts_type WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_parameter_behavior,
        request_name          TYPE c LENGTH 30,
        for_entity_alias_name TYPE string,
      END OF ts_parameter_behavior.

    TYPES:
      BEGIN OF ts_method_parameter,
        name           TYPE sxco_ao_subcomponent_name,
        type_category  TYPE tv_parm_data_type_category,
        type_name      TYPE string,
        interface_name TYPE sxco_ao_object_name,
        class_name     TYPE sxco_ao_object_name,
        default_value  TYPE string,
        behavior       TYPE ts_parameter_behavior,
      END OF ts_method_parameter,
      tt_method_parameters TYPE STANDARD TABLE OF ts_method_parameter WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_method_behavior,
        global_authorization  TYPE abap_bool,
        result_parameter_name TYPE sxco_ao_subcomponent_name,
      END OF ts_method_behavior.

    TYPES:
      BEGIN OF ts_method_definition,
        name                 TYPE if_xco_gen_clas_s_fo_d_section=>tv_method_name,
        importing_parameters TYPE tt_method_parameters,
        exporting_parameters TYPE tt_method_parameters,
        changing_parameters  TYPE tt_method_parameters,
        returning_parameter  TYPE ts_method_parameter,
        behavior             TYPE ts_method_behavior,
      END OF ts_method_definition,
      tt_method_definitions TYPE STANDARD TABLE OF ts_method_definition WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_method_implementation,
        name         TYPE if_xco_gen_clas_s_fo_d_section=>tv_method_name,
        source_lines TYPE if_xco_gen_clas_s_fo_i_method=>tt_source,
      END OF ts_method_implementation,
      tt_method_implementations TYPE STANDARD TABLE OF ts_method_implementation WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_section,
        types              TYPE tt_types,
        method_definitions TYPE tt_method_definitions,
      END OF ts_section.

    TYPES:
      BEGIN OF ts_class,
        name                     TYPE sxco_ao_object_name,
        behavior_definition_name TYPE sxco_cds_object_name,
        super_class_name         TYPE sxco_ao_object_name,

        final                    TYPE abap_bool,
        abstract                 TYPE abap_bool,

        interfaces               TYPE tt_interfaces,
        public_section           TYPE ts_section,
        protected_section        TYPE ts_section,
        private_section          TYPE ts_section,
        method_implementations   TYPE tt_method_implementations,
      END OF ts_class,
      tt_classes TYPE STANDARD TABLE OF ts_class WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_data,
        short_description TYPE if_xco_domain_content=>ts_content-short_description,
        main_class        TYPE ts_class,
        local_classes     TYPE tt_classes,
        test_classes      TYPE tt_classes,
      END OF ts_data.

    TYPES:
      BEGIN OF ts_create,
        transport_request TYPE sxco_transport,
        package           TYPE sxco_package,
        class             TYPE ts_data,
      END OF ts_create.

    CONSTANTS:
      BEGIN OF cs_data_type_category,
        type      TYPE tv_parm_data_type_category VALUE 'TYPE',
        interface TYPE tv_parm_data_type_category VALUE 'INTERFACE',
        class     TYPE tv_parm_data_type_category VALUE 'CLASS',
      END OF cs_data_type_category.

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create            TYPE ts_create
      RETURNING VALUE(ro_abap_class) TYPE REF TO z_xco_abap_class.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_class_name TYPE sxco_ao_object_name.

    CLASS-METHODS _set_section
      IMPORTING
        is_section TYPE ts_section
        io_section TYPE if_xco_gen_clas_s_fo_defntn=>ts_section-public.

    CLASS-METHODS _set_main_class
      IMPORTING
        io_specification TYPE REF TO if_xco_cp_gen_clas_s_form
        is_create        TYPE z_xco_abap_class=>ts_create.

    CLASS-METHODS _set_types
      IMPORTING
        is_section TYPE z_xco_abap_class=>ts_section
        io_section TYPE if_xco_gen_clas_s_fo_defntn=>ts_section-public.

    CLASS-METHODS _set_local_classes
      IMPORTING
        it_classes       TYPE tt_classes
        io_specification TYPE REF TO if_xco_cp_gen_clas_s_form.

    CLASS-METHODS _set_test_classes
      IMPORTING
        it_classes       TYPE tt_classes
        io_specification TYPE REF TO if_xco_cp_gen_clas_s_form.

    CLASS-METHODS _set_class_definition
      IMPORTING
        is_create_class TYPE ts_class
        io_definition   TYPE REF TO if_xco_gen_clas_s_fo_defntn.

    CLASS-METHODS _set_class_implementation
      IMPORTING
        is_create_class   TYPE z_xco_abap_class=>ts_class
        io_implementation TYPE REF TO if_xco_gen_clas_s_fo_implmtn.

    CLASS-METHODS _set_method_definitions
      IMPORTING
        is_section TYPE z_xco_abap_class=>ts_section
        io_section TYPE if_xco_gen_clas_s_fo_defntn=>ts_section-public.

ENDCLASS.


CLASS z_xco_abap_class IMPLEMENTATION.

  METHOD create_or_update_instance.

    DATA(lo_operation) = xco_cp_generation=>environment->dev_system( is_create-transport_request )->create_put_operation( ).

    DATA(lo_put_object) = lo_operation->for-clas->add_object( iv_name = is_create-class-main_class-name ).

    lo_put_object->set_package( is_create-package ).
    DATA(lo_specification) = lo_put_object->create_form_specification( ).

    lo_specification->set_short_description( is_create-class-short_description ).

    _set_main_class(
      is_create         = is_create
      io_specification = lo_specification ).

    _set_local_classes(
      it_classes       = is_create-class-local_classes
      io_specification = lo_specification ).

    _set_test_classes(
      it_classes       = is_create-class-test_classes
      io_specification = lo_specification ).

    DATA(result) = lo_operation->execute( ).

    ro_abap_class = NEW #( ).
    ro_abap_class->gv_class_name = is_create-class-main_class-name.

  ENDMETHOD.

  METHOD _set_class_definition.

    io_definition->set_final( is_create_class-final ).
    io_definition->set_abstract( is_create_class-abstract ).

    io_definition->set_for_behavior_of( is_create_class-behavior_definition_name ).

    io_definition->set_superclass( is_create_class-super_class_name ).

    "Interfaces
    LOOP AT is_create_class-interfaces
      ASSIGNING FIELD-SYMBOL(<ls_inteface>).
      io_definition->add_interface( <ls_inteface>-name  ).
    ENDLOOP.

    "Sections (public, protected, private)
    _set_section(
      is_section = is_create_class-public_section
      io_section = io_definition->section-public ).
    _set_section(
      is_section = is_create_class-protected_section
      io_section = io_definition->section-protected ).
    _set_section(
      is_section = is_create_class-private_section
      io_section = io_definition->section-private ).

  ENDMETHOD.

  METHOD _set_section.

    " Types
    _set_types(
      is_section = is_section
      io_section = io_section ).

    "TODO: Definition - Section
*    io_section->add_constant( ).

*    io_section->add_class_data( )
*    io_section->add_data( ).

*    io_section->add_class_method( )

*    io_section->add_method( ).
    _set_method_definitions(
      is_section = is_section
      io_section = io_section ).

*    io_section->add_alias( ).

  ENDMETHOD.


  METHOD _set_main_class.

    _set_class_definition(
      is_create_class = is_create-class-main_class
      io_definition = io_specification->definition ).

    _set_class_implementation(
      is_create_class = is_create-class-main_class
      io_implementation = io_specification->implementation ).

  ENDMETHOD.


  METHOD _set_types.

    LOOP AT is_section-types
      ASSIGNING FIELD-SYMBOL(<ls_type>).

      " Types
      DATA(lo_type) = io_section->add_type( <ls_type>-name ).
      lo_type->for( xco_cp_abap=>type-source->for( <ls_type>-source ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_local_classes.

    LOOP AT it_classes
      ASSIGNING FIELD-SYMBOL(<ls_local_class>).

      DATA(lo_class) = io_specification->add_local_class( <ls_local_class>-name ).

      _set_class_definition(
        is_create_class = <ls_local_class>
        io_definition = lo_class->definition ).

      _set_class_implementation(
        is_create_class = <ls_local_class>
        io_implementation = lo_class->implementation ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_test_classes.

    LOOP AT it_classes
      ASSIGNING FIELD-SYMBOL(<ls_test_class>).

      DATA(lo_class) = io_specification->add_test_class( <ls_test_class>-name ).

      _set_class_definition(
        is_create_class = <ls_test_class>
        io_definition = lo_class->definition ).

      _set_class_implementation(
        is_create_class = <ls_test_class>
        io_implementation = lo_class->implementation ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_class_implementation.

    LOOP AT is_create_class-method_implementations
         ASSIGNING FIELD-SYMBOL(<ls_implementation>).

      DATA(lo_method) = io_implementation->add_method( <ls_implementation>-name ).
      lo_method->set_source( <ls_implementation>-source_lines ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_method_definitions.

    LOOP AT is_section-method_definitions
      ASSIGNING FIELD-SYMBOL(<ls_method_definition>).

      DATA(lo_method) = io_section->add_method( <ls_method_definition>-name ).

      lo_method->behavior_implementation->set_for_global_authorization( <ls_method_definition>-behavior-global_authorization ).

      lo_method->behavior_implementation->set_result( <ls_method_definition>-behavior-result_parameter_name  ).

      LOOP AT <ls_method_definition>-importing_parameters
        ASSIGNING FIELD-SYMBOL(<ls_parameter>).

        DATA(lo_parameter) = lo_method->add_importing_parameter( <ls_parameter>-name ).


        "Behavior
        lo_parameter->behavior_implementation->set_for( <ls_parameter>-behavior-for_entity_alias_name ).

        lo_parameter->behavior_implementation->set_request( <ls_parameter>-behavior-request_name ).

        CASE <ls_parameter>-type_category.
          WHEN cs_data_type_category-type.
            lo_parameter->set_type( xco_cp_abap=>type-source->for( <ls_parameter>-type_name ) ).
          WHEN cs_data_type_category-interface.
            lo_parameter->set_type( xco_cp_abap=>interface( <ls_parameter>-interface_name ) ).
          WHEN cs_data_type_category-class.
            lo_parameter->set_type( xco_cp_abap=>class( <ls_parameter>-class_name ) ).
        ENDCASE.

        lo_parameter->set_default_value( <ls_parameter>-default_value ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
