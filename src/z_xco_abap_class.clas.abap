CLASS z_xco_abap_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
      BEGIN OF ts_section,
        types TYPE tt_types,
      END OF ts_section.

    TYPES:
      BEGIN OF ts_class,
        name                   TYPE sxco_ao_object_name,
        interfaces             TYPE tt_interfaces,
        public_section         TYPE ts_section,
        protected_section      TYPE ts_section,
        private_section        TYPE ts_section,
        implementation_methods TYPE string_table,
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

    CLASS-METHODS create_or_update_instance
      IMPORTING is_create            TYPE ts_create
      RETURNING VALUE(ro_abap_class) TYPE REF TO z_xco_abap_class.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_class_name TYPE sxco_ao_object_name.

    CLASS-METHODS _set_section
      IMPORTING
        is_section TYPE ts_section
        io_section TYPE if_xco_gen_clas_s_fo_defntn=>ts_section-public
      .

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
        is_create_class TYPE z_xco_abap_class=>ts_class
        io_definition   TYPE REF TO if_xco_gen_clas_s_fo_implmtn.

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


  METHOD _set_section.

    " Types
    _set_types(
      is_section = is_section
      io_section = io_section ).

    "TODO
*    io_section->add_constant( ).
*    io_section->add_class_data( )
*    io_section->add_data( ).
*    io_section->add_class_method( )
*    io_section->add_method( ).
*    io_section->add_alias( ).

  ENDMETHOD.


  METHOD _set_main_class.

    _set_class_definition(
      is_create_class = is_create-class-main_class
      io_definition = io_specification->definition ).

    _set_class_implementation(
      is_create_class = is_create-class-main_class
      io_definition = io_specification->implementation ).


*    "Method Implementations
*    DATA(lo_method) = io_specification->implementation->add_method( 'IF_OO_ADT_CLASSRUN~MAIN' ).
*    lo_method->set_source(
*        VALUE #( ( `    out->write( 'Hello 5' ).` ) ) ).

*    lo_specification->implementation->add_method( 'IF_OO_ADT_CLASSRUN~MAIN' )->set_source(
*      VALUE #( ( `    say_hello( name = 'Bernd' out = out ).` ) ) ).

*DATA(method) = specification->definition->section-private->add_method( 'SAY_HELLO' ).
*
*method->add_importing_parameter( 'NAME' )->set_default_value( `'Herbert'` )->set_type(
*    xco_cp_abap=>type-source->for( 'name' ) ).
*method->add_importing_parameter( 'OUT' )->set_type( xco_cp_abap=>interface( 'IF_OO_ADT_CLASSRUN_OUT' ) ).
*
*specification->implementation->add_method( 'SAY_HELLO' )->set_source(
*    VALUE #( ( `    out->write( |Hello { name }| ).` ) ) ).

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
        io_definition = io_specification->implementation ).

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
        io_definition = io_specification->implementation ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _set_class_definition.

    "Interfaces
    LOOP AT is_create_class-interfaces
      ASSIGNING FIELD-SYMBOL(<ls_inteface>).
      io_definition->add_interface( <ls_inteface>-name  ).
    ENDLOOP.

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


  METHOD _set_class_implementation.

  ENDMETHOD.

ENDCLASS.
