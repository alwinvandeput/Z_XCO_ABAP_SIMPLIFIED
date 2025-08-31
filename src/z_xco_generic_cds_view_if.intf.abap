INTERFACE z_xco_generic_cds_view_if

  PUBLIC .

  TYPES tv_cds_object_name TYPE sxco_cds_object_name.

  TYPES:
    BEGIN OF ts_data_source,
      type       TYPE sxco_ar_object_type,
      name       TYPE  z_xco_cds_data_definition=>tv_cds_object_name,
      alias_name TYPE sxco_ddef_alias_name,
    END OF ts_data_source.

  TYPES:
    BEGIN OF ts_cardinality,
      min TYPE i,
      max TYPE i,
    END OF ts_cardinality.

  TYPES:
    BEGIN OF ts_composition,
      entity_name TYPE tv_cds_object_name,
      alias_name  TYPE sxco_ddef_alias_name,
      cardinality TYPE ts_cardinality,
    END OF ts_composition,
    tt_compositions TYPE STANDARD TABLE OF ts_composition WITH EMPTY KEY.

  TYPES:
    BEGIN OF ts_field,
      name       TYPE sxco_cds_object_name,
      alias_name TYPE sxco_ddef_alias_name,
    END OF ts_field,
    tt_fields TYPE STANDARD TABLE OF ts_field WITH EMPTY KEY.

  TYPES:
    BEGIN OF ts_data,
      name         TYPE tv_cds_object_name,
      data_source  TYPE ts_data_source,
      compositions TYPE tt_compositions,
      fields       TYPE tt_fields,
    END OF ts_data.


  TYPES:
    BEGIN OF ts_select_data,
      underlying_object TYPE abap_boolean,
    END OF ts_select_data.

  METHODS get_data
    "TODO implement select_data in implementing classes
    IMPORTING select_data                  TYPE ts_select_data optional
    RETURNING VALUE(rs_view_abstract_data) TYPE ts_data.

ENDINTERFACE.
