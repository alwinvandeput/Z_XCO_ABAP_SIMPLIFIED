CLASS z_xco_cds_view_deep_read_dp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_layer,
        layer     TYPE i,
        cds_views TYPE STANDARD TABLE OF Z_XCO_CDS_VIEW_ENTITY=>ts_data WITH EMPTY KEY,
      END OF ts_layer,
      tt_layers TYPE STANDARD TABLE OF ts_layer WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_cds_view_deep_data,
        root_cds_view_name TYPE Z_XCO_CDS_VIEW_ENTITY=>tv_cds_view_name,
        layers             TYPE tt_layers,
        database_tables    TYPE STANDARD TABLE OF zzap_db_table_bo=>ts_data WITH EMPTY KEY,
      END OF ts_cds_view_deep_data.

    METHODS read_in_layers
      IMPORTING iv_cds_view_name TYPE Z_XCO_CDS_VIEW_ENTITY=>tv_cds_view_name
      RETURNING VALUE(rs_data)   TYPE ts_cds_view_deep_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _read_cds_view
      IMPORTING
        iv_cds_view_name       TYPE Z_XCO_CDS_VIEW_ENTITY=>tv_cds_view_name
        io_repo_object_factory TYPE REF TO zzap_repo_object_factory
      EXPORTING
        es_cds_view_data       TYPE Z_XCO_CDS_VIEW_ENTITY=>ts_data
      CHANGING
        cs_deep_layer          TYPE ts_layer
        cs_data                TYPE ts_cds_view_deep_data.

ENDCLASS.



CLASS z_xco_cds_view_deep_read_dp IMPLEMENTATION.

  METHOD read_in_layers.

    DATA(lo_repo_object_factory) = zzap_repo_object_factory=>get_instance(
      iv_buffering_ind = abap_true ).

    rs_data-root_cds_view_name = iv_cds_view_name.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_cds_view_name) = iv_cds_view_name.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Loop at the layers
    DATA lv_layer_counter TYPE i.

    DO.

      lv_layer_counter += 1.

      APPEND INITIAL LINE TO rs_data-layers
             ASSIGNING FIELD-SYMBOL(<ls_deep_layer>).

      <ls_deep_layer>-layer = lv_layer_counter.

      DATA ls_cds_view_data TYPE Z_XCO_CDS_VIEW_ENTITY=>ts_data.

      _read_cds_view(
        EXPORTING
          iv_cds_view_name = lv_cds_view_name
          io_repo_object_factory = lo_repo_object_factory
        IMPORTING
          es_cds_view_data = ls_cds_view_data
        CHANGING
          cs_deep_layer = <ls_deep_layer>
          cs_data = rs_data ).

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Next Root CDS View
      IF ls_cds_view_data-data_source IS INITIAL.
        EXIT.
      ENDIF.

      IF ls_cds_view_data-data_source-type <> 'DDLS'.
        EXIT.
      ENDIF.

      lv_cds_view_name = ls_cds_view_data-data_source-name.

      "Safety check to overcome endless loops
      IF lv_layer_counter > 30.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD _read_cds_view.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Read Root CDS View
*    DATA(lo_cds_view_bo) = CAST Z_XCO_CDS_VIEW_ENTITY(
*      io_repo_object_factory->get_repository_object(
*        iv_object_type = 'DDLS'
*        iv_object_name = CONV #( iv_cds_view_name ) ) ).

    DATA(lo_cds_view_bo) = z_xco_cds_view_entity=>get_instance( iv_cds_view_name ).

    es_cds_view_data = lo_cds_view_bo->get_data( ).

    APPEND es_cds_view_data TO cs_deep_layer-cds_views.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Add Database Table
    IF es_cds_view_data-data_source-type = 'TABL'.

      DATA(lo_db_table_bo) = zzap_db_table_bo=>get_instance( CONV #( es_cds_view_data-data_source-name ) ).
      DATA(ls_db_table_data) = lo_db_table_bo->get_data( ).
      APPEND ls_db_table_data TO cs_data-database_tables.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Add Compositions

    LOOP AT es_cds_view_data-compositions
         ASSIGNING FIELD-SYMBOL(<ls_composition>).

      _read_cds_view(
        EXPORTING
          iv_cds_view_name       = <ls_composition>-entity_name
          io_repo_object_factory = io_repo_object_factory
        IMPORTING
          es_cds_view_data       = DATA(ls_cds_view_data)
        CHANGING
          cs_deep_layer          = cs_deep_layer
          cs_data                = cs_data ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
