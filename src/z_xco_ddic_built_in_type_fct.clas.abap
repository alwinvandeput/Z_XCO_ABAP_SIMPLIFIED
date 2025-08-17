CLASS z_xco_ddic_built_in_type_fct DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tv_data_type TYPE c LENGTH 10.

    "See also:
    "- IF_XCO_AD_CONSTANTS=>CO_BUILT_IN_TYPE, but is not released
    "  - Method FOR returns


    CONSTANTS:
      BEGIN OF cs_data_type,
        client                TYPE tv_data_type VALUE 'CLNT',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Character types
        " - Fixed length
        character             TYPE tv_data_type VALUE 'CHAR',
        " - Max.: 2GB
        string                TYPE tv_data_type VALUE 'STRG',
        " - Up to 255 characters?
        short_string          TYPE tv_data_type VALUE 'SSTR',
        " - Up to 32.000 characters
        long_character        TYPE tv_data_type VALUE 'LCHR',

        language              TYPE tv_data_type VALUE 'LANG',
        " - Max.: 255 characters
        number_character      TYPE tv_data_type VALUE 'NUMC',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Currency and Quantity
        currency              TYPE tv_data_type VALUE 'CURR',
        currency_key          TYPE tv_data_type VALUE 'CUKY',

        quantity              TYPE tv_data_type VALUE 'QUAN',
        unit                  TYPE tv_data_type VALUE 'UNIT',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Date / Time
        date                  TYPE tv_data_type VALUE 'DATS',
        time                  TYPE tv_data_type VALUE 'TIMS',
        posting_period_yyyymm TYPE tv_data_type VALUE 'ACCP',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Float
        " - 16 digits - format: ABAP
        binary_float          TYPE tv_data_type VALUE 'FLTP',
        " - 16 digits - format: Binary
        binary_float_16       TYPE tv_data_type VALUE 'D16R',
        "- 34 digits - format: Binary
        binary_float_34       TYPE tv_data_type VALUE 'D34R',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Integer
        " - Max.: 255
        integer_1             TYPE tv_data_type VALUE 'INT1',
        " - Max.: ? - 16 bit
        integer_2             TYPE tv_data_type VALUE 'INT2',
        " - Max.: ? - 32 bit
        integer_4             TYPE tv_data_type VALUE 'INT4',
        " - Max.: ? - 64 bit
        integer_8             TYPE tv_data_type VALUE 'INT8',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Decimals
        decimals              TYPE tv_data_type VALUE 'DEC',
        decimals_16           TYPE tv_data_type VALUE 'D16D',
        decimals_34           TYPE tv_data_type VALUE 'D34D',

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Binary
        "- Max.: 255 bytes
        raw                   TYPE tv_data_type VALUE 'RAW',
        "- Max.: 32.000 bytes
        long_binary           TYPE tv_data_type VALUE 'LRAW',
        "- Max.: 2GB
        rawstring             TYPE tv_data_type VALUE 'RSTR',

      END OF cs_data_type.

    METHODS get_built_in_type
      IMPORTING iv_data_type            TYPE tv_data_type
                iv_length               TYPE cl_xco_ad_built_in_type_f=>tv_length OPTIONAL
                iv_decimals             TYPE cl_xco_ad_built_in_type_f=>tv_decimals OPTIONAL
      RETURNING VALUE(ro_built_in_type) TYPE REF TO cl_xco_ad_built_in_type.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z_XCO_DDIC_BUILT_IN_TYPE_FCT IMPLEMENTATION.


  METHOD get_built_in_type.


    CASE iv_data_type.

      WHEN cs_data_type-posting_period_yyyymm.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->accp.

      WHEN cs_data_type-client.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->clnt.

      WHEN cs_data_type-currency_key.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->cuky.

      WHEN cs_data_type-date.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->dats.

      WHEN cs_data_type-binary_float_16.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->df16_raw.
      WHEN cs_data_type-binary_float_34.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->df34_raw.
      WHEN cs_data_type-binary_float.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->fltp.

      WHEN cs_data_type-integer_1.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->int1.
      WHEN cs_data_type-integer_2.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->int2.
      WHEN cs_data_type-integer_4.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->int4.
      WHEN cs_data_type-integer_8.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->int8.

      WHEN cs_data_type-language.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->lang.

      WHEN cs_data_type-time.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->tims.

      WHEN cs_data_type-character.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->char( iv_length ).

      WHEN cs_data_type-currency.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->curr(
          iv_length   = iv_length
          iv_decimals = iv_decimals ).

      WHEN cs_data_type-decimals.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->dec(
          iv_length   = iv_length
          iv_decimals = iv_decimals ).

      WHEN cs_data_type-decimals_16.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->df16_dec(
          iv_length   = iv_length
          iv_decimals = iv_decimals ).

      WHEN cs_data_type-decimals_34.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->df34_dec(
          iv_length   = iv_length
          iv_decimals = iv_decimals ).

      WHEN cs_data_type-long_character.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->lchr( iv_length ).

      WHEN cs_data_type-long_binary.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->lraw( iv_length ).

      WHEN cs_data_type-number_character.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->numc( iv_length ).

      WHEN cs_data_type-quantity.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->quan(
          iv_length   = iv_length
          iv_decimals = iv_decimals ).

      WHEN cs_data_type-raw.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->raw( iv_length ).

      WHEN cs_data_type-rawstring.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->rawstring( iv_length ).

      WHEN cs_data_type-short_string.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->sstring( iv_length ).

      WHEN cs_data_type-string.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->string( iv_length ).

      WHEN cs_data_type-unit.
        ro_built_in_type = xco_cp_abap_dictionary=>built_in_type->unit( iv_length ).

      WHEN OTHERS.
        ASSERT 1 = 0.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
