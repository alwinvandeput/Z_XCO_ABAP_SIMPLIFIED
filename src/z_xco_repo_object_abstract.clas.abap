CLASS z_xco_repo_object_abstract DEFINITION
  PUBLIC
  abstract
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_key,
        type TYPE string,
        name TYPE string,
      END OF ts_key.

    METHODS get_key abstract
      RETURNING VALUE(rs_key) TYPE ts_key.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS z_xco_repo_object_abstract IMPLEMENTATION.

ENDCLASS.
