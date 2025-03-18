CLASS zcl_work_order_crud_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    METHODS:
*     Declaration of the method that create the work orders
      test_create_work_order IMPORTING iv_customer_id    TYPE string
                                       iv_technician_id  TYPE string
                                       iv_priority       TYPE string
                                       iv_description    TYPE string
                             RETURNING VALUE(rv_success) TYPE string,

*     Declaration of the method that reads the work orders
      test_read_work_order IMPORTING iv_work_order_id TYPE string
                           RETURNING VALUE(rv_result) TYPE ztwork_order,

*     Declaration of the method that update the work orders
      test_update_work_order IMPORTING iv_work_order_id  TYPE string
                                       iv_status         TYPE string
                                       iv_customer       TYPE string
                                       iv_priority       TYPE string
                                       iv_technician     TYPE string
                                       iv_description    TYPE string
                             RETURNING VALUE(rv_success) TYPE string,

*     Declaration of the method that delete the work orders
      test_delete_work_order IMPORTING iv_work_order_id  TYPE string
                             RETURNING VALUE(rv_success) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: lo_crud TYPE REF TO zcl_work_order_crud_handler.
ENDCLASS.



CLASS zcl_work_order_crud_test IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.


*Crear
*
*    DATA lo_test TYPE REF TO zcl_work_order_crud_test.
*    DATA result TYPE string.
*
*    IF lo_test IS NOT BOUND.
*      lo_test = NEW zcl_work_order_crud_test( ).
*    ENDIF.
*    lo_test = NEW zcl_work_order_crud_test( ).
*
*    result = lo_test->test_create_work_order( iv_customer_id = '2'
*                                                      iv_technician_id = '3'
*                                                      iv_priority = 'A'
*                                                      iv_description = 'Orden muy importante' ).
*
*    out->write( result ).


* Leer
*
*    DATA lo_test TYPE REF TO zcl_work_order_crud_test.
*    IF lo_test IS NOT BOUND.
*      lo_test = NEW zcl_work_order_crud_test( ).
*    ENDIF.
*
*    DATA(lv_prueba) = lo_test->test_read_work_order( iv_work_order_id = '2' ).
*    out->write( lv_prueba ).
*
*actualizar
*
*    DATA lo_test TYPE REF TO zcl_work_order_crud_test.
*    DATA result TYPE string.
*
*    IF lo_test IS NOT BOUND.
*      lo_test = NEW zcl_work_order_crud_test( ).
*    ENDIF.
*    lo_test = NEW zcl_work_order_crud_test( ).
*
*    result = lo_test->test_update_work_order( iv_work_order_id = '3'
*                                              iv_customer = '1'
*                                              iv_priority = 'A'
*                                              iv_technician = '3'
*                                              iv_status = 'CO'
*                                              iv_description = 'Ora ora' ).
*
*    out->write( result ).

* Borrar

    DATA lo_test TYPE REF TO zcl_work_order_crud_test.
    IF lo_test IS NOT BOUND.
      lo_test = NEW zcl_work_order_crud_test( ).
    ENDIF.

    DATA(lv_prueba) = lo_test->test_delete_work_order( iv_work_order_id = '4' ).

    out->write( lv_prueba ).
*
*    DATA ls_in TYPE zttechnician.
*
*    ls_in = VALUE #( name = 'Utak Jung' specialty = 'Policia' technician_id = 3 ).
*
*    INSERT zttechnician FROM @ls_in.
*
*    IF sy-subrc = 0.
*      out->write( 'si' ).
*    ELSE.
*      out->write( 'no' ).
*    ENDIF.
*
*    SELECT SINGLE FROM zttechnician
*    FIELDS *
*    WHERE technician_id = 3
*    INTO @DATA(registro).
*
*    DELETE FROM zttechnician.
*
*    DATA r TYPE zttechnician.
*
*    r = VALUE #( technician_id = '3' name = 'Bruno Bucciarati' specialty = 'Team Manager' ).
*    INSERT zttechnician FROM @r.



  ENDMETHOD.

  METHOD test_create_work_order.

    IF lo_crud IS NOT BOUND.
      lo_crud = NEW zcl_work_order_crud_handler( ).
    ENDIF.

    DATA(lv_validation) = lo_crud->create_work_order( iv_customer_id = iv_customer_id
                                                      iv_technician_id = iv_technician_id
                                                      iv_priority = iv_priority
                                                      iv_description = iv_description ).
    IF lv_validation IS INITIAL.
      rv_success = TEXT-001.
      RETURN.
    ELSE.
      rv_success = lv_validation.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD test_read_work_order.
    IF lo_crud IS NOT BOUND.
      lo_crud = NEW zcl_work_order_crud_handler( ).
    ENDIF.

    DATA(ls_structure) = lo_crud->read_work_order( iv_work_order_id = iv_work_order_id ).

    rv_result = ls_structure.
    RETURN.

  ENDMETHOD.

  METHOD test_update_work_order.
    IF lo_crud IS NOT BOUND.
      lo_crud = NEW zcl_work_order_crud_handler( ).
    ENDIF.

    DATA(lv_validation) = lo_crud->update_work_order( iv_work_order_id = iv_work_order_id
                                                      iv_customer = iv_customer
                                                      iv_technician = iv_technician
                                                      iv_priority = iv_priority
                                                      iv_status = iv_status
                                                      iv_description = iv_description ).
    IF lv_validation IS INITIAL.
      rv_success = TEXT-002.
      RETURN.
    ELSE.
      rv_success = lv_validation.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD test_delete_work_order.
    IF lo_crud IS NOT BOUND.
      lo_crud = NEW zcl_work_order_crud_handler( ).
    ENDIF.

    DATA(lv_validation) = lo_crud->delete_work_order( iv_work_order_id = iv_work_order_id ).
    IF lv_validation IS INITIAL.
      rv_success = TEXT-003.
      RETURN.
    ELSE.
      rv_success = lv_validation.
      RETURN.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
