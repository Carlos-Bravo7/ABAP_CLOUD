CLASS zcl_work_order_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
*     Declaration of the method that validates the creation of the work orders
      validate_create_order IMPORTING iv_customer_id   TYPE string
                                      iv_technician_id TYPE string
                                      iv_priority      TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

*     Declaration of the method that validates the update of the work orders
      validate_update_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

*     Declaration of the method that validates the delete of the work orders
      validate_delete_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

*     Declaration of the method that validates the statuses and priorities of the work orders
      validate_status_and_priority IMPORTING iv_status       TYPE string
                                             iv_priority     TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_valid_status   TYPE string VALUE 'PE CO', " Example statuses: Pending, Completed
               c_valid_priority TYPE string VALUE 'A B'. " Example priorities: High, Low

    METHODS:
      check_customer_exists IMPORTING iv_customer_id   TYPE string
                            RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_technician_exists IMPORTING iv_technician_id TYPE string
                              RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_exists IMPORTING iv_work_order_id TYPE string
                         RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_history IMPORTING iv_work_order_id TYPE string
                          RETURNING VALUE(rv_exists) TYPE abap_bool.
ENDCLASS.



CLASS zcl_work_order_validator IMPLEMENTATION.

  METHOD validate_create_order.
    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_long) = strlen( iv_priority ).

    " Check if priority is 1 character long
    IF lv_long NE 1.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority is valid
    IF iv_priority NS c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.
    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status NS c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_delete_order.
    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.

************ Validation of status ************

    "Validate if status is 2 characters long
    DATA(lv_long) = strlen( iv_status ).

    IF lv_long NE 2.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the status value
    IF iv_status NS c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

************ Validation of priority ************

    "Validate if priority is 1 character long
    CLEAR lv_long.
    lv_long = strlen( iv_priority ).

    IF lv_long NE 1.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    IF iv_priority NS c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD check_customer_exists.
*  Search the table with the given id
    SELECT SINGLE FROM ztcustomer
            FIELDS customer_id
            WHERE customer_id = @iv_customer_id
            INTO @DATA(lv_id).

*   Validation to see if the id exist
    IF sy-subrc = 0.
      "If the id exist return true
      rv_exists = abap_true.
      RETURN.
    ELSE.
      "If the id does not exist return false
      rv_exists = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_technician_exists.
*  Search the table with the given id
    SELECT SINGLE FROM zttechnician
            FIELDS technician_id
            WHERE technician_id = @iv_technician_id
            INTO @DATA(lv_id).

*   Validation to see if the id exist
    IF sy-subrc = 0.
      "If the id exist return true
      rv_exists = abap_true.
      RETURN.
    ELSE.
      "If the id does not exist return false
      rv_exists = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_order_exists.
*  Search the table with the given id
    SELECT SINGLE FROM ztwork_order
            FIELDS work_order_id
            WHERE work_order_id = @iv_work_order_id
            INTO @DATA(lv_id).

*   Validation to see if the id exist
    IF sy-subrc = 0.
      "If the id exist return true
      rv_exists = abap_true.
      RETURN.
    ELSE.
      "If the id does not exist return false
      rv_exists = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_order_history.
*  Search the table with the given id
    SELECT SINGLE FROM ztworkorder_hist
            FIELDS work_order_id
            WHERE work_order_id = @iv_work_order_id
            INTO @DATA(lv_id).

*   Validation to see if the id exist
    IF sy-subrc = 0.
      "If the id exist return true
      rv_exists = abap_true.
      RETURN.
    ELSE.
      "If the id does not exist return false
      rv_exists = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
