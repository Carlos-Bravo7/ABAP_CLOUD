CLASS zcl_work_order_crud_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
*     Declaration of the method that create the work orders
      create_work_order IMPORTING iv_customer_id    TYPE string
                                  iv_technician_id  TYPE string
                                  iv_priority       TYPE string
                                  iv_description    TYPE string
                        RETURNING VALUE(rv_success) TYPE string,

*     Declaration of the method that reads the work orders
      read_work_order IMPORTING iv_work_order_id TYPE string
                      RETURNING VALUE(rv_result) TYPE ztwork_order,

*     Declaration of the method that update the work orders
      update_work_order IMPORTING iv_work_order_id  TYPE string
                                  iv_status         TYPE string
                                  iv_customer       TYPE string
                                  iv_priority       TYPE string
                                  iv_technician     TYPE string
                                  iv_description    TYPE string
                        RETURNING VALUE(rv_success) TYPE string,

*     Declaration of the method that delete the work orders
      delete_work_order IMPORTING iv_work_order_id  TYPE string
                        RETURNING VALUE(rv_success) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: lo_validatior TYPE REF TO zcl_work_order_validator.

ENDCLASS.



CLASS zcl_work_order_crud_handler IMPLEMENTATION.

  METHOD create_work_order.


    "Declaration of the structure that contains the data that will be inserted in the table
    DATA ls_register TYPE ztwork_order.

    "Get the number of registers to assign the Work Order ID by adding 1
    SELECT COUNT(*) FROM ztwork_order INTO @DATA(lv_count).

    "Declaration of an instance of the class zcl_work_order_validator
    IF lo_validatior IS NOT BOUND.
      lo_validatior = NEW zcl_work_order_validator( ).
    ENDIF.

    DATA(lv_validation) = lo_validatior->validate_create_order( iv_customer_id = iv_customer_id
                                                             iv_technician_id = iv_technician_id
                                                             iv_priority = iv_priority ).

    "Check of the validation of the inputed parameters
    IF lv_validation IS INITIAL.

      "If the parameters are correct they are inserted in the structure ls_register
      ls_register = VALUE #( work_order_id = lv_count + 1
                             customer_id = iv_customer_id
                             technician_id = iv_technician_id
                             creation_date = cl_abap_context_info=>get_system_date( )
                             status = 'PE'
                             priority = iv_priority
                             description = iv_description ).

      "Try of the insertion of the data into the table
      TRY.
          INSERT ztwork_order FROM @ls_register.
        CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).

      ENDTRY.

      "Return of true meaning that everything went smoothly
      CLEAR rv_success.
      RETURN.
    ELSE.
      rv_success = lv_validation.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.
*  Search the table with the given id
    SELECT SINGLE FROM ztwork_order
            FIELDS *
            WHERE work_order_id = @iv_work_order_id
            INTO @DATA(ls_wo).

    rv_result = ls_wo.
    RETURN.

  ENDMETHOD.

  METHOD update_work_order.

    "Declaration of the structure for the register that will be inserted in ZTWORKORDER_HIST
    DATA ls_hist TYPE ztworkorder_hist.
    DATA lv_change_des TYPE string VALUE 'Updated'.

    "Get the number of registers to assign the Work Order ID by adding 1
    SELECT COUNT(*) FROM ztworkorder_hist INTO @DATA(lv_count).

    "Declaration of an instance of the class zcl_work_order_validator
    IF lo_validatior IS NOT BOUND.
      lo_validatior = NEW zcl_work_order_validator( ).
    ENDIF.
    DATA(lv_validation_1) = lo_validatior->validate_create_order( iv_customer_id = iv_customer
                                                             iv_technician_id = iv_technician
                                                             iv_priority = iv_priority ).

    DATA(lv_validation_2) = lo_validatior->validate_update_order( iv_work_order_id = iv_work_order_id
                                                                  iv_status = iv_status ).

    "If neither of the validations pass, the method return false
    IF lv_validation_1 IS INITIAL AND lv_validation_2 IS INITIAL.


      "Search the work order we want to update
      SELECT SINGLE FROM ztwork_order
              FIELDS *
              WHERE work_order_id = @iv_work_order_id
              INTO @DATA(ls_wo).

      IF sy-subrc = 0.
        "Modify the fields we want to update


        IF ls_wo-customer_id NE iv_customer.
          ls_wo-customer_id = iv_customer.
          CONCATENATE lv_change_des 'customer' INTO lv_change_des SEPARATED BY ','.
        ENDIF.

        IF ls_wo-technician_id NE iv_technician.
          ls_wo-technician_id = iv_technician.
          CONCATENATE lv_change_des 'technician' INTO lv_change_des SEPARATED BY ','.
        ENDIF.


        IF ls_wo-status NE iv_status.
          ls_wo-status = iv_status.
          CONCATENATE lv_change_des 'stat' INTO lv_change_des SEPARATED BY ','.
        ENDIF.


        IF ls_wo-priority NE iv_priority.
          ls_wo-priority = iv_priority.
          CONCATENATE lv_change_des 'priority' INTO lv_change_des SEPARATED BY ','.
        ENDIF.


        IF ls_wo-description NE iv_description.
          ls_wo-description = iv_description.
          CONCATENATE lv_change_des 'descrip' INTO lv_change_des SEPARATED BY ','.
        ENDIF.

        "Try of the update of the data into the table
        TRY.
            MODIFY ztwork_order FROM @ls_wo.
          CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).

        ENDTRY.


        ls_hist = VALUE #( history_id = lv_count + 1
                           work_order_id = iv_work_order_id
                           modification_date = cl_abap_context_info=>get_system_date( )
                           change_description = lv_change_des ).

        "Try of the insertion of the data into the table
        TRY.
            INSERT ztworkorder_hist FROM @ls_hist.
          CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db_hist).

        ENDTRY.

        CLEAR rv_success.
        RETURN.

      ELSE.
        rv_success = TEXT-001.
        RETURN.
      ENDIF.

    ELSE.
      rv_success = |{ lv_validation_1 }  { lv_validation_2 }|.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD delete_work_order.
    "Search the work order we want to delete
    SELECT SINGLE FROM ztwork_order
            FIELDS *
            WHERE work_order_id = @iv_work_order_id
            INTO @DATA(ls_wo).

    "Declaration of an instance of the class zcl_work_order_validator
    IF lo_validatior IS NOT BOUND.
      lo_validatior = NEW zcl_work_order_validator( ).
    ENDIF.

    DATA(lv_validation) = lo_validatior->validate_delete_order( iv_work_order_id = iv_work_order_id
                                                             iv_status = ls_wo-status ).

    "Check of the validation of the inputed parameters
    IF lv_validation IS INITIAL.

      "Deletion of the register
      TRY.
          DELETE ztwork_order FROM @ls_wo.
        CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).

      ENDTRY.

      "Return of true meaning that everything went smoothly
      CLEAR rv_success.
      RETURN.

    ELSE.
      rv_success = lv_validation.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
