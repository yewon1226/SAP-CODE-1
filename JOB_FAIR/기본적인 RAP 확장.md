- `SE09` -> CTS 생성
- `ADT` -> Package 생성
- 실행 단축키 : class는 `F9`, 데이터 확인용은 `F8`

<img width="520" height="324" alt="image" src="https://github.com/user-attachments/assets/035c9840-b596-4360-bb81-41563cc25d04" />

</br>
</br>

- DB Table -> Class로 데이터 채우기 -> Rap 모델 -> Meatadata Extension 파일 -> UI5 App -> 확장
- `F8` : 실행 (데이터확인용)

<img width="517" height="305" alt="image" src="https://github.com/user-attachments/assets/61ef311c-56d1-4070-bd03-5b9e0ffcac7f" />
</br>
</br>

- class 생성 후 `F9` 로 실행
```abap
CLASS zclcl2fe0301 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zclcl2fe0301 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA lt_flight TYPE TABLE OF ztcl2fe0301.
    GET TIME STAMP FIELD DATA(lv_ts).
    DELETE FROM ztcl2fe0301.

    SELECT FROM sflight
      FIELDS
        mandt AS client,
        carrid AS carrier_id,
        connid AS connection_id,
        fldate AS flight_date,
        price,
        currency AS currency_code,
        planetype AS plane_type_id,
        seatsmax AS seats_max,
        seatsocc AS seats_occupied
      INTO TABLE @lt_flight
      UP TO 200 ROWS.

    LOOP AT lt_flight ASSIGNING FIELD-SYMBOL(<ls_flight>).
      <ls_flight>-created_by = sy-uname.
      <ls_flight>-created_at = lv_ts.
      <ls_flight>-last_changed_by = sy-uname.
      <ls_flight>-last_changed_at = lv_ts.
    ENDLOOP.

    INSERT ztcl2fe0301 FROM TABLE @lt_flight.

    out->write( |{ sy-dbcnt }건 생성 완료!| ).
  ENDMETHOD.
ENDCLASS.
```

</br>

<img width="567" height="193" alt="image" src="https://github.com/user-attachments/assets/c4f894b7-7f01-4f05-8403-81033e78c79b" />
</br>
</br>

<img width="488" height="563" alt="image" src="https://github.com/user-attachments/assets/d0445a75-9eee-41f4-a53c-d87f86cd3279" />
