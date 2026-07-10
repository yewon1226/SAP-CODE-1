## 07/09 - RAP

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
</br>
</br>

<img width="642" height="341" alt="image" src="https://github.com/user-attachments/assets/64d263d6-a583-4bbf-9ce0-fbd4ff70b78a" />
</br>
</br>

<img width="747" height="281" alt="image" src="https://github.com/user-attachments/assets/ade5b655-de3c-4fe3-a949-d0fd3983b923" />
</br>
</br>

- 삭제 전에 디버그가 열림
```abap
sap.ui.define(['sap/ui/core/mvc/ControllerExtension'], function (ControllerExtension) {
	'use strict';

	return ControllerExtension.extend('zcl2feapp03.ext.controller.ListReportExt', {
		override: {
			onInit: function () {
				var oModel = this.base.getExtensionAPI().getModel();
			},
			editFlow: {
                onBeforeDelete: function() {
                    debugger;
                }
            }
		}
	});
});
```
</br>
</br>

## 07/10 - FI

### < 계정 종류 >

| 구분 | 의미 |
|------|------|
| **AP (Accounts Payable)** | 매입채무 (거래처에 지급해야 할 금액) |
| **AR (Accounts Receivable)** | 매출채권 (거래처로부터 받을 금액) |
| **GL (General Ledger)** | 총계정원장 계정 |

</br>

### < 전표 구성 >
- **차변 (Debit)** : `+`
- **대변 (Credit)** : `-`

> 하나의 전표는 차변(Debit)과 대변(Credit)으로 구성된다.

</br>

### < 전표 작성 원칙 >

1. 계정 수는 같을 필요가 없다.

- 차변 1개 / 대변 3개 가능
- 차변 2개 / 대변 1개 가능

</br>

2. 금액의 합은 반드시 일치해야 한다.

```text
차변 합계 = 대변 합계
```

</br>

- 예시 1

| 차변(Debit) | 금액 | 대변(Credit) | 금액 |
|--------------|------:|--------------|------:|
| 비품(GL) | 100,000 | 현금(GL) | 100,000 |

- 예시 2

| 차변(Debit) | 금액 | 대변(Credit) | 금액 |
|--------------|------:|--------------|------:|
| 매출채권(AR) | 110,000 | 매출(GL) | 100,000 |
| | | 부가세예수금(GL) | 10,000 |

```text
차변 합계 : 110,000
대변 합계 : 110,000
```


<img width="755" height="402" alt="image" src="https://github.com/user-attachments/assets/98bf402a-7d74-4a04-a2f7-653448e3ac1f" />
