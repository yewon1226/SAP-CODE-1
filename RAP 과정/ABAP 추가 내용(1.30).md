## < FIELD-SYMBOLS >
</br>

### 1) FIELD-SYMBOLS & ASSIGN
- 변수 값을 복사하지 않고, 메모리를 직접 가리켜서 공유·수정하는 방식 (`< >` 으로 표시)
- 한쪽 변경 시 원본도 즉시 변경
- `FIELD-SYMBOL` → 변수 값을 복사 ❌, 메모리 주소를 직접 참조 ⭕
- `ASSIGN` → 특정 변수/테이블 라인을 필드심볼에 연결
```abap
FIELD-SYMBOLS <fs>.                      " TYPE ANY와 동일 → 어떤 타입이든 참조 가능 (제네릭)
FIELD-SYMBOLS:
  <fs_all>    TYPE ANY,                  " 모든 타입 참조 가능 (구조/단순/테이블 라인 등)
  <fs_data>   TYPE SIMPLE,               " 문자·숫자 등 단순 타입만 참조 가능 (WRITE/계산용)
  <fs_flight> TYPE SFLIGHT,              " SFLIGHT 구조 전용 → 필드 접근 가능 (<fs_flight>-carrid)
  <fs_tab>    TYPE ANY TABLE.            " 내부 테이블 전용 참조 → 테이블 자체를 가리킴
```
```abap
*************** LOOP ASSIGNING 방식 ****************
CLASS-METHODS <write_any_table>
  IMPORTING
    <ig_info> TYPE SIMPLE
    <it_data> TYPE ANY TABLE.

METHOD <write_any_table>.
  FIELD-SYMBOLS <ls_line> TYPE ANY.

  LOOP AT <it_data> ASSIGNING <ls_line>.
    ...
  ENDLOOP.

  WRITE / <ig_info>.
ENDMETHOD.


*************** DIRECT ASSIGN 방식 ****************
DATA: <gt_scarr>      TYPE TABLE OF <scarr>,
      <gt_sbook>      TYPE TABLE OF <sbook>,
      <gv_table_name> TYPE string.

FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.

CASE <gv_table_name>.
  WHEN 'SCARR'.
    ASSIGN <gt_scarr> TO <fs_tab>.
  WHEN 'SBOOK'.
    ASSIGN <gt_sbook> TO <fs_tab>.
ENDCASE.

IF <fs_tab> IS ASSIGNED.
  SELECT * FROM (<gv_table_name>)
    UP TO 100 ROWS
    INTO TABLE <fs_tab>.
ENDIF.
```
```abap
REPORT ZPROGB03_0038.

*************** DIRECT ASSIGN 방식 ****************
* Field Symbols 선언문
FIELD-SYMBOLS <fs>. " TYPE ANY와 동일함
FIELD-SYMBOLS: <fs_all> TYPE ANY,
               <fs_data> TYPE SIMPLE,
               <fs_flight> TYPE SFLIGHT,
               <fs_tab> TYPE ANY TABLE.

* Assign 특정 변수와 연결하기
* Assign 하면 변수의 메모리 주소를 바라보게 됨

DATA: lv_text(10) TYPE C VALUE 'Hello'.

ASSIGN lv_text TO <fs>.
" IS ASSIGNED 구문 말고도, sy-subrc 로 확인 가능
" ASSIGN 구문 성공 시 0, 실패 시 4 가 들어옴
IF <fs> IS ASSIGNED.
  <fs> = 'Symbols~~'. " 직접 변경 가능
  WRITE: <fs>.
ENDIF.

*************** LOOP ASSIGNING 방식 ****************
*FIELD-SYMBOLS <gs_flight> TYPE sflight.
DATA: gt_flight TYPE TABLE OF sflight.

SELECT MANDT CARRID CONNID FLDATE
  FROM SFLIGHT
  INTO TABLE gt_flight
  WHERE CARRID EQ 'AA'
    AND CONNID EQ '17'.

*LOOP AT gt_flight ASSIGNING <gs_flight>.
LOOP AT gt_flight ASSIGNING FIELD-SYMBOL(<gs_flight>).
  <gs_flight>-connid = '20'.
  WRITE:/ <gs_flight>-carrid,
          <gs_flight>-connid,
          <gs_flight>-fldate.
ENDLOOP.
cl_demo_output=>display( gt_flight ).
```
</br>
</br>

### 2) FIELD-SYMBOL CASTING (타입 캐스팅)
- `CASTING` → 값을 바꾸지 않고, 같은 메모리를 다른 타입 구조로 해석하는 것
- 값 변환 ❌ / 해석 방식만 변경
- FIELD-SYMBOL에 연결할 때 타입을 암시적/명시적으로 지정 가능
```abap
*************** 타입 정의 ****************
TYPES: BEGIN OF <gty_s_date>,
         year  TYPE n LENGTH 4,
         month TYPE n LENGTH 2,
         day   TYPE n LENGTH 2,
       END OF <gty_s_date>.

*************** 암시적 CASTING ****************
FIELD-SYMBOLS <fs_date> TYPE <gty_s_date>.

ASSIGN sy-datum TO <fs_date> CASTING.
WRITE: / <fs_date>-year,
       / <fs_date>-month,
       / <fs_date>-day.

*************** 명시적 CASTING ****************
FIELD-SYMBOLS <fs_any> TYPE ANY.

ASSIGN sy-datum TO <fs_any> CASTING TYPE <gty_s_date>.
WRITE: / <fs_any>-year,
       / <fs_any>-month,
       / <fs_any>-day.
```
```abap
*************** CASTING (날짜 → 구조 해석) ****************
TYPES: BEGIN OF ty_date,
        year TYPE N LENGTH 4,
        month TYPE N LENGTH 2,
        dat TYPE N LENGTH 2,
       END OF ty_date.
FIELD-SYMBOLS <fs> TYPE ty_date.
DATA: gv_date TYPE D.
gv_date = sy-datum.

ASSIGN gv_date TO <fs> CASTING.
<fs>-year = 2023.

WRITE:/ '시스템 날짜: ', sy-datum,
      / 'gs_date: ', gv_date.

*************** CASTING (문자열 → 구조 해석) ****************
TYPES: BEGIN OF ty_user,
        id(5) TYPE C,
        name(10) TYPE C,
       END OF ty_user.
DATA: gv_data(20) TYPE C VALUE 'C0001HONGGILD'.
FIELD-SYMBOLS <fs_user> TYPE ANY.

ASSIGN gv_data TO <fs_user> CASTING TYPE ty_user.
WRITE: gv_data.
```
</br>
</br>

### 3) Dynamic ASSIGN을 이용한 데이터·구조·속성 접근 방식
- `ASSIGN (변수)` 는 문자열로 된 이름을 실제 데이터 객체로 해석해 필드심볼`(<fs>)` 에 연결
- Structure 필드는 전체 이름 / 필드명 / 위치 번호로 접근할 수 있음
```abap
*************** Any Data Object (일반 변수) ****************
gv_name = 'GV_CARRID'.        " 변수명 문자열 (대문자)
ASSIGN (gv_name) TO <fs>.     " gv_carrid 변수에 동적 연결

*************** Structure Component (전체 이름 사용) ****************
gv_name = 'GS_SPFLI-CARRID'.  " 구조명-필드명
ASSIGN (gv_name) TO <fs>.     " 구조 필드에 동적 접근

*************** Static Attribute (클래스 정적 속성) ****************
gv_name = 'LCL_VEHICLE=>N_O_AIRPLANES'.
ASSIGN (gv_name) TO <fs>.

*************** Instance Attribute (객체 인스턴스 속성) ****************
gv_name = 'LO_VEHICLE->N_O_AIRPLANES'.
ASSIGN (gv_name) TO <fs>.

*************** Structure Component (전체 이름 사용) ****************
gv_name = 'GS_SPFLI-CARRID'.  " 구조명-필드명
ASSIGN (gv_name) TO <fs>.     " 구조 필드에 동적 접근

*************** Structure Component (필드명만 사용) ****************
gv_comp_name = 'CARRID'.
ASSIGN COMPONENT gv_comp_name
  OF STRUCTURE gs_spfli TO <fs>.

*************** Structure Component (위치 번호 사용) ****************
gv_comp_number = 2.
ASSIGN COMPONENT gv_comp_number
  OF STRUCTURE gs_spfli TO <fs>.
```
</br>
</br>

### 4) New Open SQL 인라인 선언 형식 (@DATA 사용)
- `DATA(변수)` → SELECT 문 안에서 변수 즉시 선언(인라인 선언)
- `@` → DB 컬럼이 아니라, ABAP 변수라는 걸 알려주기 위해 붙임
- `UNASSIGN` → 필드심볼이 가리키던 메모리 참조를 해제
```abap
SELECT SINGLE *
  FROM <DB_TABLE>
  INTO @DATA(<ABAP_VARIABLE>)   " @ : ABAP 변수 표시 / DATA() : 즉시 선언 형식
  WHERE <DB_FIELD> = <VALUE>
    AND <DB_FIELD> = <VALUE>.
```
```abap
*************** New Open SQL (@DATA 인라인 선언) ****************
FIELD-SYMBOLS <fs> TYPE SIMPLE.

SELECT SINGLE *
  FROM spfli
  INTO @DATA(gs_spfli)     
  WHERE carrid = 'AA'
    AND connid = '17'.

*************** Structure Component 순차 접근 (위치 기반) ****************
DO.
  " gs_spfli 스트럭처의 각 Component 위치를 sy-index로 접근해서
  " Field symbol과 연결함
  ASSIGN COMPONENT sy-index OF STRUCTURE gs_spfli TO <fs>.
  IF sy-subrc <> 0. " 만약 ASSIGN에 성공하면 0이 들어옴
    EXIT.
  ENDIF.
  IF <fs> IS ASSIGNED.
    WRITE: <fs>.
    UNASSIGN <fs>. " <fs> 연결 해제
  ENDIF.

  IF <fs> IS NOT ASSIGNED. " UNASSIGN 하고 나서는 OK
  ENDIF.
ENDDO.
```
