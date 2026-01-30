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
REPORT ZPROGB03_0038.

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
REPORT ZPROGB03_0038.

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
</br>
</br>

### 5) 동적 Field Catalog 설정 (FIELD-SYMBOL 활용)
- 필드 이름을 문자로 넘겨서, ALV 컬럼 속성을 하나씩 설정하는 방식
- S로 시작하고 E로 끝내며, FIELD-SYMBOL로 바로 값을 넣어 컬럼을 완성
```abap
*************** 동적 Field Catalog 설정 ****************
FORM <set_fieldcat> USING <pv_type>  TYPE c
                           <pv_fname> TYPE any
                           <pv_value> TYPE any.
  FIELD-SYMBOLS <fs> TYPE any.

  IF <pv_type> = 'S'.          " Start
    CLEAR <gs_fcat>.
  ENDIF.

  ASSIGN <gs_fcat>-(<pv_fname>) TO <fs>.   " 필드명 문자열로 동적 접근
  <fs> = <pv_value>.                       " 값 설정

  IF <pv_type> = 'E'.          " End
    APPEND <gs_fcat> TO <gt_fcat>.
  ENDIF.
ENDFORM.


*************** Field Catalog 설정 호출 ****************
PERFORM <set_fieldcat> USING:
  'S'  <FIELDNAME>  <VALUE>,
  ' '  <FIELDNAME>  <VALUE>,
  'E'  <FIELDNAME>  <VALUE>.
```
```abap
REPORT ZPROGB03_0029.

*************** 동적 Field Catalog 설정 ****************
FORM set_fieldcat USING pv_type TYPE C
                        pv_fname TYPE ANY
                        pv_value TYPE ANY.
  FIELD-SYMBOLS <fs> TYPE ANY.
  
  IF pv_type = 'S'. "Start
    CLEAR gs_fcat.
  ENDIF.
  
  ASSIGN gs_fcat-(pv_fname) TO <fs>.
  <fs> = pv_value.
  
  IF pv_type = 'E'. "End
    APPEND gs_fcat TO gt_fcat.
  ENDIF.
ENDFORM.

*************** Field Catalog 설정 호출 ****************
PERFORM set_fieldcat USING:
    'S'  'FIELDNAME'  'CANCELLED_ICON',
    ' '  'ICON'       'X',
    'E'  'COLTEXT'    'Cancelled',
    'S'  'FIELDNAME'  'CANCELLED',
    'E'  'NO_OUT'     'X',
    'S'  'FIELDNAME'  'PASSFORM',
    'E'  'NO_OUT'     'X',
    'S'  'FIELDNAME'  'PASSBIRTH',
    'E'  'NO_OUT'     'X',
    'S'  'FIELDNAME'  'PHONE',
    ' '  'REF_FIELD'  'TELEPHONE',
    ' '  'REF_TABLE'  'SCUSTOM',
    ' '  'COL_POS'    4,
    'E'  'HOTSPOT'    'X',
    'S'  'FIELDNAME'  'SMOKER',
    'E'  'CHECKBOX'   'X',
    'S'  'FIELDNAME'  'INVOICE',
    'E'  'CHECKBOX'   'X',
    'S'  'FIELDNAME'  'CLASS',
    'E'  'EMPHASIZE'  'C510',
    'S'  'FIELDNAME'  'LOCCURAM',
    'E'  'COL_POS'    6,
    'S'  'FIELDNAME'  'DISPLAY_BOOKINGS',
    ' '  'COLTEXT'    'Display Detail',
    'E'  'COL_POS'    3
    .
```
</br>
</br>

---

</br>

## < 엑셀 업로드 표준 형식 (row/col 기반 내부테이블 구성) >
- 엑셀 파일을 선택해 row/col 구조의 임시 테이블로 읽어오는 형식
- col 값으로 필드에 매핑하고, `AT END OF row` 에서 한 행을 완성해 결과 테이블에 추가하는 패턴
</br>

- `cl_gui_frontend_services=>file_open_dialog` : PC에서 파일 선택 창을 띄워서 사용자가 고른 파일 경로를 가져오는 메서드
- `CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'` : 엑셀 파일을 읽어서 셀 단위(row/col) 내부테이블로 변환해주는 표준 함수
```abap
*************** 파일 선택 파라미터 ****************
PARAMETERS <파일경로> TYPE localfile.

*************** 파일 선택 다이얼로그 ****************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR <파일경로>.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter = <파일필터>
    CHANGING
      file_table  = <선택파일목록>
      rc          = <리턴코드>
  ).
  READ TABLE <선택파일목록> INTO <파일경로> INDEX 1.

*************** 엑셀 데이터 임시 테이블 ****************
DATA <엑셀원본테이블> TYPE TABLE OF alsmex_tabline.

*************** 엑셀 → 내부테이블 변환 ****************
CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    filename    = <파일경로>
    i_begin_col = <시작컬럼>
    i_begin_row = <시작행>
    i_end_col   = <종료컬럼>
    i_end_row   = <종료행>
  TABLES
    intern      = <엑셀원본테이블>.

*************** 엑셀 데이터 가공 (row/col 핵심 로직) ****************
SORT <엑셀원본테이블> BY row col.

LOOP AT <엑셀원본테이블> INTO <엑셀한셀>.
  CASE <엑셀한셀>-col.
    WHEN <컬럼번호>.
      <결과한행>-<필드명> = <엑셀한셀>-value.
  ENDCASE.

  AT END OF row.        " 같은 행(row)의 마지막 컬럼일 때
    APPEND <결과한행> TO <결과테이블>.
    CLEAR <결과한행>.
  ENDAT.
ENDLOOP.
```
```abap
REPORT ZPROGB03_0039.

*************** 파라미터 및 데이터 선언 ****************
PARAMETERS pa_file TYPE localfile.           " 엑셀업로드를 위한 파일업로드 타입의 파라미터 선언

TYPES BEGIN OF ts_data.
  TYPES: a_field(20),
         b_field(20),
         c_field(20).
TYPES END OF ts_data.

DATA: gt_filename   TYPE filetable,          " 선택한 파일 목록
      gv_rc         TYPE i,                  " 파일 선택 결과 코드
      gt_excel_data TYPE TABLE OF alsmex_tabline. " 엑셀 원본(row/col) 데이터
DATA: gt_itab TYPE TABLE OF ts_data,          " 결과 내부테이블
      gs_itab TYPE ts_data.                   " 결과 한 행
FIELD-SYMBOLS: <fs>.                          " 동적 필드 매핑용
CONSTANTS gc_filters TYPE string VALUE
  'EXCEL FILES (*.XLSX)|*.XLSX|EXCEL FILES (*.XLS)|*.XLS|'.

*************** 파일 선택 다이얼로그 ****************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.    " pa_file 입력칸에서 F4(값 도움) 눌렀을 때 실행됨
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title     = '파일찾기'    " 파일 선택 창 상단에 표시될 제목
      default_filename = space         " 기본으로 입력되어 있을 파일명 (없으면 공백)
      file_filter      = gc_filters    " 선택 가능한 파일 확장자 제한 (엑셀만 보이게)
      initial_directory= 'C:'          " 파일 선택 창이 처음 열릴 경로
    CHANGING
      file_table       = gt_filename   " 사용자가 선택한 파일 목록이 담기는 내부테이블
      rc               = gv_rc         " 선택 결과
                                        "  > 0 : 선택한 파일 개수
                                        "  -1  : 오류 발생
  ).
  IF sy-subrc = 0.
    READ TABLE gt_filename INTO pa_file INDEX 1. " 선택 파일 중 첫 번째 사용
  ENDIF.

*************** 엑셀 업로드 및 데이터 가공 ****************
START-OF-SELECTION.
  IF pa_file IS NOT INITIAL.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename    = pa_file   " 사용자가 선택한 엑셀 파일 전체 경로
        i_begin_col = 1         " 엑셀에서 읽기 시작할 컬럼 번호
        i_begin_row = 2         " 엑셀에서 읽기 시작할 행 번호 (보통 헤더 제외)
        i_end_col   = 3         " 엑셀에서 읽을 마지막 컬럼 번호
        i_end_row   = 100       " 엑셀에서 읽을 마지막 행 번호 (2~100행)
      TABLES
        intern      = gt_excel_data. " 엑셀을 row/col 단위로 담는 임시 내부테이블

    IF sy-subrc = 0.
      SORT gt_excel_data BY row col.
      LOOP AT gt_excel_data INTO DATA(gs_excel_data).
*        CASE gs_excel_data-col.
*          WHEN '1'.
*            gs_itab-a_field = gs_excel_data-value.
*          WHEN '2'.
*            gs_itab-b_field = gs_excel_data-value.
*          WHEN '3'.
*            gs_itab-c_field = gs_excel_data-value.
*          WHEN OTHERS.
*        ENDCASE.

" 필드 심볼 이용
        UNASSIGN <fs>. "CLEAR와 비슷하게 필드심볼 재사용 시 연결 해제
        ASSIGN COMPONENT gs_excel_data-col OF STRUCTURE gs_itab TO <fs>.
        <fs> = gs_excel_data-value.

        AT END OF row. " row 값이 변경되기 전 마지막에 반영
          APPEND gs_itab TO gt_itab.
          CLEAR gs_itab.
        ENDAT.
      ENDLOOP.
      cl_demo_output=>display( gt_itab ).
    ENDIF.
  ENDIF.
```
</br>
</br>

---

</br>

## < Simple Tree 생성 및 노드 구성 >
- `CL_GUI_SIMPLE_TREE` : 화면에 폴더처럼 생긴 메뉴 트리를 만들고, 클릭한 항목으로 동작을 나누는 UI 컨트롤
- `CLASS … DEFINITION DEFERRED` : 클래스 구현이 뒤에 나오니까, 지금은 “이런 클래스가 있다”만 미리 알려주는 선언
- `CALL METHOD go_tree->add_nodes` : 만들어둔 노드 테이블(MTREESNODE)을 Tree 화면에 실제로 그려주는 메서드
- `go_tree->set_registered_events` : Tree가 어떤 이벤트(예: 더블클릭)를 사용할지 미리 등록해주는 설정 메서드
```abap
*************** Tree 관련 데이터 선언 ****************
DATA: <go_tree>       TYPE REF TO cl_gui_simple_tree,   " Tree 컨트롤 객체
      <go_tree_event> TYPE REF TO <lcl_tree_event>,     " Tree 이벤트 객체
      <gs_node>       TYPE mtreesnode,                  " 노드 1건
      <gt_node>       TYPE TABLE OF mtreesnode,         " 노드 전체 테이블
      <lt_events>     TYPE cntl_simple_events,          " 등록 이벤트 목록
      <ls_event>      TYPE cntl_simple_event.           " 이벤트 1건

*************** Tree 생성 ****************
CREATE OBJECT <go_tree>
  EXPORTING
    parent              = <부모컨테이너>
    node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.

*************** Tree 노드 구성 ****************
CLEAR <gs_node>.
<gs_node>-node_key  = <노드키>.        " 이 노드를 구분하는 고유 ID (겹치면 안 됨)
<gs_node>-relatkey  = <부모노드키>.    " 이 노드가 속한 부모 노드의 KEY
<gs_node>-isfolder  = 'X'.             " 폴더 노드 여부 (X: 펼칠 수 있음 / 공백: 말단 노드)
<gs_node>-text      = <노드텍스트>.    " 트리 화면에 실제로 보여줄 글자
<gs_node>-n_image   = <기본아이콘>.    " 노드가 접혀 있을 때 아이콘
<gs_node>-exp_image = <확장아이콘>.    " 노드를 펼쳤을 때 아이콘
APPEND <gs_node> TO <gt_node>.

*************** Tree 노드 반영 ****************
* 노드 테이블을 Tree에 추가
CALL METHOD <go_tree>->add_nodes
  EXPORTING
    table_structure_name = 'MTREESNODE'
    node_table           = <gt_node>.

*************** Tree 이벤트 핸들러 클래스 ****************
* 노드 더블클릭 이벤트 처리
CLASS <lcl_tree_event> DEFINITION.
  PUBLIC SECTION.
    METHODS <on_node_double_click>
      FOR EVENT node_double_click OF cl_gui_simple_tree
      IMPORTING node_key.  " 클릭된 노드 키
ENDCLASS.

CLASS <lcl_tree_event> IMPLEMENTATION.
  METHOD <on_node_double_click>.
    " node_key 기준 처리 로직
  ENDMETHOD.
ENDCLASS.

*************** Tree 이벤트 등록 ****************
CREATE OBJECT <go_tree_event>.
SET HANDLER <go_tree_event>-><on_node_double_click> FOR <go_tree>.

<ls_event>-eventid    = cl_gui_simple_tree=>eventid_node_double_click. " 더블클릭 이벤트
<ls_event>-appl_event = 'X'.  " 화면 PAI 먼저 실행
APPEND <ls_event> TO <lt_events>.

CALL METHOD <go_tree>->set_registered_events
  EXPORTING
    events = <lt_events>.
```
```abap
REPORT zprogb03_0040.

*************** Tree 관련 데이터 선언 ****************
DATA: go_tree       TYPE REF TO cl_gui_simple_tree,   " 트리 컨트롤 객체
      gs_node       TYPE mtreesnode,                  " 트리 노드 1건
      gt_node       TYPE TABLE OF mtreesnode.         " 트리 노드 전체 목록
CLASS: lcl_node_event_handler DEFINITION DEFERRED.
DATA: go_node_event TYPE REF TO lcl_node_event_handler. " 트리 이벤트 핸들러 객체


*************** Tree 이벤트 핸들러 클래스 ****************
CLASS lcl_node_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      double_click
        FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.   " 더블클릭한 노드 KEY
ENDCLASS.

CLASS lcl_node_event_handler IMPLEMENTATION.
  METHOD double_click.
    MESSAGE node_key TYPE 'E' DISPLAY LIKE 'S'.  " 클릭된 노드 KEY 표시
  ENDMETHOD.
ENDCLASS.


*************** Tree 생성 MODULE ****************
MODULE init_tree OUTPUT.
  IF go_tree IS INITIAL.
    CREATE OBJECT go_tree
      EXPORTING
        parent              = go_cont_r
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.
    IF sy-subrc = 0.
      PERFORM add_node.
      PERFORM set_node_events.
    ENDIF.
  ENDIF.
ENDMODULE.


*************** Tree 노드 구성 FORM ****************
FORM add_node .
  CLEAR gs_node.
  gs_node-node_key  = 'ROOT'.        " 루트 노드
  gs_node-relatkey  = ' '.           " 최상위
  gs_node-isfolder  = 'X'.           " 폴더 노드
  gs_node-text      = 'TREE Header'. " 헤더 텍스트
  gs_node-n_image   = '@5C@'.        " 접힘 아이콘
  gs_node-exp_image = '@5B@'.        " 펼침 아이콘
  APPEND gs_node TO gt_node.

  CLEAR gs_node.
  gs_node-node_key  = 'LV11'.
  gs_node-relatkey  = 'ROOT'.        " ROOT의 자식
  gs_node-isfolder  = 'X'.
  gs_node-text      = '자식노드'.
  gs_node-n_image   = '@5C@'.
  gs_node-exp_image = '@5B@'.
  APPEND gs_node TO gt_node.

  CLEAR gs_node.
  gs_node-node_key  = 'LV21'.
  gs_node-relatkey  = 'ROOT'.        " ROOT의 형제 레벨
  gs_node-isfolder  = 'X'.
  gs_node-text      = '형제노드'.
  gs_node-n_image   = '@5C@'.
  gs_node-exp_image = '@5B@'.
  APPEND gs_node TO gt_node.

  CLEAR gs_node.
  gs_node-node_key  = 'LV12'.
  gs_node-relatkey  = 'LV11'.        " LV11의 자식
  gs_node-isfolder  = 'X'.
  gs_node-text      = '자식의 자식 노드'.
  gs_node-n_image   = '@BU@'.
  gs_node-exp_image = '@GZ@'.
  APPEND gs_node TO gt_node.

  CLEAR gs_node.
  gs_node-node_key  = 'LV13'.
  gs_node-relatkey  = 'LV12'.        " LV12의 자식
  gs_node-isfolder  = 'X'.
  gs_node-text      = '자식의 자식의 자식 노드'.
  gs_node-n_image   = '@BU@'.
  gs_node-exp_image = '@GZ@'.
  APPEND gs_node TO gt_node.

  CLEAR gs_node.
  gs_node-node_key  = 'LV22'.
  gs_node-relatkey  = 'LV21'.        " LV21의 자식
  gs_node-isfolder  = 'X'.
  gs_node-text      = '형제의 자식 노드'.
  gs_node-n_image   = '@BU@'.
  gs_node-exp_image = '@GZ@'.
  APPEND gs_node TO gt_node.

  CALL METHOD go_tree->add_nodes
    EXPORTING
      table_structure_name = 'MTREESNODE'
      node_table           = gt_node.
ENDFORM.

*************** Tree 이벤트 등록 FORM ****************
FORM set_node_events .
  DATA: lt_events TYPE cntl_simple_events,
        ls_event  TYPE cntl_simple_event.

  CREATE OBJECT go_node_event.
  SET HANDLER go_node_event->double_click FOR go_tree.

  " tree 객체에도 실행할 이벤트가 어떤건지 알려줘야 함
  ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  ls_event-appl_event = 'X'. " Tree가 속한 화면(100번)의 PAI를 먼저 실행
  APPEND ls_event TO lt_events.

  CALL METHOD go_tree->set_registered_events
    EXPORTING
      events = lt_events.
ENDFORM.
```
