# SAPUI5 바인딩·이벤트 핵심정리
### < oEvent 객체 정보 2가지 >
1) `oEvent.getSource()`
   - 이벤트를 일으킨 놈 (컨트롤 자체)
   - 누가 이벤트를 발생시켰는지
</br>

2) `oEvent.getParameter("...")`
   - 이벤트가 들고 온 데이터
   - 이벤트와 함께 전달된 정보

### < 컨트롤 찾는 3단계 >
1) `this.byId("idName")`
   - View에 있는 컨트롤(ID로 직접 찾기)
   - 버튼, 테이블, 입력창 등 가져올 때
</br>

2) `oEvent.getSource()`
   - 지금 이벤트를 일으킨 놈
   - “지금 누른 버튼” 같은 거
</br>

3) `oEvent.getSource().getParent()`
   - 이벤트 일으킨 놈의 상위 컨트롤
   - 버튼이 들어있는 행(Row) 가져올 때
</br>

### < 이벤트 객체 구조 >
- 모든 이벤트는 `oEvent` 하나로 들어옴
- 그 안에서 필요한 정보 꺼내는 방식만 달라짐

| 함수 | 리턴값 | 쓰임 |
|------|---------|------|
| `getSource()` | 이벤트 발생한 컨트롤 | 버튼 / 콤보박스 / 차트 등 |
| `getParameters()` | 이벤트 파라미터 전체 묶음 | 선택한 아이템 / 입력값 등 |
| `getParameter("value")` | Input, ComboBox의 입력값 | `"apple"` 같은 값 |
| `getParameter("selectedItem")` | ComboBox 선택 아이템 | ListItem 객체 |
| `getParameter("data")` | 차트 선택 이벤트(selectData) | 선택된 데이터 묶음 |
</br>

### < 바인딩이란? (쉽게 말하면 “데이터 연결선”) >
- 화면(UI)과 뒤의 데이터가 실시간으로 연결되어 움직이는 것
- 한 줄 데이터 : `text="{필드}"`
- 여러 줄 데이터 : `items="{경로}"`
- 특정 객체 하나 : `bindElement({ path: "경로" })`
</br>

### < Row 선택 시 데이터 꺼내는 공식 >
```java
onSelectionChange: function(oEvent) {
  // 선택한 Row 객체
  let oRow = oEvent.getParameters().listItem;

  // Row가 연결된 데이터의 경로 정보
  let oContext = oRow.getBindingContext(); 
  let sPath = oContext.getPath();  // "/customers/1"

  // 실제 데이터 꺼내기
  let oData = oContext.getObject(); // {name: "강호동"}
  console.log(oData.name); // "강호동"
}
```
