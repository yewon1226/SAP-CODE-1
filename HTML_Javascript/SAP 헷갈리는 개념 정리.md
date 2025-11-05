# SAPUI5 바인딩·이벤트 핵심정리
### < oEvent 객체 정보 2가지 >
1) `oEvent.getSource()`
   - 이벤트를 일으킨 놈 (컨트롤 자체)
   - 누가 이벤트를 발생시켰는지

2) `oEvent.getParameter("...")`
   - 이벤트가 들고 온 데이터
   - 이벤트와 함께 전달된 정보
</br>

### < 컨트롤 찾는 3단계 >
1) `this.byId("idName")`
   - View에 있는 컨트롤(ID로 직접 찾기)
   - 버튼, 테이블, 입력창 등 가져올 때

2) `oEvent.getSource()`
   - 지금 이벤트를 일으킨 놈
   - “지금 누른 버튼” 같은 거

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
</br>

### < source parameter 비교 >
| 상황 | 이벤트 | 가져올 방법 | 이유 |
|------|---------|--------------|------|
| 버튼 클릭 | `press` | `getSource()` | 버튼이 이벤트 주체, 데이터는 없음 |
| 테이블 Row 선택 | `selectionChange` | `getParameters().listItem` | 선택한 행(Row)이 파라미터에 포함됨 |
| Input 입력 | `change` | `getParameter("value")` | 입력값이 이벤트 파라미터로 전달됨 |
| 차트 클릭 | `selectData` | `getParameter("data")` | 선택된 데이터 묶음이 파라미터로 전달됨 |

</br>

### < 이벤트별 `oEvent` 구조 비교표 >
| 이벤트 | 주요 파라미터 | `getSource()` 결과 | `getParameter()` 예시 | 설명 |
|--------|----------------|--------------------|-----------------------|------|
| `press` | (없음) | 버튼(`sap.m.Button`) | X | 버튼이 직접 이벤트 발생 |
| `selectionChange` | `listItem` | 테이블(`sap.m.Table`) | `oEvent.getParameters().listItem` | 선택된 Row 객체 포함 |
| `change` | `value` | 인풋(`sap.m.Input`) | `oEvent.getParameter("value")` | 입력된 텍스트 값 전달 |
| `selectData` | `data` | 차트(`sap.viz.ui5.controls.VizFrame`) | `oEvent.getParameter("data")` | 선택된 데이터 묶음 배열로 전달 |
</br>

---
</br>

### SAPUI5 실전 종합 모의시험

### 문제 1. (이벤트 객체 구조 구분)
- 다음 네 가지 이벤트에서 각각 값 또는 데이터를 가져오는 코드를 작성하시오.
- (ComboBox, Input, Table Row, VizFrame 차트 클릭)
```java
// ComboBox 선택
let oItem = oEvent.getParameter("selectedItem");
console.log(oItem.getKey());

// Input 입력
let sValue = oEvent.getParameter("value");
console.log(sValue);

// Table Row 선택
let oRow = oEvent.getParameters().listItem;
let oData = oRow.getBindingContext("customer").getObject();
console.log(oData.Name);

// 차트 클릭
let aData = oEvent.getParameter("data");
let sOrderID = aData[0].data.OrderID;
console.log(sOrderID);
```
</br>

### 문제 2. (라우팅 전체 흐름 완성)
- Main → Detail 페이지 이동 시 OrderID, ProductID 전달
- (1) pattern (2) navTo (3) attachPatternMatched (4) bindElement 순서로 작성하시오.
```json
// manifest.json
"pattern": "Detail/{param1}/{param2}"
```
```java
// Main.controller.js
onSelectData: function(oEvent) {
  let aData = oEvent.getParameter("data");
  let sOrderID = aData[0].data.OrderID;
  let sProductID = aData[0].data.ProductID;

  let oRouter = this.getOwnerComponent().getRouter();
  oRouter.navTo("RouteDetail", {
    param1: sOrderID,
    param2: sProductID
  });
}
```
```java
// Detail.controller.js
onInit() {
  let oRouter = this.getOwnerComponent().getRouter();
  oRouter.getRoute("RouteDetail").attachPatternMatched(this._onMatched, this);
},

_onMatched: function(oEvent) {
  let sOrder = oEvent.getParameters().arguments.param1;
  let sProd = oEvent.getParameters().arguments.param2;
  this.byId("ObjectPageLayout").bindElement({
    path: `/Order_Details(OrderID=${sOrder},ProductID=${sProd})`
  });
}
```
</br>

### 문제 3. (FilterBar + Chart 필터 적용)
- 검색 버튼 클릭 시 ComboBox에서 선택한 OrderID로 차트 데이터 필터링하는 코드를 작성하시오.
- 필터는 Dataset의 "data" 바인딩에만 적용 가능
- getSelectedItem() → getBindingContext() → getObject() 순서 중요
```java
onSearch: function() {
  let oCombo = this.byId("idOrderID");
  let oItem = oCombo.getSelectedItem();
  if (!oItem) return;

  let oContext = oItem.getBindingContext();
  let sOrderID = oContext.getObject().OrderID;

  let oDataset = this.byId("idFlattenedDataset");
  let oBinding = oDataset.getBinding("data");

  let aFilter = [
    new sap.ui.model.Filter("OrderID", "EQ", sOrderID)
  ];

  oBinding.filter(aFilter);
}
```
</br>

### 문제 4. (Chart Type 변경 + Validation)
- ComboBox에서 선택한 타입(bar, line, donut)으로 차트 형태를 바꾸되, 입력값이 잘못되면 오류 상태를 표시하는 코드를 작성하시오.
```java
onChange: function(oEvent) {
  let sValue = oEvent.getParameter("value");
  let oInput = oEvent.getSource();
  let oChart = this.byId("idViewChart");

  if (["bar","line","donut"].includes(sValue)) {
    oInput.setValueState("None");
    oChart.setVizType(sValue);
  } else {
    oInput.setValueState("Error");
    oInput.setValueStateText("올바른 차트 타입을 입력하세요.");
  }
}
```
</br>

### 문제 5. (버튼 이벤트로 Row 데이터 접근)
- 테이블 안에 있는 버튼 클릭 시, 그 버튼이 속한 Row의 CustomerID를 콘솔에 출력하시오.
- getSource() → 버튼, getParent() → 버튼이 포함된 Row, getBindingContext() → Row의 데이터 연결 정보, getObject() → 실제 JSON 데이터
```java
onPress: function(oEvent) {
  let oRow = oEvent.getSource().getParent();
  let oData = oRow.getBindingContext("customer").getObject();
  console.log(oData.CustomerID);
}
```
</br>

### 문제 6. (ComboBox 초기값 설정)
- 화면이 실행될 때 ComboBox에 “bar” 값을 자동 선택되도록 하시오.
- 초기값은 Model이 아닌 UI컨트롤에서 setSelectedKey()로 직접 지정 가능
```java
onInit() {
  let oCombo = this.byId("idType");
  oCombo.setSelectedKey("bar");
}
```
</br>

### 문제 7. (다중 모델 혼합 바인딩)
- 하나의 View에 OData 모델(default)과 JSON 모델(typeList)을 함께 사용하는 경우, 각각의 컨트롤에 올바르게 바인딩하는 방법을 코드로 작성하시오.
```java
<ComboBox items="{typeList>/Key}">
  <core:ListItem text="{typeList>type}" />
</ComboBox>

<Table items="{/Orders}">
  <columns>
    <Column><Text text="OrderID"/></Column>
  </columns>
  <items>
    <ColumnListItem>
      <cells>
        <Text text="{OrderID}" />
      </cells>
    </ColumnListItem>
  </items>
</Table>
```
</br>

### 문제 8. (데이터 경로와 바인딩Context 이해)
- 테이블에서 선택한 행(Row)의 모델 경로(/Customers/1)를 얻고, 그 경로로 데이터를 직접 읽는 코드를 작성하시오.
```java
onSelectionChange: function(oEvent) {
  let oRow = oEvent.getParameters().listItem;
  let oContext = oRow.getBindingContext("customer");
  let sPath = oContext.getPath();
  let oModel = this.getView().getModel("customer");
  let oData = oModel.getProperty(sPath);
  console.log(oData.Name);
}
```
</br>

### 문제 9. (Fragment 로드 + 컨트롤 접근)
- 다이얼로그 Fragment를 열 때, Fragment 내부의 Input 컨트롤에 접근하여 초기값을 설정하는 코드를 작성하시오.
- Fragment는 별도 scope라서 `sap.ui.getCore().byId()`로 접근
```java
onOpenDialog: function() {
  if (!this._oDialog) {
    this._oDialog = sap.ui.xmlfragment("sap.btp.ux410solving.view.fragment.MyDialog", this);
    this.getView().addDependent(this._oDialog);
  }
  this._oDialog.open();
  let oInput = sap.ui.getCore().byId("idDialogInput");
  oInput.setValue("초기값");
}
```
</br>

### 문제 10. (조건부 필터 + 차트 동적 변경)
- 검색 시 ComboBox에 선택된 OrderID가 없으면 전체 차트 표시, 있으면 해당 OrderID만 표시되도록 구현하시오.
```java
onSearch: function() {
  let oCombo = this.byId("idOrderID");
  let oItem = oCombo.getSelectedItem();
  let oDataset = this.byId("idFlattenedDataset");
  let oBinding = oDataset.getBinding("data");

  if (!oItem) {
    oBinding.filter([]); // 전체 표시
    return;
  }

  let sOrderID = oItem.getBindingContext().getObject().OrderID;
  let aFilter = [ new sap.ui.model.Filter("OrderID", "EQ", sOrderID) ];
  oBinding.filter(aFilter);
}
```
