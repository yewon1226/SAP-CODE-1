### 버튼 클릭 시 OData read()로 필터링된 제품 데이터를 테이블에 표시
- 버튼 클릭 시 onPress 함수에서 OData read()를 사용해 제품 데이터를 가져와 JSON 모델에 담고, DynamicPage와 FilterBar로 필터링 후 테이블에 표시하는 SAPUI5 화면
</br>

<img width="791" height="245" alt="image" src="https://github.com/user-attachments/assets/540a4f50-8c3b-464c-8827-00cf7014c522" />
</br>
</br>

<img width="805" height="392" alt="image" src="https://github.com/user-attachments/assets/88a0eef9-28d9-42d1-a7f6-5eb35187fac1" />
</br>
</br>
</br>

- Read.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], (Controller, JSONModel, Filter, FilterOperator) => {
    "use strict";

    return Controller.extend("ycl2project09.controller.Read", {
        onInit() {
            let oModel = new JSONModel({results : []});
            this.getView().setModel(oModel, "local");
        },
        onPress: function() {
            // 버튼 클릭 시 OData Model 데이터 조회
            // 서버모델 데이터를 가져오려면, Request 요청을 보내야 함
            // OData Model에서 데이터를 가져온다.
            // 데이터를 가져오면, json 구조에 맞는 데이터가 리턴
            // 해당 json data를 json model에다가 담아서 사용한다.
            // let oModel = this.getView().getModel();
            let oLocalModel = this.getView().getModel("local");
            let oModel = this.getOwnerComponent().getModel();   // OData Model. 이름없는 기본모델
            let oFilter = new Filter({
                path : "ProductID",
                operator : "BT",
                value1 : 10,
                value2 : 20
            });

            // .create() 생성 .read() 조회 .update() 업데이트 .remove() 삭제 
            // oModel.read("경로", {});
            oModel.read("/Products", {
                urlParameters: {
                    "$expand" : "Supplier"
                },
                filters: [oFilter],
                // sorters: [],
                success: function(oReturn) {
                    // 단 건 조회 : oReturn 자체에 데이터가 들어옴
                    // 다 건 조회 : oReturn.results 해서 배열 접근

                    // oLocalModel.setProperty("경로", 세팅할데이터);
                    let oData = [];
                    for(let i=0; i<oReturn.results.length; i++) {
                        let item = oReturn.results[i];
                        oData.push({
                            ProductID: item.ProductID,
                            ProductName: item.ProductName,
                            SupplierID: item.SupplierID,
                            UnitsInStock: item.UnitsInStock
                        });
                    }
                    oLocalModel.setProperty("/results", oData);
                },
                error: function() {}
            });
        }
    });
});
```
</br>

- Read.view.xml
```html
<mvc:View controllerName="ycl2project09.controller.Read"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:f="sap.f"
    xmlns:fb="sap.ui.comp.filterbar"
    xmlns:core="sap.ui.core"
    xmlns="sap.m"
    >
    
    <f:DynamicPage id="dynamicPageId" headerExpanded="{/headerExpanded}" toggleHeaderOnTitleClick="{/titleClickable}">
        <!-- DynamicPage Title -->
        <!-- DynamicPage Header -->
        <f:header>
            <f:DynamicPageHeader pinnable="true">
                <fb:FilterBar id="filterbar" persistencyKey="myPersKey" useToolbar="false" search=".onSearch" filterChange=".onFilterChange" afterVariantLoad=".onAfterVariantLoad">
                    <fb:filterGroupItems>
                        <fb:FilterGroupItem name="Name" label="Name" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <Input id="inputProductName" showValueHelp="true" valueHelpRequest="onValueHelpRequest" placeholder="ProductName" />
                            </fb:control>
                        </fb:FilterGroupItem>
                        <fb:FilterGroupItem name="Category" label="Category" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <MultiComboBox
                                    name="Category"
                                    selectionChange=".onSelectionChange"
                                    items="{
                                        path: '/ProductCategories',
                                        templateShareable: true
                                    }"
                                >
                                    <core:Item key="{key}" text="{name}"/>
                                </MultiComboBox>
                            </fb:control>
                        </fb:FilterGroupItem>
                        <fb:FilterGroupItem name="SupplierName" label="SupplierName" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <MultiComboBox
                                    name="SupplierName"
                                    selectionChange=".onSelectionChange"
                                    items="{
                                        path: '/ProductSuppliers',
                                        templateShareable: true
                                    }"
                                >
                                    <core:Item key="{key}" text="{name}"/>
                                </MultiComboBox>
                            </fb:control>
                        </fb:FilterGroupItem>
                    </fb:filterGroupItems>
                </fb:FilterBar>
            </f:DynamicPageHeader>
        </f:header>
        <f:content>
            <VBox> 
                <Button text="데이터 조회" press="onPress" />
                <Table id="idProductsTable2"
                    sticky="HeaderToolbar,ColumnHeaders"
                    inset="false"
                    items="{local>/results}"
                    class="sapFDynamicPageAlignContent"
                    width="auto">
                    <headerToolbar>
                        <Toolbar>
                            <Title text="Products" level="H2"/>
                        </Toolbar>
                    </headerToolbar>
                    <columns>
                        <Column width="12em">
                            <Text text="ProductID" />
                        </Column>
                        <Column width="12em">
                            <Text text="ProductName" />
                        </Column>
                        <Column width="12em">
                            <Text text="SupplierID" />
                        </Column>
                        <Column width="12em">
                            <Text text="UnitsInStock" />
                        </Column>
                    </columns>
                    <items>
                        <ColumnListItem>
                            <cells>
                                <Text text="{local>ProductID}" />
                                <Text text="{local>ProductName}" />
                                <Text text="{local>SupplierID}" />
                                <Text text="{local>UnitsInStock}" />
                            </cells>
                        </ColumnListItem>
                    </items>
                </Table>
            </VBox>
        </f:content>
        <!-- DynamicPage Footer -->
        <f:footer>
            <OverflowToolbar>
                <ToolbarSpacer/>
                <Button type="Accept" text="Accept"/>
                <Button type="Reject" text="Reject"/>
            </OverflowToolbar>
        </f:footer>
    </f:DynamicPage>
</mvc:View>
```
