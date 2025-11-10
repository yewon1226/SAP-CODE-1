- Detail.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/viz/ui5/data/FlattenedDataset"
], (Controller, JSONModel, Filter, FlattenedDataset) => {
    "use strict";

    return Controller.extend("exam.excode032.controller.Detail", {
        onInit() {
            const oRouter = this.getOwnerComponent().getRouter();
            oRouter.getRoute("RouteDetail").attachPatternMatched(this.onPatternMatched, this);
        },
        onSelectionChange: function(oEvent) {
            let oBindingContext = oEvent.getParameters().listItem.getBindingContext("DetailModel");
            let sPath = oBindingContext.getPath();
            let oData = this.getView().getModel("DetailModel").getProperty(sPath).to_flights;

            let to_flightsModel = new JSONModel(oData);
            this.getView().setModel(to_flightsModel, "to_flightsModel");

            this.byId("idChart").setVisible(true);
        },
        onSearch: function() {
            this.byId("idChart").setVisible(false);

            let inputConnectionID = this.getView().byId("idConnectionID").getValue();
            let inputCountryFrom = this.getView().byId("idCountryFrom").getValue();
            let inputCityTo = this.getView().byId("idCityTo").getValue();
            let oFilters = [];

            if(inputCountryFrom && inputCityTo) {       // CountryFrom 와 CityTo 입력
                oFilters.push(new Filter({ path: 'Countryfr', operator: 'EQ', value1: inputCountryFrom }));
                oFilters.push(new Filter({ path: 'Cityto', operator: 'EQ', value1: inputCityTo }));
            }
            else if(inputCountryFrom) {     // CountryFrom 입력 들어올 때 
                oFilters.push(new Filter({ path: 'Countryfr', operator: 'EQ', value1: inputCountryFrom }));
            }
            else if(inputCityTo) {      // CityTo 입력 들어올 때
                oFilters.push(new Filter({ path: 'Cityto', operator: 'EQ', value1: inputCityTo }));
            }
            oFilters.push(new Filter({ path: 'Connid', operator: 'Contains', value1: inputConnectionID }));
            
            let oDataModel = this.getView().getModel();
            let DetailModel = this.getView().getModel("DetailModel");
            let sParam = DetailModel.getProperty("/results")[0].Carrid;
            oFilters.push(new Filter({ path: 'Carrid', operator: 'EQ', value1: sParam }));
            let arr = [];

            oDataModel.read("/Spfli", {
                urlParameters: {
                    "$expand" : "to_flights"
                },
                filters: oFilters,
                success: function(oReturn) {
                    for(let i=0; i<oReturn.results.length; i++) {
                        arr.push(oReturn.results[i]);
                    }
                    DetailModel.setProperty("/results", arr);
                },
                error: function() { console.log("error"); }
            });
        },
        onPatternMatched: function(oEvent) {
            let sParam = oEvent.getParameters().arguments.param1;
            
            let oDataModel = this.getView().getModel();
            let DetailModel = new JSONModel({ results : [] });
            let arr = [];

            oDataModel.read("/Spfli", {
                urlParameters: {
                    "$expand" : "to_flights"
                },
                filters: [
                    new Filter("Carrid", "EQ", sParam)
                ],
                success: function(oReturn) {
                    for(let i=0; i<oReturn.results.length; i++) {
                        arr.push(oReturn.results[i]);
                    }
                    DetailModel.setProperty("/results", arr);
                    this.getView().setModel(DetailModel, "DetailModel");
                }.bind(this),
                error: function() { console.log("error"); }
            })
        }
    }); 
});
```
</br>

- Main.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/core/Fragment"
], (Controller, JSONModel, Fragment) => {
    "use strict";

    return Controller.extend("exam.excode032.controller.Main", {
        onInit() {
            let oData = { result : [] };
            let oLocalModel = new JSONModel(oData);     // 로컬 모델 만들기
            this.getView().setModel(oLocalModel, "local");      // 로컬이라는 이름으로 로컬모델 세팅
            // 기본 모델은 oData Model로 이미 존재하기 때문에 새로 추가하는 JSON Model인 경우 이름 부여
        },
        onDetails: function(oEvent) {
            let oRow = oEvent.getSource().getParent();      // 버튼 객체 위에 있는 해당 열을 가져옴
            let oBindingContext = oRow.getBindingContext("local");
            let sPath = oBindingContext.getPath();
            let sParam = this.getView().getModel("local").getProperty(sPath).Carrid;

            let oRouter = this.getOwnerComponent().getRouter();     // 라우터
            oRouter.navTo("RouteDetail", {
                param1 : sParam
            })
        },
        onAddCarrier: function(oEvent) {
            // Fragmnet.load 해서 파일 로드 하면, oLoadDialog 에 Dialog 객체가 들어옴.
            // 하지만 로드 하기 전에는, oLoadDialog는 undefined로 빈 값임.
            let oDialog = sap.ui.getCore().byId("idDialog");
            let oDataModel = this.getView().getModel();
            
            // Add Carrier 버튼 클릭 시 Dialog fragment가 load 되도록 한다.

            if(oDialog) {       // 이미 로드 되었다면
                oDialog.open();
            }
            else {
                Fragment.load({
                    name: "exam.excode032.view.fragment.Dialog",        // Dialog fragment의 파일 경로
                    type: "XML",        // fragment의 종류 : HTML, XML, JS
                    controller: this    // 현재 controller를 넘겨줌
                                        // Dialog 안에 있는 이벤트 함수 등을 해당 controller에서 구현할 수 있도록 !!
                }).then(function(oDialog) {
                    oDialog.open();
                    oDialog.setModel(oDataModel);
                })
            }
        },
        onDelete: function() {      // Main 테이블 리스트 선택 시 삭제하는 함수
            let oTable = this.byId("idMainCarrierlist");
            let oRows = oTable.getSelectedIndices();        // 선택한 테이블 리스트 인덱스 가져오기

            let oLocalModel = this.getView().getModel("local");
            let arr = oLocalModel.getData().result;                 // 원래 Main 테이블의 리스트들 가져오기
            let delete_arr = [];
            let flag = true;
            for(let i=0; i<arr.length; i++) {
                for(let j=0; j<oRows.length; j++) {
                    if(i === oRows[j]) {
                        flag=false;
                        break;
                    }
                }
                if(flag) {
                    delete_arr.push(arr[i]);
                }
                flag=true;
            }
            oLocalModel.setProperty("/result", delete_arr);
        },
        onOKPress: function() {
            let oTable = sap.ui.getCore().byId("idDialogCarrierlist");
            let sPath = oTable.getSelectedItem().getBindingContext().getPath();
            let sParam = this.getView().getModel().getProperty(sPath).Carrid;

            let oLocalModel = this.getView().getModel("local");
            let oDataModel = this.getView().getModel();

            oDataModel.read(sPath, {
                urlParameters: {
                    "$expand" : "to_schedules"
                },
                success: function(oReturn) {
                    let arr = oLocalModel.getData().result;
                    arr.push(oReturn);
                    oLocalModel.setProperty("/result", arr);
                    this.onClose();
                }.bind(this),
                error: function() {

                }
            })
        },
        onClose: function() {
            // Fragment.load 로 불러온 객체의 경우
            // sap.ui.getCore().byId("객체아이디") 로 접근해서 가져옴
            sap.ui.getCore().byId("idDialog").close();

            // 만약 Dialog 안에 Table이 있고 해당 Table을 가져오고자 한다면?
            // sap.ui.getCore().byId("TableID");  
        }
    });
});
```
</br>

- fragmnet/Dialog.fragment.xml
```html
<c:FragmentDefinition 
    xmlns="sap.m" 
    xmlns:c="sap.ui.core">
    <Dialog id="idDialog"
            title="Select List"
            contentHeight="50%"
            contentWidth="40%">
        <content>
            <Table id="idDialogCarrierlist"
                inset="false"
                mode="SingleSelectLeft"
                selectionChange="onSelectionChange"
                items="{/Carriers}">
                <columns>
                    <Column>
                        <Text text="Carrier ID" />
                    </Column>
                    <Column>
                        <Text text="Carrier Name" />
                    </Column>
                </columns>
                <items>
                    <ColumnListItem vAlign="Middle">
                        <cells>
                            <Text text="{Carrid}" />
                            <Text text="{Carrname}" />
                        </cells>
                    </ColumnListItem>
                </items>
            </Table>
        </content>
        <buttons>
            <Button text="OK" press=".onOKPress" type="Emphasized" />
            <Button text="close" press=".onClose" />
        </buttons>
    </Dialog>
</c:FragmentDefinition>
```
</br>

- Detail.view.xml
```html
<mvc:View controllerName="exam.excode032.controller.Detail"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:f="sap.f"
    xmlns:fb="sap.ui.comp.filterbar"
    xmlns:viz="sap.viz.ui5.controls"
    xmlns:viz.feeds="sap.viz.ui5.controls.common.feeds"
    xmlns:viz.data="sap.viz.ui5.data"
    xmlns="sap.m">
    <f:DynamicPage id="dynamicPageId" headerExpanded="{/headerExpanded}" toggleHeaderOnTitleClick="{/titleClickable}">
        <f:header>
            <f:DynamicPageHeader>
                <fb:FilterBar id="filterbar" persistencyKey="myPersKey" useToolbar="false" search=".onSearch" filterChange=".onFilterChange" afterVariantLoad=".onAfterVariantLoad">
                    <fb:filterGroupItems>
                        <fb:FilterGroupItem name="ConnectionID" label="Connection ID" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <Input id="idConnectionID"/>
                            </fb:control>
                        </fb:FilterGroupItem>

                        <fb:FilterGroupItem name="CountryFrom" label="Country From" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <Input id="idCountryFrom"/>
                            </fb:control>
                        </fb:FilterGroupItem>

                        <fb:FilterGroupItem name="CityTo" label="City To" groupName="Group1" visibleInFilterBar="true">
                            <fb:control>
                                <Input id="idCityTo"/>
                            </fb:control>
                        </fb:FilterGroupItem>
                    </fb:filterGroupItems>
                </fb:FilterBar>
            </f:DynamicPageHeader>
        </f:header>
        <f:content>
            <VBox> 
                <Table id="idDetailCarrierlist" 
                    inset="false"
                    mode="SingleSelectLeft"
                    selectionChange="onSelectionChange"
                    items="{DetailModel>/results}">
                    <columns>
                        <Column hAlign="Center">
                            <Text text="Carrier ID" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Connection ID" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Country from" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="City from" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Airpfrom" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Countryto" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Cityto" />
                        </Column>
                        <Column hAlign="Center">
                            <Text text="Airpto" />
                        </Column>
                    </columns>
                    <items>
                        <ColumnListItem vAlign="Middle">
                            <cells>
                                <Text text="{DetailModel>Carrid}" />
                                <Text text="{DetailModel>Connid}" />
                                <Text text="{DetailModel>Countryfr}" />
                                <Text text="{DetailModel>Cityfrom}" />
                                <Text text="{DetailModel>Airpfrom}" />
                                <Text text="{DetailModel>Countryto}" />
                                <Text text="{DetailModel>Cityto}" />
                                <Text text="{DetailModel>Airpto}" />
                            </cells>
                        </ColumnListItem>
                    </items>
                </Table>

                <viz:VizFrame id="idChart" width="100%" height="400px"
                            vizType="column"
                            visible="false"
                            uiConfig="{'applicationSet':'fiori'}"
                            vizProperties="{ 
                                type : 'sap.ui.model.type.Date',
                                formatOptions: { pattern: 'yy-MM-dd'},
                                title : { visible : true, text : '항공편별 잔여석 현황', alignment: 'left' },
                                plotArea : { 
                                    dataLabel: { visible: true } ,
                                    colorPalette: ['#B7F0B1']
                                }
                            }">
                    <viz:dataset>
                        <viz.data:FlattenedDataset data="{to_flightsModel>/results}">
                            <viz.data:dimensions>
                                <viz.data:DimensionDefinition name="날짜" value="{
                                    path: 'to_flightsModel>Fldate',
                                    type: 'sap.ui.model.type.Date',
                                    formatOptions: { pattern: 'yy-MM-dd' }
                                }" />
                            </viz.data:dimensions>

                            <viz.data:measures>
                                <viz.data:MeasureDefinition name="잔여석" value="{to_flightsModel>Seatsocc}" />
                                <viz.data:MeasureDefinition name="잔여석2" value="{to_flightsModel>Seatsocc}" />
                            </viz.data:measures>
                        </viz.data:FlattenedDataset>
                    </viz:dataset>

                    <viz:feeds>
                        <viz.feeds:FeedItem uid="categoryAxis" type="Dimension" values="날짜" />
                        <viz.feeds:FeedItem uid="valueAxis" type="Measure" values="잔여석" />
                    </viz:feeds>
                </viz:VizFrame>

            </VBox>
        </f:content>
    </f:DynamicPage>
</mvc:View>
```
</br>

- Main.view.xml
```html
<mvc:View controllerName="exam.excode032.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:t="sap.ui.table"
    xmlns:rowmodes="sap.ui.table.rowmodes"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">
        <t:Table id="idMainCarrierlist"
            class="sapUiMediumMargin"
            rows="{local>/result}"
            selectionMode="MultiToggle"
            paste="onPaste"
            ariaLabelledBy="title">
            <t:rowMode>
                <rowmodes:Interactive rowCount="20"/>
            </t:rowMode>
            <t:extension>
                <OverflowToolbar style="Clear">
                    <Title text="Carrier List"/>
                    <ToolbarSpacer />
                    <Button text="Add Carrier" press="onAddCarrier" />
                    <Button text="Delete" press="onDelete" />
                </OverflowToolbar>
            </t:extension>
            <t:columns>
                <t:Column>
                    <Label text="Carrier ID" />
                    <t:template>
                        <Text text="{local>Carrid}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column>
                    <Label text="Carrier Name" />
                    <t:template>
                        <Text text="{local>Carrname}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column>
                    <Label text="Currency Code" />
                    <t:template>
                        <Text text="{local>Currcode}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column>
                    <Label text="Url" />
                    <t:template>
                        <Text text="{local>Url}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column>
                    <Label text="Details" />
                    <t:template>
                        <Button text="Details" press="onDetails" enabled="{= ${local>to_schedules/results}.length === 0 ? false : true}" />
                    </t:template>
                </t:Column>
            </t:columns>
        </t:Table>
    </Page>
</mvc:View>
```
</br>

- manifest.json
```java
"routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "controlAggregation": "pages",
        "controlId": "app",
        "transition": "slide",
        "type": "View",
        "viewType": "XML",
        "path": "exam.excode032.view",
        "async": true,
        "viewPath": "exam.excode032.view"
      },
      "routes": [
        {
          "name": "RouteMain",
          "pattern": ":?query:",
          "target": [
            "TargetMain"
          ]
        }, {
          "name": "RouteDetail",
          "pattern": "detail/{param1}/:param2:",
          "target": [
            "TargetDetail"
          ]
        }
      ],
      "targets": {
        "TargetMain": {
          "id": "Main",
          "name": "Main"
        }, 
        "TargetDetail": {
          "id": "Detail",
          "name": "Detail"
        }
      }
    }
```
