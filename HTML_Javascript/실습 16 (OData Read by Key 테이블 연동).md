### OData Read by Key로 단일 데이터 조회 후 JSONModel에 바인딩하여 테이블 표시
- 첫 번째 OData 테이블에서 항공편을 선택하면, 선택된 키로 단건 조회(read)를 수행하여 조회된 상세 데이터(Airpto)를 JSONModel에 저장하고, 두 번째 테이블에 자동으로 표시
</br>

<img width="642" height="315" alt="image" src="https://github.com/user-attachments/assets/71a97f85-a5a0-4b2f-a549-6f2f9459efa8" />
</br>
</br>

<img width="646" height="125" alt="image" src="https://github.com/user-attachments/assets/bbd996ac-09cd-440e-a490-1df645dc6cd9" />
</br>
</br>
</br>

- Main.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel"
], (Controller, JSONModel) => {
    "use strict";

    return Controller.extend("practice01.controller.Main", {
        onInit() {
            let oModel = this.getOwnerComponent().getModel();
            this.getView().setModel(oModel, "local");

            let AirptoModel = new JSONModel({data : []});
            this.getView().setModel(AirptoModel, "aModel");
        },
        onSelectionChange: function() {
            let table = this.byId("idesConListSet");  
            let oRow = table.getSelectedItem();     
            let oBindingContext = oRow.getBindingContext("local");
            let sPath = oBindingContext.getPath();

            let oModel = this.getView().getModel("local");
            let inputCarrid = oModel.getProperty(sPath).Carrid;  
            let inputConnid = oModel.getProperty(sPath).Connid; 
            
            let aModel = this.getView().getModel("aModel");
            
            oModel.read(`/esConDetailSet(Carrid='${inputCarrid}',Connid='${inputConnid}')`, {            
                success: function(oReturn) {
                    aModel.setProperty("/data", [{ Airpto: oReturn.Airpto }]);
                },
                error: function() {
                    
                }
            });
        }
    });
});
```
</br>

- Main.view.xml
```html
<mvc:View controllerName="practice01.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">
        <Table id="idesConListSet"
            inset="false"
            mode="SingleSelectLeft"
            selectionChange="onSelectionChange"
            items="{local>/esConListSet}">
            <columns>
                <Column hAlign="Center">
                    <Text text="Carrid" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Carrname" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Connid" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Cityfrom" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Cityto" />
                </Column>
            </columns>
            <items>
                <ColumnListItem vAlign="Middle">
                    <cells>
                        <Text text="{local>Carrid}" />
                        <Text text="{local>Carrname}" />
                        <Text text="{local>Connid}" />
                        <Text text="{local>Cityfrom}" />
                        <Text text="{local>Cityto}" />
                    </cells>
                </ColumnListItem>
            </items>
        </Table>

        <Table id="idesConDetailSet"
            inset="false"
            mode="SingleSelectLeft"
            selectionChange="onSelectionChange"
            items="{aModel>/data}">
            <columns>
                <Column hAlign="Center">
                    <Text text="Airpto" />
                </Column>
            </columns>
            <items>
                <ColumnListItem vAlign="Middle">
                    <cells>
                        <Text text="{aModel>Airpto}" />
                    </cells>
                </ColumnListItem>
            </items>
        </Table>
    </Page>
</mvc:View>
```
