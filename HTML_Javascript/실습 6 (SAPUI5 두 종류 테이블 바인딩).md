### SAPUI5 테이블 바인딩 실습 (m.Table + grid Table)
- 로컬 JSONModel의 persons 배열을 두 종류 테이블(sap.m.Table, sap.ui.table.Table)에 바인딩해 이름/나이/전화 표시, m.Table에서는 나이 필드 편집 가능(Input)
</br>

<img width="827" height="353" alt="image" src="https://github.com/user-attachments/assets/d49ed7a5-d0bc-4d54-9210-3ae034911c53" />

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

    return Controller.extend("ycl2project06.controller.Main", {
        onInit() {
            let oData = { inputValue: { firstName: "Park", lastName: "Gildong"},
                          persons : [
                            { name: "김예원", age: 23, tel: "010-8952-0763"},
                            { name: "김형동", age: 10, tel: "010-1234-0763"},
                            { name: "박근철", age: 23, tel: "010-6379-6174"},
                            { name: "정다현", age: 24, tel: "010-1234-5678"}
                          ]
                        };
            let oModel = new JSONModel(oData);
            this.getView().setModel(oModel, "local");
        },
    });
});
```
</br>

- Main.view.xml
```html
<mvc:View controllerName="ycl2project06.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:core="sap.ui.core"
    xmlns:p="sap.m.plugins"
    xmlns:u="sap.ui.unified"
    xmlns:t="sap.ui.table"
    xmlns="sap.m">
    <Page id="page" title="{test>/inputValue/firstName} {test>/inputValue/lastName}">        <!-- 객체에는 local>/name/firstName -->
        <!-- <Input id="input" value="{/inputValue/firstName} {/inputValue/lastName}" /> -->
        <Table id="idProductsTable"
            inset="false"
            items="{local>/persons}">       <!-- 배열까지 바인딩 상태 -->
            <columns>
                <Column hAlign="Center">
                    <Text text="Name" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Age" />
                </Column>
                <Column hAlign="Center">
                    <Text text="Tel" />
                </Column>
            </columns>
            <items>
                <ColumnListItem vAlign="Middle">
                    <cells>
                        <Text text="{local>name}" />        <!-- 상대경로 -->
                        <Input value="{local>age}" />
                        <Text text="{local>tel}" />
                    </cells>
                </ColumnListItem>
            </items>
        </Table>

        <t:Table
            rows="{local>/persons}"
            selectionMode="MultiToggle"
            paste="onPaste"
            ariaLabelledBy="title">
            <t:extension>
                <OverflowToolbar style="Clear">
                    <Title id="title" text="Persons"/>
                </OverflowToolbar>
            </t:extension>
            <t:columns>
                <t:Column width="11rem">
                    <Label text="Name" />
                    <t:template>
                        <Text text="{local>name}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column width="11rem">
                    <Label text="Age" />
                    <t:template>
                        <Text text="{local>age}" wrapping="false" />
                    </t:template>
                </t:Column>
                <t:Column width="11rem">
                    <Label text="Tel" />
                    <t:template>
                        <Text text="{local>tel}" wrapping="false" />
                    </t:template>
                </t:Column>
            </t:columns>
        </t:Table>

    </Page>

</mvc:View>
```
