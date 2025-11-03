### 두 개의 체크박스 선택 상태에 따라 텍스트를 동적으로 표시하는 SAPUI5 이벤트 처리 실습 예제
- 체크박스 선택 여부에 따라 버튼 클릭 시 해당 옵션 텍스트를 화면에 출력하는 기능을 개발함

<img width="256" height="232" alt="image" src="https://github.com/user-attachments/assets/ecd0ec75-476b-486d-895b-1ae021b2534d" />

<img width="188" height="173" alt="image" src="https://github.com/user-attachments/assets/7946b852-0794-48b6-b8ff-3b9553f89931" />

</br>
</br>
</br>

- Practice.view.xml
```html
<mvc:View controllerName="ycl2project02.controller.Practice"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">
        <content>
            <VBox id="vbox">
                <CheckBox id="idCheckBox1" text="option 1" selected="true"/>
                <CheckBox id="idCheckBox2" text="option 2" selected="false"/>
                <Button id="idButton" text="Button" press="onClick"/>
                <Text id="idText"/>
            </VBox>
        </content>
    </Page>
</mvc:View>
```
</br>

- Practice.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller"
], (Controller) => {
    "use strict";

    return Controller.extend("ycl2project02.controller.Practice", {
        onInit() {
        },
        onClick: function() {
            let checkbox1 = this.getView().byId("idCheckBox1");
            let checkbox2 = this.getView().byId("idCheckBox2");
            let textbox1 = checkbox1.getText();
            let textbox2 = checkbox2.getText();
            let selectbox1 = checkbox1.getSelected();
            let selectbox2 = checkbox2.getSelected();
            let oText = this.getView().byId("idText");

            if(selectbox1 && selectbox2) {
                oText.setText(textbox1+textbox2);
            }
            else if(selectbox1) {
                oText.setText(textbox1);
            }
            else if(selectbox2) {
                oText.setText(textbox2);
            }
        }
    });
});
```
