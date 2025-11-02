### Fragment 기반 Part UI + 동적 Dialog 실습
- 버튼 클릭 시 Dialog 프래그먼트를 동적으로 Fragment.load로 로드·오픈하고, core 접근으로 닫는 기능 개발
</br>

<img width="497" height="193" alt="image" src="https://github.com/user-attachments/assets/f0cfca31-1966-4334-b629-811a6c932930" />

<img width="759" height="345" alt="image" src="https://github.com/user-attachments/assets/d9042e52-8ce2-454a-bab6-d8e0e4100600" />

</br>
</br>
</br>

- Main.view.xml
```html
<mvc:View controllerName="ycl2project05.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:core="sap.ui.core"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">

    <core:Fragment fragmentName="ycl2project05.view.fragment.PartUi" type="XML" />

    </Page>
</mvc:View>
```
</br>

- fragment/PartUi.fragment.xml
```html
<c:FragmentDefinition 
    xmlns="sap.m" 
    xmlns:c="sap.ui.core">
    
    <Text text="Part UI Fragment" />
    <Button text="Click" press=".onClick" />
    
</c:FragmentDefinition>
```
</br>

- fragment/Dialog.fragment.xml
```html
<c:FragmentDefinition 
    xmlns="sap.m" 
    xmlns:c="sap.ui.core">
    <Dialog id="idDialog"
            title="First Dialog">
        <content>
            <Text text="Hello Dialog" />
        </content>
        <endButton>
            <Button text="close" press=".onClose" />
        </endButton>
    </Dialog>
</c:FragmentDefinition>
```
</br>

- Main.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/core/Fragment"
], (Controller, Fragment) => {
    "use strict";

    return Controller.extend("ycl2project05.controller.Main", {
        onInit() {
        },
        onClick: function() {
            let oDialog=sap.ui.getCore().byId("idDialog");

           // sap.m.MessageToast.show("PartUI 버튼 클릭");
           console.log("11111111111");

           if(oDialog) {        // 있으면 true, 없으면 undefined
                oDialog.open();
           } else {
                Fragment.load({
                        name: "ycl2project05.view.fragment.Dialog",
                        type: "XML",
                        controller: this    // 로드하는 fragment에서 사용할 수 있도록 현재 controller 넘겨줌
                }).then(function(oDialog) {
                        console.log("222222222222");
                        oDialog.open();
                })
           }

          console.log("33333333333")
        },
        onClose: function() {
            // sap.m.MessageToast.show("onClose 클릭");
            // this.byId("idDialog").close(); 하면 동작하지 않음.
            // 왜?
            // => Fragment.load 해서 로드한 뷰는 기존 view의 계층 구조와 분리된 UI요소로 간주된다. 따라서 view가 아닌 core에 접근해야함

            sap.ui.getCore().byId("idDialog").close();
        }
    });
});
```
