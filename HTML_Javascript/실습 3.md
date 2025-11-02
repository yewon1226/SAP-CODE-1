### SAPUI5 레이아웃 및 폼 구성 실습
- Panel + Toolbar + SimpleForm(ColumnLayout)으로 Supplier 정보 입력 폼을 구현하고, 확장/접기 가능한 패널 UI 레이아웃 구성을 실습함

<img width="1887" height="728" alt="image" src="https://github.com/user-attachments/assets/4137b90c-1d0a-4a83-be19-6e3e59fdf3f8" />

<img width="1919" height="865" alt="image" src="https://github.com/user-attachments/assets/baf8ce8e-ee5d-4220-be48-12a2c33ec529" />

</br>
</br>
</br>

- Main.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller"
], (Controller) => {
    "use strict";

    return Controller.extend("ycl2project03.controller.Main", {
        onInit() {
        }
    });
});
```
</br>

- Main.view.xml
```java
<mvc:View controllerName="ycl2project03.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:l="sap.ui.layout"
    xmlns:f="sap.ui.layout.form"
	xmlns:core="sap.ui.core"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">
        <!-- <l:VerticalLayout class="sapUiContentPadding">       

            <Input value="input Value"/>
            <Text text="text" />
            <StepInput />
            <ComboBox ></ComboBox>      
            <Select />                  
            <DatePicker displayFormat="yyyy-MM-dd" />
            <DateRangeSelection />
            <Label text="label" />

        </l:VerticalLayout> -->

        <!-- sap.m.VBox 또는 sap.m.HBox로 대체 사용 가능 -->
        
        <!-- <l:HorizontalLayout >
            <Input value="input Value"/>
            <Text text="text" />
            <StepInput />
            <ComboBox ></ComboBox>      
            <Select />                  
            <DatePicker displayFormat="yyyy-MM-dd" />
            <DateRangeSelection />
            <Label text="label" />
        </l:HorizontalLayout> -->

        <!-- <l:Grid>
            <Input value="TEST1">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
            <Input value="TEST2">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
            <Input value="TEST3">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
            <Input value="TEST4">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
            <Input value="TEST5">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
            <Input value="TEST6">
                <layoutData>
                    <l:GridData span="XL6 L2 M4 S12" />
                </layoutData>
            </Input>
        </l:Grid> -->

        <!-- <f:Form id="FormChangeColumn_threeGroups234"
			editable="true">
			<f:title>
				<core:Title text="Supplier" />
			</f:title>
            <f:toolbar>
                <Toolbar >
                    <Title text="Toolbar title" />
                    <ToolbarSpacer />
                    <Button text="Test" />
                </Toolbar>
            </f:toolbar>
			<f:layout>
				<f:ColumnLayout
					columnsM="2"
					columnsL="3"
					columnsXL="4"
				/>
			</f:layout>
			<f:formContainers>
				<f:FormContainer title="Address">
					<f:formElements>
						<f:FormElement label="Name">
							<f:fields>
								<Input value="{SupplierName}" id="name"/>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Street">
							<f:fields>
								<Input value="{Street}" />
								<Input value="{HouseNumber}">
									<layoutData>
										<f:ColumnElementData cellsSmall="2" cellsLarge="1" />
									</layoutData>
								</Input>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="ZIP Code/City">
							<f:fields>
								<Input value="{ZIPCode}">
									<layoutData>
										<f:ColumnElementData cellsSmall="3" cellsLarge="2" />
									</layoutData>
								</Input>
								<Input value="{City}" />
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Country">
							<f:fields>
								<Select id="country" selectedKey="{Country}">
									<items>
										<core:Item text="England" key="England"/>
										<core:Item text="Germany" key="Germany"/>
										<core:Item text="USA" key="USA"/>
									</items>
								</Select>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Web">
							<f:fields>
								<Input value="{Url}" type="Url"/>
							</f:fields>
						</f:FormElement>
					</f:formElements>
				</f:FormContainer>
				<f:FormContainer title="Contact">
					<f:formElements>
						<f:FormElement label="Twitter">
							<f:fields>
								<Input value="{Twitter}" />
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Email">
							<f:fields>
								<Input value="{Email}" type="Email"/>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Tel.">
							<f:fields>
								<Input value="{Tel}" type="Tel"/>
							</f:fields>
						</f:FormElement>
					</f:formElements>
				</f:FormContainer>
				<f:FormContainer title="Other">
					<f:formElements>
						<f:FormElement label="SMS">
							<f:fields>
								<Input value="{Sms}" type="Tel"/>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Mobile">
							<f:fields>
								<Input value="{Mobile}" type="Tel"/>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Pager">
							<f:fields>
								<Input value="{Pager}" type="Tel"/>
							</f:fields>
						</f:FormElement>
						<f:FormElement label="Fax">
							<f:fields>
								<Input value="{Fax}" type="Tel"/>
							</f:fields>
						</f:FormElement>
					</f:formElements>
				</f:FormContainer>
			</f:formContainers>
		</f:Form> -->

        <!-- /////////////////////////////////////////////////////////////////////////////////////// -->
        
        <!-- <f:SimpleForm id="SimpleFormChangeColumn_threeGroups234"
			editable="true"
			layout="ColumnLayout"
			title="Supplier"
			columnsM="2"
			columnsL="3"
			columnsXL="4"
			>
			<f:content>
				<core:Title text="Address" />
				<Label text="Name" />
				<Input id="name" value="{SupplierName}" />
				<Label text="Street/No." />
				<Input value="{Street}">
				</Input>
				<Input value="{HouseNumber}">
					<layoutData>
						<f:ColumnElementData cellsSmall="2" cellsLarge="1" />
					</layoutData>
				</Input>
				<Label text="ZIP Code/City" />
				<Input value="{ZIPCode}">
					<layoutData>
						<f:ColumnElementData cellsSmall="3" cellsLarge="2" />
					</layoutData>
				</Input>
				<Input value="{City}" />
				<Label text="Country" />
				<Select id="country" selectedKey="{Country}">
					<items>
						<core:Item text="England" key="England"/>
						<core:Item text="Germany" key="Germany"/>
						<core:Item text="USA" key="USA"/>
					</items>
				</Select>
				<Label text="Web" />
				<Input value="{Url}" type="Url"/>
				<core:Title text="Contact" />
				<Label text="Twitter" />
				<Input value="{Twitter}" />
				<Label text="Email" />
				<Input value="{Email}" type="Email"/>
				<Label text="Tel." />
				<Input value="{Tel}" type="Tel"/>
				<core:Title text="Other" />
				<Label text="SMS" />
				<Input value="{Sms}" type="Tel"/>
				<Label text="Mobile" />
				<Input value="{Mobile}" type="Tel"/>
				<Label text="Pager" />
				<Input value="{Pager}" type="Tel"/>
				<Label text="Fax" />
				<Input value="{Fax}" type="Tel"/>
			</f:content>
		</f:SimpleForm> -->

        <!-- /////////////////////////////////////////////////////////////////////////////////////// -->

        <Panel expandable="true" expanded="true">
            <headerToolbar>
                <Toolbar>
                    <Text text="Panel TEST" />
                    <ToolbarSpacer />
                    <Button text="add" icon="sap-icon://add-contact" />
                </Toolbar>
            </headerToolbar>
                <f:SimpleForm id="SimpleFormChangeColumn_threeGroups234"
                editable="true"
                layout="ColumnLayout"
                title="Supplier"
                columnsM="2"
                columnsL="3"
                columnsXL="4"
                >
                <f:content>
                    <core:Title text="Address" />
                    <Label text="Name" />
                    <Input id="name" value="{SupplierName}" />
                    <Label text="Street/No." />
                    <Input value="{Street}">
                    </Input>
                    <Input value="{HouseNumber}">
                        <layoutData>
                            <f:ColumnElementData cellsSmall="2" cellsLarge="1" />
                        </layoutData>
                    </Input>
                    <Label text="ZIP Code/City" />
                    <Input value="{ZIPCode}">
                        <layoutData>
                            <f:ColumnElementData cellsSmall="3" cellsLarge="2" />
                        </layoutData>
                    </Input>
                    <Input value="{City}" />
                    <Label text="Country" />
                    <Select id="country" selectedKey="{Country}">
                        <items>
                            <core:Item text="England" key="England"/>
                            <core:Item text="Germany" key="Germany"/>
                            <core:Item text="USA" key="USA"/>
                        </items>
                    </Select>
                    <Label text="Web" />
                    <Input value="{Url}" type="Url"/>
                    <core:Title text="Contact" />
                    <Label text="Twitter" />
                    <Input value="{Twitter}" />
                    <Label text="Email" />
                    <Input value="{Email}" type="Email"/>
                    <Label text="Tel." />
                    <Input value="{Tel}" type="Tel"/>
                    <core:Title text="Other" />
                    <Label text="SMS" />
                    <Input value="{Sms}" type="Tel"/>
                    <Label text="Mobile" />
                    <Input value="{Mobile}" type="Tel"/>
                    <Label text="Pager" />
                    <Input value="{Pager}" type="Tel"/>
                    <Label text="Fax" />
                    <Input value="{Fax}" type="Tel"/>
                </f:content>
            </f:SimpleForm>
        </Panel>

        <Panel expandable="true" expanded="true">
            <headerToolbar>
                <Toolbar>
                    <Text text="Panel TEST2" />
                    <ToolbarSpacer />
                    <Button text="add" icon="sap-icon://add-contact" />
                </Toolbar>
            </headerToolbar>
            <Text text="Hello Panel" />
        </Panel>

        <Panel class="sapUiSmallMargin" expandable="true" expanded="true">
            <headerToolbar>
                <Toolbar>
                    <Text text="Panel TEST3" />
                    <ToolbarSpacer />
                    <Button text="add" icon="sap-icon://add-contact" />
                </Toolbar>
            </headerToolbar>
            <Text text="Hello Panel" />
        </Panel>
    </Page>
</mvc:View>
```
