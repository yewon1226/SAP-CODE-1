### VizFrame 차트 실습 (도넛 + 라인/막대 차트)
- sap.viz.ui5 라이브러리를 활용해 도넛 차트(View XML) 와 라인/막대 차트(Controller 동적 생성) 를 각각 구성하여, JSONModel 데이터 바인딩 + FlattenedDataset + FeedItem 으로 시각화하는 실습
<img width="790" height="861" alt="image" src="https://github.com/user-attachments/assets/a403f395-ed9a-42d2-a5de-88ad66f0d705" />

- Main.controller.js
```java
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/viz/ui5/data/FlattenedDataset",
    "sap/viz/ui5/controls/common/feeds/FeedItem"
], (Controller, JSONModel, FlattenedDataset, FeedItem) => {
    "use strict";

    return Controller.extend("ycl2project11.controller.Main", {
        onInit() {
            let oData = {
                list : [
                    {name: "aaa", rate: "35", cost: "10"},
                    {name: "bbb", rate: "15", cost: "12"},
                    {name: "ccc", rate: "10", cost: "11"},
                    {name: "ddd", rate: "15", cost: "15"},
                    {name: "eee", rate: "20", cost: "10"},
                    {name: "fff", rate: "5", cost: "16"}
                ]
            };
            this.getView().setModel(new JSONModel(oData), "view");

            let oData_controller = {
                sales : [
                    { product : "Jackets", amount : "65" },
                    { product : "Shirts", amount : "70" },
                    { product : "Pants", amount : "86" },
                    { product : "Coats", amount : "92" },
                    { product : "Purse", amount : "77" },
                ]
            };
            this.getView().setModel(new JSONModel(oData_controller), "cont");
            
            this._setChartController();
        },
        _setChartController: function() {
            let oChart = this.byId("idControllerChart");
            // dataset 만들기
            let oDataSet = new FlattenedDataset({
                dimensions: [{ name : "Product", value : "{cont>product}" }],
                measures: [{ name : "Amount", value : "{cont>amount}" }],
                data: { path : "cont>/sales" }
            });
            oChart.setDataset(oDataSet);
            // feedItem 구성하기
            let feedValueAxis = new FeedItem({
                uid: "valueAxis",
                type: "Measure",
                values: ["Amount"]
            });
            let feedCategoryAxis = new FeedItem({
                uid: "categoryAxis",
                type: "Dimension",
                values: ["Product"]
            });

            oChart.addFeed(feedValueAxis);
            oChart.addFeed(feedCategoryAxis);

            oChart.setVizProperties({
                title: { test : '막대차트', visible : true }
            })
            oChart.setVizType("line");
        }
    });
});
```
</br>

- Main.view.xml
```html
<mvc:View controllerName="ycl2project11.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:viz="sap.viz.ui5.controls"
    xmlns:viz.feeds="sap.viz.ui5.controls.common.feeds"
    xmlns:viz.data="sap.viz.ui5.data"
    xmlns="sap.m">
    <Page id="page" title="{i18n>title}">
        <viz:VizFrame id="idViewChart" width="300px" height="300px"
                      vizType="donut"
                      uiConfig="{'applicationSet':'fiori'}"
                      vizProperties="{
                        title : { visible : true, text : '차트', alignment: 'left' },
                        legendGroup: {layout:{position: 'right'}},
                        plotArea : {
                            drawingEffect: 'glowsy',
                            dataLabel: { visible: true },
                            colorPalette: ['#FAED7D', '#FFB2D9', '#FFC19E', '#990085']
                        }
                      }">
            <viz:dataset>
                <viz.data:FlattenedDataset data="{view>/list}">
                <!-- X축 -->
                    <viz.data:dimensions>
                        <viz.data:DimensionDefinition name="이름" value="{view>name}" />
                    </viz.data:dimensions>
                <!-- Y축 -->
                    <viz.data:measures>
                        <viz.data:MeasureDefinition name="숫자" value="{view>rate}" />
                    </viz.data:measures>
                </viz.data:FlattenedDataset>
            </viz:dataset>

            <!-- 화면에 그려지는 feeditem 추가 -->
            <viz:feeds>
            <!-- 보통 막대차트나 라인차트 같은 차트는 -> CategoryAxis/valueAxis -->
            <!-- 도넛차트나 파이차티 같은 경우 -> color/size -->
                <viz.feeds:FeedItem uid="color" type="Dimension" values="이름" />
                <viz.feeds:FeedItem uid="size" type="Measure" values="숫자" />
            </viz:feeds>
        </viz:VizFrame>

        <viz:VizFrame id="idControllerChart" vizType="column">
        </viz:VizFrame>
    </Page>
</mvc:View>
```
