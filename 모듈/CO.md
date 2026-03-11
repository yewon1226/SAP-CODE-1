## <  핵심 개념 >
</br>

- **1) **
</br>

---

</br>

## < SAP CO (Managerial Accounting) 개요 및 조직 구조 >
</br>

### Managerial Accounting (CO) Overview
- 원가요소관리, 간접비관리, 원가관리, 손익관리 등 기업 내부 원가와 수익성을 관리하는 관리회계 모듈
- 제조기업에서 주로 사용하며 제조가 없는 기업은 CO를 사용하지 않거나 일부 기능만 사용
</br>

- ① 원가요소 회계 → 비용을 1차원가 / 2차원가로 분류
- ② 간접비 관리 → 코스트센터·활동·내부오더로 비용 관리
- ③ 원가관리 → BOM·Routing 기반 제품 원가 계산
- ④ 수익성 분석 → 고객·제품 기준 매출·원가·이익 분석
<img width="709" height="287" alt="image" src="https://github.com/user-attachments/assets/337df55e-625b-4898-8d39-e2592434986e" />
</br>
</br>
</br>
</br>

### Controlling Organization Structure (CO 조직 구조)
- Operating Concern → 회사 전체 매출·이익 등 수익성을 통합적으로 분석하는 최상위 관리 단위(CO-PA)
- Controlling Area → 회사 내부 비용 관리와 원가 배부가 이루어지는 관리회계 영역이며 여러 Company Code를 함께 관리 가능
- Company Code → 재무회계(FI) 기준 조직으로 재무상태표(B/S)와 손익계산서(P/L)를 생성하는 단위
- Plant → 생산·구매·재고 관리가 이루어지는 물류 운영 단위로 하나의 Company Code 아래 여러 개 존재 가능
- Segment → 사업 영역별 재무 성과를 분석하기 위한 단위로 IFRS 등 재무 보고에 사용
- Profit Center → 부서·사업부 기준 수익성과 경영 성과를 분석하는 관리 단위
</br>

- Operating Concern → Controlling Area : 1:N 또는 1:1
- Controlling Area → Company Code : 1:N 또는 1:1
- Company Code → Plant : 일반적으로 1:N 구조
<img width="344" height="230" alt="image" src="https://github.com/user-attachments/assets/f09710ca-e41c-4051-99ba-4bd40fe83cd4" />
</br>
</br>
</br>
</br>

## 2

### 표준 계층 구조(Standard Hierarchy)와 Cost Center
- CO에서 코스트센터를 관리하기 위한 기본 조직 구조이며 코스트센터 생성 전에 먼저 만들어야 함
- Node(그룹) 아래에 코스트센터를 포함해 회사 → 부서 → 팀 → 코스트센터 형태로 계층적으로 관리
- Standard Hierarchy는 코스트센터를 묶어 관리하는 트리 구조이며 모든 Cost Center는 반드시 이 구조 안에서 생성됨
  
<img width="732" height="278" alt="image" src="https://github.com/user-attachments/assets/504e40f6-1e2d-47ad-9328-0dbee22c3828" />
</br>
</br>
</br>
</br>

### Cost Center Master Data
- 코스트센터 생성 시 Standard Hierarchy를 반드시 지정해야 하며, 회사코드, 사업영역, 기능영역 등 조직 정보와 책임자, 기간 등의 관리 정보가 함께 저장됨

<img width="1090" height="415" alt="image" src="https://github.com/user-attachments/assets/dc77eede-cacb-4751-90f7-230a313450e1" />
</br>
</br>
</br>
</br>

### Manage Cost Centers
- Manage Cost Centers 앱 → 코스트센터를 생성(Create), 변경(Change), 삭제(Delete) 하면서 관리하는 기능
- Master Data 관리 메뉴에서 코스트센터 관련 설정과 관리 작업 수행 가능
<img width="1048" height="388" alt="image" src="https://github.com/user-attachments/assets/12a97502-b9ce-4de1-a4e2-4537ccf0d73d" />
</br>
</br>
</br>
</br>

### Cost Center 생성 (Manage Cost Centers)
- Manage Cost Centers 앱에서 코스트센터를 생성하며, Controlling Area, Cost Center ID, 이름, 유효기간 등을 입력해 기본 정보를 설정
- Standard Hierarchy, Company Code, Profit Center 등 조직 정보를 함께 지정하여 코스트센터를 조직 구조에 연결함

<img width="1090" height="425" alt="image" src="https://github.com/user-attachments/assets/77335193-bf26-46a4-9066-9fe5726e8039" />
</br>
</br>
</br>
</br>

### Manage Cost Centers 기능
- Manage Cost Centers 앱에서 조회된 코스트센터 목록을 확인할 수 있으며, Create를 선택해 신규 코스트센터를 생성하거나 기존 코스트센터를 선택해 변경 가능
- 검색 및 필터 기능으로 회사코드, 코스트센터, 카테고리 등 기준으로 코스트센터를 조회·관리 가능
<img width="1114" height="439" alt="image" src="https://github.com/user-attachments/assets/c13de38e-551e-4eaf-a263-35b2f8497665" />
</br>
</br>
</br>
</br>

### Cost Center Category Configuration
- 컨피그에서 Cost Center Category를 정의하여 코스트센터의 유형(관리, 서비스, 생산 등)을 구분함
- 각 카테고리별로 실제원가, 계획원가 등 Control 설정과 기능영역(Function Area)도 사전에 지정 가능

<img width="750" height="376" alt="image" src="https://github.com/user-attachments/assets/ebf27dc8-e232-49f0-8331-b020e51c3c69" />
</br>
</br>
</br>
</br>

### G/L Account Type – Cost Element Category
- G/L Account에서 Cost Element Category를 설정하여 비용 계정을 구분함 (예: Primary Cost, Secondary Cost 등)
- 손익계정(P&L)은 원가·수익 계정으로 CO와 연계되며, 대차대조표 계정(Balance Sheet)은 현금, 자산 등 재무계정으로 사용됨
<img width="1090" height="379" alt="image" src="https://github.com/user-attachments/assets/9f9808ca-7d6c-44e7-8a1a-4fd6b8e62877" />
</br>
</br>
</br>
</br>

### Manage G/L Account Master Data
- S/4HANA에서는 Manage G/L Account Master Data에서 G/L 계정을 조회·관리하며, 1차 원가요소(Primary)와 2차 원가요소(Secondary)를 확인 가능
- G/L Account의 Cost Element Category 설정을 통해 CO에서 사용할 원가 유형을 구분함

<img width="1090" height="428" alt="image" src="https://github.com/user-attachments/assets/4aefa6bd-d54e-4d73-8f27-90802a18d38f" />
</br>
</br>
</br>
</br>

### Activity Type (액티비티 타입)
- Activity Type → 작업시간, 기계가동시간 등 작업 활동량을 측정하는 단위이며 생산오더나 작업을 통해 코스트센터에 비용이 집계됨
- 주로 Secondary Cost 계정과 연결되어, 작업센터(Work Center)의 활동량을 기준으로 원가를 배부할 때 사용됨

<img width="435" height="386" alt="image" src="https://github.com/user-attachments/assets/abfe37d2-14fc-4a0a-be73-1162dbccf3ca" />
</br>
</br>
</br>
</br>

### Manage Activity Types
- Manage Activity Types 앱에서 Activity Type을 생성(Create)하거나 조회 후 변경 가능하며 작업시간·기계시간 등 활동 단위를 관리함
- Controlling Area, Activity Unit, Allocation Cost Element 등을 설정하여 코스트센터 활동 배부 기준을 관리함

<img width="1090" height="439" alt="image" src="https://github.com/user-attachments/assets/7d8e43dc-00a7-4b20-836b-875f4d0602f5" />
</br>
</br>

<img width="1090" height="451" alt="image" src="https://github.com/user-attachments/assets/bad1f9a2-9f46-4da8-be65-78658bef4c0f" />
</br>
</br>
</br>
</br>

### Statistical Key Figure (통계 주요지표)
- Statistical Key Figure(SKF) → Assessment나 Distribution 수행 시 원가 배부 기준으로 사용하는 통계 지표 (예: 인원수, 매출액)
- Fixed Value → 한 번 입력하면 이후 기간에도 같은 값이 계속 적용되는 고정값 (예: 인원수)
- Totals Value → 입력한 기간에만 값이 기록되는 변동값 (예: 매출액)
  
<img width="553" height="116" alt="image" src="https://github.com/user-attachments/assets/12f4ee1f-6df1-40c7-9877-553b2f556dfd" />
</br>
</br>
</br>
</br>

### Manage Statistical Key Figures
- Manage Statistical Key Figures 앱에서 SKF를 생성(Create), 조회, 변경 가능하며 인원수·매출 등 원가 배부 기준 데이터를 관리
- Controlling Area, 단위(Quantity Unit), Category(Fixed / Totals Value)를 설정하여 통계 지표를 관리함

<img width="1090" height="484" alt="image" src="https://github.com/user-attachments/assets/9c50b9e5-7679-4d18-8f65-9c2e83af5aaa" />
</br>
</br>
</br>
</br>

### Manage Cost Center Groups
- Manage Cost Center Groups 앱에서 코스트센터 그룹을 생성, 변경, 삭제 가능하며 여러 코스트센터를 하나의 그룹으로 묶어 관리
- Standard Hierarchy를 포함한 계층 구조로 그룹을 구성하여 코스트센터를 조직 단위별로 관리함

<img width="944" height="410" alt="image" src="https://github.com/user-attachments/assets/f5ad9d5a-cbd4-403f-beeb-0dd822b1b9f1" />
</br>
</br>

<img width="1078" height="377" alt="image" src="https://github.com/user-attachments/assets/a3ddcf32-9042-4811-8747-e4625b8a56cf" />
</br>
</br>
</br>
</br>

### Entering Primary Costs in CO
- Primary Cost는 FI에서 발생한 비용이 코스트센터로 전달되어 CO에 기록되는 비용 (예: 전표, 송장 등)
- FI나 MM 등의 업무 트랜잭션이 FI 문서를 생성하며, 해당 데이터는 ACDOCA 테이블에 저장되어 CO 원가 전표로도 사용됨
  
<img width="405" height="296" alt="image" src="https://github.com/user-attachments/assets/b2491bed-1c45-410a-896b-9668329a3e45" />
</br>
</br>
</br>
</br>

### Real vs Statistical Postings in CO
- Real Posting → 실제로 원가가 반영되는 객체로 비용이 직접 집계됨 (예: Cost Center, Order)
- Statistical Posting → 실제 원가는 반영되지 않고 참고용으로만 기록되는 정보 (예: Profit Center 등)

<img width="670" height="337" alt="image" src="https://github.com/user-attachments/assets/0972e5c1-6644-4bdf-91e6-02a8606d102c" />
</br>
</br>
</br>
</br>

### Cost Centers Actuals
- Cost Centers Actuals 앱에서 코스트센터의 실제 원가(Actual Cost) 실적을 조회 가능
- 기간, 회사코드, 코스트센터, G/L 계정 등의 조건으로 비용 발생 현황을 분석함

<img width="1090" height="426" alt="image" src="https://github.com/user-attachments/assets/74cee03e-aa73-4541-8648-2bdcd870bb66" />
</br>
</br>
</br>
</br>

### Cost Centers Actuals 조회 기능
- Cost Centers Actuals 화면에서 코스트센터 실제 비용을 조회하며, 기간별·계정별 원가 실적을 확인 가능
- 필드 리스트에서 원하는 항목을 추가하여 분석 화면을 구성하고, 여러 기간의 금액을 동시에 비교 조회 가능

<img width="1090" height="485" alt="image" src="https://github.com/user-attachments/assets/062cb1e0-30b8-476a-925f-69f1a0ebe722" />
</br>
</br>
</br>
</br>

### 간접비 관리 – 코스트센터 비용 조회
- 재무 전표 입력 시 비용 계정에 코스트센터를 지정하여 간접비를 기록함
- 코스트센터 기준으로 발생한 비용을 집계하여 비용 리포트에서 조회 가능
<img width="1026" height="489" alt="image" src="https://github.com/user-attachments/assets/9b8eeee7-38c5-45a8-b4c4-19154449855a" />
</br>
</br>
</br>
</br>

### 간접비 관리 – 코스트센터 비용 리포트
- 코스트센터 비용 리포트에서 계획(Plan)과 실제(Actual) 비용을 비교 조회 가능
- 필터 조건을 설정하여 코스트센터·기간·계정 기준으로 비용 차이를 분석함
<img width="1014" height="491" alt="image" src="https://github.com/user-attachments/assets/aba8310e-0704-4b8f-8b4b-e109da09ab8f" />
</br>
</br>
</br>
</br>

### Manage Postings – Validation / Substitution
- Validation → 전표 입력 시 G/L 계정이나 코스트센터 등의 값이 정해진 규칙에 맞는지 검증하는 기능
- Substitution → 전표 입력 시 조건에 따라 코스트센터 등 입력값을 자동으로 다른 값으로 변경하는 기능

<img width="409" height="313" alt="image" src="https://github.com/user-attachments/assets/e7c27042-5311-4e56-9049-2732c401b97b" />
</br>
</br>
</br>
</br>

### Reassign Costs and Revenues
- Reassign Costs and Revenues 기능에서 비용이나 수익을 다른 코스트센터나 객체로 재배부 가능
- 엑셀 업로드 등을 통해 여러 항목을 한 번에 재전기하여 비용·수익 배분을 조정함
<img width="1038" height="362" alt="image" src="https://github.com/user-attachments/assets/5af378e6-6526-439c-867f-aed4b41165ed" />
</br>
</br>
</br>
</br>

### Repost Line Items
- Repost Line Items → 이미 기록된 비용을 한 코스트센터(Sender)에서 다른 코스트센터(Receiver)로 재배부하는 기능
- 재배부 시 원가 항목은 그대로 유지되며, CO 문서가 생성되고 기존 FI 문서를 참조하여 기록됨

<img width="449" height="263" alt="image" src="https://github.com/user-attachments/assets/6861f3d3-66ed-4131-b458-7eaecf01a6a0" />
</br>
</br>
</br>
</br>

### Reassign Costs and Revenues 입력
- Sender Cost Center → Receiver Cost Center로 비용 또는 수익을 재배부하도록 항목을 입력함
- 엑셀·CSV 업로드 기능을 통해 여러 재전기 항목을 한 번에 등록 가능

<img width="996" height="475" alt="image" src="https://github.com/user-attachments/assets/95295654-56ae-4e66-8c23-272c7314e9fa" />
</br>
</br>
</br>
</br>

### 배부 기준 입력 (SKF vs 배부율)
- 통계 주요지표(SKF)를 입력하여 비용 배부의 기준 값으로 사용 (예: 인원수, 전화 사용량 등)
- 또는 배부율을 직접 지정하여 각 코스트센터에 퍼센트나 금액 기준으로 비용을 배분 가능

<img width="974" height="508" alt="image" src="https://github.com/user-attachments/assets/c4032e5f-eb80-4c92-936a-08701b89051d" />
</br>
</br>
</br>
</br>

### Distribution & Allocation Cycle
- Distribution → Sender 코스트센터의 비용을 같은 Cost Element로 Receiver 코스트센터에 배부하는 기능 (Primary cost만 배부)
- 배부 규칙은 Cycle과 Segment로 설정하며, 배부 기준은 SKF 등 Receiver 추적요인으로 지정함
<img width="542" height="393" alt="image" src="https://github.com/user-attachments/assets/50c37507-8419-4629-8060-2856aa1bcba9" />

</br>
</br>
</br>
</br>

### Allocation Structure
- Allocation Structure → Assessment 배부 시 어떤 비용(Cost Element)을 어떤 Assessment Cost Element로 배부할지 정의하는 구조
- Configuration에서 Cost Element 그룹을 설정하고, 이를 Assignment에 지정하여 배부 규칙을 구성함

<img width="976" height="481" alt="image" src="https://github.com/user-attachments/assets/6120b5fa-1cee-4875-890d-018a936d6b7b" />
</br>
</br>
</br>
</br>

### Assessment 수행 예시
- T-Code KSU5에서 Assessment를 실행하여 Sender 코스트센터 비용을 Receiver로 배부함
- Allocation Structure에 지정된 Assessment Cost Element를 사용하여 비용이 재배부됨

<img width="1000" height="361" alt="image" src="https://github.com/user-attachments/assets/d9dbcd58-9f32-43db-a7c3-51be7dd5bba4" />

</br>
</br>
</br>
</br>

### Manage Allocation
- Manage Allocation 앱에서 Distribution, Top-down, Assessment 등의 배부 Cycle을 실행하고 관리 가능
- Cycle과 Segment를 설정하여 Sender·Receiver와 배부 기준(SKF, 금액 등)을 정의해 비용 배부를 수행함

<img width="988" height="544" alt="image" src="https://github.com/user-attachments/assets/7fab8ec3-5eaa-4390-a364-5d4ca2bd95d5" />

</br>
</br>

- Manage Allocation에서 배부 Cycle 데이터를 엑셀 템플릿으로 다운로드 후 수정하여 업로드 가능
- 엑셀 업로드를 통해 Sender·Receiver 및 배부 기준 데이터를 한 번에 등록하거나 수정 가능
<img width="1027" height="566" alt="image" src="https://github.com/user-attachments/assets/725bbb1b-82bf-4bcf-a494-aba5fcbf07c7" />
</br>
</br>

- Allocation Result 화면에서 배부 실행 결과를 한 번에 조회 가능
- 배부 Cycle 실행 이력, 상태, 실행시간 등을 확인하여 배부 결과를 분석함
<img width="1053" height="456" alt="image" src="https://github.com/user-attachments/assets/fcdb21c8-9d48-4062-8251-4b7a566043ce" />
