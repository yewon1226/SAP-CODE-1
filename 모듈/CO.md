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
