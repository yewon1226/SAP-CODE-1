## <  핵심 개념 >
</br>

- **1) 진행 개요 및 솔루션 Overview**
- **2) 모듈 프로세스 Overview 및 조직 구조**
- **3) 마스터 데이터 및 트랜잭션 데이터 개념**
- **4) FI 기본 개념, FI/CO 흐름 및 FI/CO 와 연관관계, FI 마스터**
- **5) 재무제표 개념 및 실습, 마스터 관리 분개 생성 실습**
- **6) 총계정원장 개요, G/L 계정 마스터, FI 영역 관리 범위**
- **7) AP 프로세스, Business Partner 개념 및 개요, BP 생성 및 전표 생성 실습**
- **8) AP 지급 프로세스, AR 프로세스 개요, AA 프로세스 개요, 자산 마스터 생성 및 전표 생성 실습**
</br>

---

</br>

## < ABAP 개념 및 문법 >
</br>

### SAP 회계 구조 변화
- R/3는 회계 데이터가 여러 모듈에 분산되어 있었고, S/4HANA에서는 Universal Journal(ACDOCA) 하나로 통합됨
<img width="481" height="344" alt="image" src="https://github.com/user-attachments/assets/d8f1b55f-994d-4ce1-84b2-a0c80c940f3f" />
</br>
</br>
</br>
</br>

### Finance / Financial Accounting 역할

| 구분 | 내용 |
|---|---|
| 주요 역할 | 재무 거래 기록, 분석, 보고 및 법적 기준에 따른 재무 성과 관리 |
| 연계 부서 | Customers(매출채권 관리), Sales(신용위험·결제문제 확인), Procurement(매입 송장 및 공급업체 지급 분석) |
| 주요 산출물 | 재무제표(재무상태표, 손익계산서, 현금흐름표), 비즈니스 데이터(AP, AR, 고정자산 등) |
| 핵심 성공 요소 | 재무 거래 정확한 분석, 거래처 신용 및 지급 관리, 규정 준수 및 리스크 관리 |
</br>
</br>
</br>

### SAP 비즈니스 프로세스 통합 구조
- 기업 부서(재무, 인사, 영업, 생산, 구매 등)는 서로 연결된 End-to-End 프로세스로 운영됨
- SAP S/4HANA(ERP)가 중심이 되어 모든 업무 프로세스를 하나의 시스템에서 통합 관리
</br>

- Record to Report (R2R) → 재무회계 처리 및 재무보고
- Recruit to Retire (R2R) → 채용부터 퇴직까지 인사관리
- Lead to Cash (L2C) → 고객 발굴 → 판매 → 대금 회수
- Design to Operate (D2O) → 제품 설계 → 생산 운영
- Source to Pay (S2P) → 구매 요청 → 공급업체 지급

<img width="638" height="374" alt="image" src="https://github.com/user-attachments/assets/b6988695-7cd1-4a51-a9a3-7cee93dccfeb" />
</br>
</br>
</br>
</br>

### Enterprise Structure 개념
- 회사는 생산공장, 법인, 물류센터, 창고 등 여러 조직으로 구성됨
- Bikes, Spare parts, Services 3개 사업 영역 운영
- 여러 지역과 조직의 비용·매출 데이터를 통합 관리해야 함

<img width="539" height="317" alt="image" src="https://github.com/user-attachments/assets/a4c267fd-fc8e-41e8-ba28-00fbda028be3" />
</br>
</br>
</br>
</br>

### Financial Accounting 요구사항
- 재무제표는 IFRS 기준으로 작성
- 회사는 12개의 글로벌 법인(legal entities) 존재
- 각 국가의 회계 기준(GAAP 등)도 함께 고려 필요
- IFRS 8 규정에 따라 사업 영역별 재무 지표 관리 필요
  - 사업 영역 : bikes, spare parts, service
</br>
</br>
</br>

### SAP Enterprise Structure 핵심
- Client → SAP 시스템 전체를 대표하는 최상위 조직
- Company Code → 재무회계가 관리되는 법인 단위
- Controlling Area / Operating Concern → 비용·수익 분석을 위한 관리회계 구조
- Sales Organization / Purchasing Organization → 영업·구매 조직
- Plant → Storage Location → Warehouse → 생산·재고 관리 구조

<img width="614" height="334" alt="image" src="https://github.com/user-attachments/assets/65f42898-1ef3-478f-8c62-be17b369fce8" />
</br>
</br>
</br>
</br>

### Bike Company ↔ SAP S/4HANA 매핑

| 기업 구조 | SAP S/4HANA | 의미 |
|---|---|---|
| 전체 회사 | 1 Client | SAP 시스템 전체를 구분하는 최상위 단위 |
| 재무 부서 | Financial Accounting (FI) | 재무회계 및 재무제표 관리 |
| 관리회계 부서 | Controlling (CO) | 비용·수익 및 내부 성과 관리 |
| 인사 부서 | Human Resource Management | 직원 및 인사 정보 관리 |
| 구매 부서 | Materials Management (MM) | 자재 구매 및 공급업체 관리 |
| 영업 및 서비스 부서 | Sales and Distribution (SD) | 제품 판매 및 고객 관리 |
| 생산 부서 | Production (PP) | 생산 계획 및 생산 관리 |

- 상세 구조 매핑

| 기업 구조 | SAP S/4HANA | 의미 |
|---|---|---|
| 전체 비용·수익 통합 관리 | 1 Controlling Area | 회사 전체 비용·수익을 통합 관리하는 단위 |
| 수익성 분석 구조 | 1 Operating Concern | 제품·사업 기준 수익성 분석 단위 |
| 12개 법인 | 12 Company Codes | 법인별 재무회계를 관리하는 단위 |
| 3개 사업 영역 | 3 Segments | 사업 분야별 재무 성과 분석 단위 |
| 12개 인사 조직 | 12 Personnel Areas | 조직별 인사 관리 단위 |
| 4개 생산 공장 | 4 Plants | 생산이 이루어지는 공장 단위 |
| 8개 물류센터 | 8 Distribution Centers | 제품 보관 및 배송을 담당하는 물류 단위 |
</br>
</br>
</br>

### Master Data vs Transaction Data
#### - Master Data (마스터 데이터)
- 기업 운영에 필요한 기본 데이터이며 시간이 지나도 거의 변하지 않음  
- Enterprise Structure 기준으로 관리되는 장기 유지 데이터
- 예: 고객 정보, 제품 정보, 직원 정보, 공급업체 정보

#### - Transaction Data (거래 데이터)
- 일상 업무 처리 과정에서 계속 생성되는 데이터  
- 업무 활동에 따라 자주 변경되며 Document 형태로 저장됨
- 예: 판매 주문, 송장(Invoice), 결제(Payment), 배송(Shipment)

#### - Document (문서)
- Transaction Data를 기록하는 문서 형태의 데이터  
- Company Code, Plant 등 Enterprise Structure에 연결되어 저장됨
- 예: Sales Order 생성, Vendor Invoice 전표 처리, Production Order 생성

</br>
</br>
</br>

### Financial Accounting 기본 구조
- 회계는 Financial Accounting(FI)와 Management Accounting(CO) 두 영역으로 구성됨
- Subledger에서 발생한 거래 → General Ledger에 반영 → 재무제표 생성
#### - General Ledger (G/L) 

  - 모든 회계 거래를 통합 관리하는 중심 계정
#### - Subledger (보조원장)
  
  - Accounts Receivable (AR) → 고객 매출채권 관리
  - Accounts Payable (AP) → 공급업체 매입채무 관리
  - Asset Accounting → 고정자산 관리
#### - 재무 계정 구조

  - Balance Sheet Accounts → 자산·부채 계정 (재무상태표)
  - Profit and Loss Accounts → 수익·비용 계정 (손익계산서)

<img width="550" height="282" alt="image" src="https://github.com/user-attachments/assets/48172040-d412-4640-b1a5-a315210885c3" />
</br>
</br>
</br>
</br>

### Management Accounting (CO) 기본 구조
- FI에서 발생한 비용·수익 데이터를 활용해 내부 경영 분석을 수행
- FI 전표 → 비용 분류 → 부서/제품 원가 분석 → 수익성 분석
</br>

1) FI에서 비용·수익 발생 → 매출, 비용, 급여, 자재비 등 전표 발생
2) Cost Element Accounting → 비용/수익을 관리회계용 비용요소로 분류
3) Overhead Cost Controlling → 비용을 부서(비용센터) 기준으로 관리
4) Product Cost Controlling → 제품별 원가 계산
5) Margin Analysis → 제품 / 고객 / 사업별 수익성 분석
   
<img width="605" height="338" alt="image" src="https://github.com/user-attachments/assets/d0ba690d-f325-4e9a-873c-7240ef5ca450" />
</br>
</br>
</br>
</br>

### G/L Accounting 주요 기능
- Chart of Accounts → G/L 계정을 일관된 기준으로 생성·관리하는 계정 체계
- Financial Statement Version (FSV) → 재무상태표와 손익계산서 구조를 정의하는 재무제표 구조
- General Journal Entry → 재무 거래를 기록하며 최소 두 계정(차변·대변)에 영향을 주는 분개 전표
</br>
</br>
</br>

### Operating Chart of Accounts (운영 계정표)
- Operating Chart of Accounts란 하나의 Chart of Accounts(계정 체계)를 여러 Company Code가 공통으로 사용하는 구조
<img width="516" height="197" alt="image" src="https://github.com/user-attachments/assets/068f1bfc-6601-4635-b63e-0e72d2af6dd6" />
</br>
</br>
</br>
</br>

### G/L Account 번호 구조
- G/L 계정은 번호 앞자리로 계정 종류(자산·부채·자본·수익·비용)를 구분함
- Balance Sheet Accounts (1~3xxxxxxx) → 자산·부채·자본 계정

  - 1xxxxxxx → 은행, 현금, 고정자산 등 자산 계정
  - 2xxxxxxx → 매입채무, 세금 등 부채 계정
  - 3xxxxxxx → 자본금 등 자본 계정
- Profit and Loss Accounts (4~7xxxxxxx) → 수익·비용 계정

  - 4xxxxxxx → 매출 계정
  - 5xxxxxxx → 재료비·재고 관련 비용 계정
  - 6xxxxxxx → 운영비(여비, 관리비 등) 계정
  - 7xxxxxxx → 이자·환차손익 등 기타 손익 계정
<img width="600" height="267" alt="image" src="https://github.com/user-attachments/assets/4ba12152-f0dc-4cf4-8f53-cee2ef33f544" />
</br>
</br>
</br>
</br>

### Financial Statement Version (FSV)
- FSV 역할 → 재무상태표(Balance Sheet)와 손익계산서(P&L) 구조를 정의함
- G/L 계정 관리 → 각 G/L 계정을 재무제표 항목(Item / Sub-item)에 매핑
- 재무제표 표시 → 연결된 G/L 계정 금액을 합계로 계산하여 재무제표에 표시

<img width="594" height="330" alt="image" src="https://github.com/user-attachments/assets/160333c9-f429-4b9c-b682-f012c36f0f8e" />
</br>
</br>
</br>
</br>

### General Journal Entry 화면 구성
- ① Journal Entry Date / Posting Date → 전표 작성일 / 회계 반영일
- ② Journal Entry Type → 전표 유형 (예: G/L 전표)
- ③ Company Code → 전표가 기록될 회사 코드
- ④ G/L Account → 금액을 기록할 계정 번호
- ⑤ Debit / Credit → 차변 / 대변 금액 입력
- ⑥ Line Item 추가/관리 → 전표 라인 추가 및 관리
- ⑦ Total Balance → 차변·대변 합계 확인 (0이어야 전표 등록 가능)
<img width="689" height="409" alt="image" src="https://github.com/user-attachments/assets/f9313fab-a746-4c96-9ef5-25e8cc3cceb0" />

</br>
</br>
</br>
</br>

### G/L Account Type
- G/L 계정은 유형(Account Type)에 따라 재무제표와 비용·수익 처리 방식이 달라짐
- Balance Sheet Account → 재무상태표에 표시되는 자산·부채·자본 계정
- Cash Account / Bank Reconciliation Account → 은행·현금 거래를 관리하는 계정
- Non-Operating Expense / Income → 회사 주요 영업과 직접 관련 없는 수익·비용 계정
- Primary Costs or Revenue → 회사 운영 활동에서 발생하는 주요 비용·수익 계정
</br>
</br>
</br>

### Primary Costs vs Secondary Costs 비교


| 구분 | Primary Costs or Revenue | Secondary Costs |
|-----|--------------------------|----------------|
| 거래 대상 | 외부 업체, 고객, 직원 등 | 사내 부서 간 (Cost Center, Order 등) |
| FI 연동 여부 | 연동됨 (G/L 계정이 곧 비용 요소) | 연동 안 됨 (CO 내부에서만 생성) |
| 예시 | 급여 지급, 사무용품 구입, 제품 매출 | IT 지원 서비스 배부, 공통비 배부 |
| 데이터 흐름 | 외부 → FI → CO | CO 내부 → CO 내부 |

</br>
</br>
</br>

### Business Partner : FI Vendor 
- Business Partner(BP) → 고객·공급업체 등의 마스터 데이터를 통합 관리하는 객체
- BP 데이터 구조

  - General Data → 클라이언트 전체에서 공통 사용 (주소, 연락처, 은행 정보 등)
  - Financial Accounting Data → 회사코드별 관리 (지급 방법, GL 연동, 세금 정보 등)
- Business Partner Role

  - BP에 역할(Role)을 부여하여 사용 목적을 구분
  - Customer → 고객 / Supplier → 공급업체 / FI Customer → 회계용 고객 / FI Vendor → 회계용 공급업체

<img width="604" height="272" alt="image" src="https://github.com/user-attachments/assets/a53009bf-f210-4147-8c05-cf7d7b92a82e" />
</br>
</br>
</br>
</br>

### 구매 프로세스 회계 흐름 (FI 자동 분개)
- 구매 프로세스 진행 시 단계별로 FI 전표가 자동 생성됨
  
<img width="906" height="264" alt="image" src="https://github.com/user-attachments/assets/5aa5a4bb-1b91-42cb-8556-2487e3d9d8f7" />
</br>
</br>
</br>
</br>

### Vendor Invoice 회계 흐름
- ① Vendor Invoice 입력 → 공급업체 송장 등록
- ② Accounts Payable 기록 → 매입채무(Reconciliation Account) 발생
- ③ Expense Account 반영 → 비용 계정(P&L)에 금액 기록 (예: 사무용품 비용)
- ④ CO Cost Center 반영 → 비용이 코스트센터로 전달되어 관리
  
<img width="665" height="365" alt="image" src="https://github.com/user-attachments/assets/15655bea-8f85-4036-bc0a-5657944c8fe9" />
</br>
</br>
</br>
</br>

### Accounts Receivable Value Flow
- ① Create Business Partner → 고객 BP(FI Customer) 생성 및 Reconciliation Account 설정
- ② Post Invoice → 매출 송장 발행 → 매출(P&L) + 매출채권(AR) 발생
- ③ Post Payment → 고객 결제 → 매출채권 정리(Clearing) 및 은행 입금
