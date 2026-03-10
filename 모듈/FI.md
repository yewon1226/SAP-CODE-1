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

  - 기업 운영에 필요한 기본 데이터
  - 시간이 지나도 거의 변하지 않는 데이터
  - 예: 고객 정보, 제품 정보, 직원 정보, 공급업체 정보
#### - Transaction Data (거래 데이터)

  - 일상 업무 처리 과정에서 계속 생성되는 데이터
  - 업무 활동에 따라 자주 변경됨
  - 예: 판매 주문, 송장(Invoice), 결제(Payment), 배송(Shipment)
