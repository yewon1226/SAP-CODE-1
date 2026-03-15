# SAP Standard Table

</br>

## SAP FI / CO 주요 Standard Tables

### FI (Financial Accounting)

| Table | Description |
|------|-------------|
| BKPF | 회계 전표 헤더 데이터 (전표번호, 전표유형, 회사코드, 전표일자 등 전표 기본정보 저장) |
| BSEG | 회계 전표 라인 아이템 (계정, 금액, 고객/공급업체 등 전표 상세 거래 데이터 저장) |
| BSET | 전표 세금(Tax) 데이터 (전표에 적용된 세금 코드 및 세금 금액 정보 저장) |
| BSEC | 전표 Business Partner 데이터 (전표에 입력된 고객/공급업체의 추가 파트너 정보 저장) |
| BSIS | 미결 G/L 계정 항목 (Clearing되지 않은 G/L 계정 거래 내역 관리) |
| BSAS | 정산된 G/L 계정 항목 (Clearing 완료된 G/L 계정 거래 내역 관리) |
| BSIK | 미결 매입채무(AP) 항목 (지급되지 않은 공급업체 채무 관리) |
| BSAK | 정산된 매입채무(AP) 항목 (지급 완료된 공급업체 채무 관리) |
| BSID | 미결 매출채권(AR) 항목 (입금되지 않은 고객 채권 관리) |
| BSAD | 정산된 매출채권(AR) 항목 (입금 완료된 고객 채권 관리) |
| FAGLFLEXA | New G/L Line Item 데이터 (New GL 환경에서 회계 전표 라인 아이템 데이터 저장) |
| FAGLFLEXT | New G/L 합계 데이터 (New GL 환경에서 계정별 집계 데이터 저장) |
| ACDOCA | Universal Journal (S/4HANA 통합 회계 Line Item 데이터) |
| SKA1 | G/L 계정 마스터 (Chart of Accounts 기준 G/L 계정 기본정보 관리) |
| SKB1 | G/L 계정 마스터 (Company Code 기준 G/L 계정 설정 정보 관리) |
| KNA1 | 고객 마스터 일반 데이터 (고객 이름, 주소, 국가 등 기본정보 관리) |
| LFA1 | 공급업체 마스터 일반 데이터 (공급업체 이름, 주소, 국가 등 기본정보 관리) |
| T001 | Company Code 정보 (회사코드 기본 설정 및 재무 관련 조직 정보 저장) |
| T001W | Plant 정보 (플랜트 코드와 플랜트 이름 등 조직 정보 저장) |

</br>
</br>

---

</br>

### CO (Controlling)

| Table | Description |
|------|-------------|
| CSKA | 원가요소 마스터 (비용 계정을 원가요소로 정의하여 CO에서 사용) |
| CSKB | 원가요소 회사코드 데이터 (회사코드별 원가요소 설정 정보 저장) |
| CSKS | 코스트센터 마스터 (부서 또는 비용 발생 조직 단위 관리) |
| CSKT | 코스트센터 텍스트 (코스트센터 설명 텍스트 관리) |
| COSS | 코스트센터 합계 데이터 (코스트센터별 비용 집계 데이터 저장) |
| COSP | 코스트센터 계획 데이터 (코스트센터 계획 비용 관리) |
| COEP | CO Line Item 데이터 (코스트센터, 내부오더 등 CO 거래 상세 데이터 저장) |
| COBK | CO 문서 헤더 (CO 전표의 헤더 정보 관리) |
| AUFK | 내부오더 마스터 (Internal Order 기본정보 관리) |
| CE1XXXX | Profitability Analysis 실제 데이터 (CO-PA 실제 수익성 분석 데이터 저장) |
| CE2XXXX | Profitability Analysis 계획 데이터 (CO-PA 계획 수익성 분석 데이터 저장) |

</br>
</br>

---

</br>

## SAP MM 주요 Standard Tables

### Material Master

| Table | Description |
|------|-------------|
| MARA | 자재 마스터 기본 데이터 (자재 번호, 자재 유형 등 전체 공통 정보 관리) |
| MARC | 플랜트별 자재 데이터 (플랜트 단위 자재 관리 정보 저장) |
| MARD | 저장위치별 재고 데이터 (저장위치 기준 자재 재고 수량 관리) |
| MBEW | 자재 평가 데이터 (자재 가격 및 재고 평가 관련 정보 관리) |
| MAKT | 자재 설명 텍스트 (자재명 및 자재 설명 관리) |
| MARM | 자재 단위 변환 데이터 (자재 단위 변환 정보 관리) |

### Vendor Master

| Table | Description |
|------|-------------|
| LFA1 | 공급업체 일반 데이터 (공급업체 기본 정보 관리) |
| LFB1 | 회사코드별 공급업체 데이터 (회사코드 기준 회계 관련 공급업체 정보 관리) |
| LFM1 | 구매조직별 공급업체 데이터 (구매조직 기준 공급업체 구매 정보 관리) |
| BUT000 | Business Partner 기본 데이터 (BP 번호, 유형, 이름 등 기본 정보 저장) |
| BUT020 | Business Partner 주소 데이터 |
| BUT100 | Business Partner Role 데이터 (Customer, Vendor 등 BP 역할 관리) |

### Purchasing (구매)

| Table | Description |
|------|-------------|
| EKKO | 구매오더 헤더 (PO 전체 기본정보 관리) |
| EKPO | 구매오더 아이템 (PO 자재 및 수량 등 상세 정보 관리) |
| EKET | 구매오더 스케줄라인 (납기일 및 납품 일정 관리) |
| EKBE | 구매오더 이력 (입고 및 송장 처리 이력 관리) |
| EBAN | 구매요청(PR) 데이터 (구매요청 생성 정보 관리) |
| EINA | 구매정보레코드 일반 데이터 (자재-공급업체 기본 구매정보 관리) |
| EINE | 구매정보레코드 구매조직 데이터 (구매조직 기준 자재-공급업체 구매조건 관리) |

### Invoice (송장)
| RBKP | 송장 문서 헤더 (Invoice Verification 문서 기본 정보) |
| RSEG | 송장 문서 아이템 (송장 자재 및 금액 정보 관리) |

### Inventory Management (재고)

| Table | Description |
|------|-------------|
| MATDOC | S/4HANA 자재 문서 통합 테이블 |
| MARD | 저장위치 재고 (저장위치 기준 재고 수량 관리) |
| MCHA | Batch Master 데이터 (Batch 기본 정보 관리) |
| MCHB | Batch 재고 (Batch 단위 자재 재고 관리) |

### Logistics / Plant

| Table | Description |
|------|-------------|
| T001W | 플랜트 정보 (플랜트 조직 정보 관리) |
| T001L | 저장위치 정보 (저장위치 조직 정보 관리) |

</br>
</br>

---

</br>

## SAP PP 주요 Standard Tables

### Production Order

| Table | Description |
|------|-------------|
| AFKO | 생산오더 헤더 (생산오더 기본 정보 관리) |
| AFPO | 생산오더 Item (생산할 자재 및 수량 정보 관리) |
| AFVC | 공정(Operation) 데이터 (생산 공정 단계 정보 관리) |
| AFVV | Operation 상세 데이터 |
| AUFK | 오더 마스터 데이터 (생산오더 및 내부오더 공통 정보 관리) |

### BOM (Bill of Material)

| Table | Description |
|------|-------------|
| STKO | BOM 헤더 (BOM 기본 정보 관리) |
| STPO | BOM 아이템 (BOM 구성 자재 정보 관리) |
| MAST | 자재와 BOM 연결 정보 |

### Routing

| Table | Description |
|------|-------------|
| PLKO | Routing 헤더 (공정 Routing 기본 정보 관리) |
| PLPO | Routing Operation (공정 작업 단계 정보 관리) |
| MAPL | 자재와 Routing 연결 정보 |

### MRP / Planning

| Table | Description |
|------|-------------|
| MDKP | MRP Document Header (MRP 계획 문서 헤더 관리) |
| MDTB | MRP Document Item (MRP 계획 상세 데이터 관리) |

### Production Execution

| Table | Description |
|------|-------------|
| AFRU | 생산 실적 Confirm 데이터 (생산 작업 실적 입력 데이터 관리) |
| CRHD | Work Center 마스터 (작업장 및 생산 설비 정보 관리) |
| CRCA | Work Center Capacity 데이터 |

</br>
</br>

---

</br>

## SAP SD 주요 Standard Tables

### Sales Document (판매 문서)

| Table | Description |
|------|-------------|
| VBAK | Sales Order Header (판매오더 기본 정보 관리) |
| VBAP | Sales Order Item (판매오더 자재 및 수량 정보 관리) |
| VBEP | Sales Order Schedule Line (판매 납기 일정 관리) |
| VBUK | Sales Document Header Status (판매문서 전체 상태 정보 관리) |
| VBUP | Sales Document Item Status (판매문서 아이템 상태 관리) |
| VBFA | Sales Document Flow (판매문서 간 흐름 관계 관리) |
| VBPA | Sales Partner 데이터 |
| VBKD | Sales Document Business Data |
| KONV | Pricing 조건 결과 데이터 |
| KONH | Pricing 조건 헤더 |
| KONP | Pricing 조건 상세 |

### Delivery (출고 / 배송)

| Table | Description |
|------|-------------|
| LIKP | Delivery Header (출고 문서 기본 정보 관리) |
| LIPS | Delivery Item (출고 자재 및 수량 정보 관리) |

### Billing (청구)

| Table | Description |
|------|-------------|
| VBRK | Billing Header (청구 문서 기본 정보 관리) |
| VBRP | Billing Item (청구 자재 및 금액 정보 관리) |

### Customer Master (고객 마스터)

| Table | Description |
|------|-------------|
| KNA1 | 고객 마스터 일반 데이터 (고객 기본 정보 관리) |
| KNB1 | 회사코드 고객 데이터 (회계 기준 고객 정보 관리) |
| KNVV | 판매영역 고객 데이터 (판매조직 기준 고객 판매 정보 관리) |
| BUT000 | Business Partner 기본 데이터 (BP 번호, 유형, 이름 등 기본 정보 저장) |
| BUT020 | Business Partner 주소 데이터 |
| BUT100 | Business Partner Role 데이터 (Customer, Vendor 등 BP 역할 관리) |




