
# SAP Standard Table

</br>

## SAP FI / CO 주요 Standard Tables & T-Codes

### FI (Financial Accounting)

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| BKPF | 회계 전표 헤더 데이터 | FB03, FB01 |
| BSEG | 회계 전표 라인 아이템 | FB03 |
| BSIS | 미결 G/L 계정 항목 | FBL3N |
| BSAS | 정산된 G/L 계정 항목 | FBL3N |
| BSIK | 미결 매입채무(AP) 항목 | FBL1N |
| BSAK | 정산된 매입채무(AP) 항목 | FBL1N |
| BSID | 미결 매출채권(AR) 항목 | FBL5N |
| BSAD | 정산된 매출채권(AR) 항목 | FBL5N |
| SKA1 | G/L 계정 마스터 (Chart of Accounts) | FS00 |
| SKB1 | G/L 계정 마스터 (Company Code) | FS00 |
| KNA1 | 고객 마스터 일반 데이터 | XD03 |
| LFA1 | 공급업체 마스터 일반 데이터 | XK03 |
| T001 | Company Code 정보 | OX02 |
| T001W | Plant 정보 | OX10 |

### CO (Controlling)

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| CSKS | 코스트센터 마스터 | KS03 |
| CSKT | 코스트센터 텍스트 | KS03 |
| COSS | 코스트센터 합계 데이터 | KSB1 |
| COEP | CO Line Item 데이터 | KSB1 |
| COBK | CO 문서 헤더 | KSB1 |
| AUFK | 내부오더 마스터 | KO03 |
| AFKO | 생산오더 헤더 | CO03 |
| AFVC | 작업 단계 정보 | CO03 |
| CE1XXXX | Profitability Analysis 실제 데이터 | KE24 |
| CE2XXXX | Profitability Analysis 계획 데이터 | KE25 |


### FI/CO 공통 조회 T-Code

| T-Code | Description |
|------|-------------|
| FB03 | 회계 전표 조회 |
| FBL1N | 공급업체 계정 조회 |
| FBL5N | 고객 계정 조회 |
| FBL3N | G/L 계정 조회 |
| KSB1 | 코스트센터 라인아이템 조회 |
| KE24 | 수익성 분석 조회 |

</br>

## SAP MM 주요 Standard Tables & T-Codes

### Material Master

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| MARA | 자재 마스터 기본 데이터 | MM03 |
| MARC | 플랜트별 자재 데이터 | MM03 |
| MARD | 저장위치별 재고 데이터 | MM03 |
| MBEW | 자재 평가(가격) 데이터 | MM03 |
| MAKT | 자재 설명 텍스트 | MM03 |

### Vendor Master

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| LFA1 | 공급업체 일반 데이터 | XK03 |
| LFB1 | 회사코드별 공급업체 데이터 | XK03 |
| LFM1 | 구매조직별 공급업체 데이터 | XK03 |

### Purchasing (구매)

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| EKKO | 구매오더 헤더 | ME23N |
| EKPO | 구매오더 아이템 | ME23N |
| EKET | 구매오더 스케줄라인 | ME23N |
| EKBE | 구매오더 이력 (GR/Invoice) | ME23N |
| EBAN | 구매요청(PR) | ME53N |
| EBKN | PR Account Assignment | ME53N |

### Inventory Management (재고)

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| MKPF | 자재문서 헤더 | MB03 |
| MSEG | 자재문서 아이템 | MB03 |
| MARD | 저장위치 재고 | MM03 |
| MCHB | Batch 재고 | MSC3N |

### Valuation / Accounting

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| MBEW | 자재 평가 데이터 | CKM3 |
| CKMLCR | 실제 원가 데이터 | CKM3 |

### Logistics / Plant

| Table | Description | 주요 T-Code |
|------|-------------|-------------|
| T001W | 플랜트 정보 | OX10 |
| T001L | 저장위치 정보 | OX09 |

</br>

## SAP PP 주요 T-Code (Production Planning)

### 생산오더 (Production Order)

| T-Code | Description |
|------|-------------|
| CO01 | 생산오더 생성 |
| CO02 | 생산오더 변경 |
| CO03 | 생산오더 조회 |
| COHV | 생산오더 일괄 처리 |
| COHVPI | 프로세스 오더 일괄 처리 |

### MRP (Material Requirement Planning)

| T-Code | Description |
|------|-------------|
| MD01 | 전체 MRP 실행 |
| MD02 | 개별 자재 MRP 실행 |
| MD03 | 단일 자재 MRP 실행 |
| MD04 | 자재 재고/소요 조회 |
| MD05 | MRP 리스트 조회 |
| MD06 | MRP 예외 메시지 조회 |

### BOM (Bill of Material)

| T-Code | Description |
|------|-------------|
| CS01 | BOM 생성 |
| CS02 | BOM 변경 |
| CS03 | BOM 조회 |
| CS11 | BOM 구조 조회 |
| CS12 | BOM 다단계 조회 |

### Routing (공정 관리)

| T-Code | Description |
|------|-------------|
| CA01 | Routing 생성 |
| CA02 | Routing 변경 |
| CA03 | Routing 조회 |
| CA60 | Routing 리스트 조회 |

### 생산 계획 / 수요 계획

| T-Code | Description |
|------|-------------|
| MD61 | 독립 수요(PIR) 생성 |
| MD62 | 독립 수요(PIR) 변경 |
| MD63 | 독립 수요(PIR) 조회 |
| MD74 | PIR 삭제 |
| MD75 | PIR 일괄 삭제 |

### 재고 및 생산 관련 조회

| T-Code | Description |
|------|-------------|
| MB52 | 자재 재고 조회 |
| MB51 | 자재 문서 조회 |
| COOIS | 생산오더 정보 조회 |
| MF50 | 생산 계획 테이블 |

</br>

## SAP SD 주요 T-Code (Sales & Distribution)

### Sales Document (판매 문서)

| T-Code | Description |
|------|-------------|
| VA01 | 판매오더 생성 |
| VA02 | 판매오더 변경 |
| VA03 | 판매오더 조회 |
| VA05 | 판매오더 리스트 조회 |
| VA21 | 견적 생성 |
| VA22 | 견적 변경 |
| VA23 | 견적 조회 |
| VA11 | Inquiry 생성 |
| VA12 | Inquiry 변경 |
| VA13 | Inquiry 조회 |

### Delivery (출고 / 배송)

| T-Code | Description |
|------|-------------|
| VL01N | 납품 생성 |
| VL02N | 납품 변경 |
| VL03N | 납품 조회 |
| VL06O | 출고 모니터링 |
| LT03 | 피킹 처리 |

### Billing (청구)

| T-Code | Description |
|------|-------------|
| VF01 | 청구 문서 생성 |
| VF02 | 청구 문서 변경 |
| VF03 | 청구 문서 조회 |
| VF04 | 청구 대기 리스트 |
| VF05 | 청구 문서 리스트 조회 |

### Customer Master (고객 마스터)

| T-Code | Description |
|------|-------------|
| XD01 | 고객 생성 |
| XD02 | 고객 변경 |
| XD03 | 고객 조회 |
| VD01 | 판매영역 고객 생성 |
| VD02 | 판매영역 고객 변경 |
| VD03 | 판매영역 고객 조회 |

### Pricing (가격 관리)

| T-Code | Description |
|------|-------------|
| VK11 | 가격 조건 생성 |
| VK12 | 가격 조건 변경 |
| VK13 | 가격 조건 조회 |

### Shipping / Logistics 조회

| T-Code | Description |
|------|-------------|
| VLO1N | 출하 생성 |
| VLPOD | POD 처리 |
| VT01N | 운송 생성 |
| VT02N | 운송 변경 |
| VT03N | 운송 조회 |
