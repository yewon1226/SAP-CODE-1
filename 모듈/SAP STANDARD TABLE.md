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
</br>
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
</br>
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
</br>
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

</br>
</br>
</br>

---

</br>

# SAP FI / CO 주요 Standard Tables
</br>

## < FI (Financial Accounting) >

### 1. BKPF (Accounting Document Header)
- 회계 전표의 헤더 정보(회사코드, 전표번호, 전기일자, 통화 등)를 저장하는 FI 핵심 테이블

```
전표번호 : 1900001234

라인1
계정 : 400000 (매출)
금액 : 150000
차변/대변 : S (대변)

라인2
계정 : 110000 (매출채권)
금액 : 150000
차변/대변 : H (차변)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| BELNR | CHAR(10) | 회계 전표 번호 | 1900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BLART | CHAR(2) | 전표 유형 | SA |
| BLDAT | DATS | 전표 날짜 | 20260301 |
| BUDAT | DATS | 전기 날짜 (Posting Date) | 20260301 |
| MONAT | NUMC(2) | 회계 기간 | 03 |
| CPUDT | DATS | 전표 입력 날짜 | 20260301 |
| CPUTM | TIMS | 전표 입력 시간 | 103000 |
| USNAM | CHAR(12) | 전표 생성 사용자 | SAPUSER |
| TCODE | CHAR(20) | 전표 생성 T-Code | FB01 |
| WAERS | CUKY | 문서 통화 | KRW |
| KURSF | DEC(9,5) | 환율 | 1.00000 |
| XBLNR | CHAR(16) | 참조 문서 번호 | INV20260301 |
| BKTXT | CHAR(25) | 전표 헤더 텍스트 | Sales Posting |
| STBLG | CHAR(10) | 역분개 전표 번호 | 1900005678 |
| STJAH | NUMC(4) | 역분개 회계연도 | 2026 |
| BSTAT | CHAR(1) | 전표 상태 | A |
| GLVOR | CHAR(4) | Business Transaction | RFBU |
| AWTYP | CHAR(5) | 참조 문서 유형 | VBRK |
| AWKEY | CHAR(20) | 참조 문서 키 | 9000001234 |
| HWAER | CUKY | 회사 코드 통화 | KRW |
| BVORG | CHAR(10) | Cross Company Posting 번호 | 3000001234 |

</br>

### 2. BSEG (Accounting Document Segment)
- 회계 전표의 각 라인아이템(계정, 금액, 차변/대변, 거래처 등)을 저장하는 FI 핵심 테이블

```
BKPF (전표 헤더)
전표번호 : 1900001234
회사코드 : 1000
전기일자 : 2026-03-01
통화 : KRW

↓

BSEG (전표 라인)

라인1
계정 : 400000 (매출)
금액 : 150000

라인2
계정 : 110000 (매출채권)
금액 : 150000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| BELNR | CHAR(10) | 회계 전표 번호 | 1900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| AUGDT | DATS | 정산 날짜 | 20260310 |
| AUGBL | CHAR(10) | 정산 전표 번호 | 2000000456 |
| KOART | CHAR(1) | 계정 유형 | D |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| HKONT | CHAR(10) | G/L 계정 | 400000 |
| KUNNR | CHAR(10) | 고객 번호 | 10000001 |
| LIFNR | CHAR(10) | 공급업체 번호 | 20000001 |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 150000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 150000 |
| MWSKZ | CHAR(2) | 세금 코드 | A1 |
| ZFBDT | DATS | 지급 기준일 | 20260301 |
| ZTERM | CHAR(4) | 지급 조건 | 0001 |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Sales Invoice |
| ZUONR | CHAR(18) | Assignment 번호 | INV20260301 |
| KOSTL | CHAR(10) | 코스트센터 | 100010 |
| AUFNR | CHAR(12) | 내부 오더 | 500000123456 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| MENGE | QUAN(13,3) | 수량 | 100 |
| MEINS | UNIT(3) | 단위 | EA |
| EBELN | CHAR(10) | 구매오더 번호 | 4500001234 |
| EBELP | NUMC(5) | 구매오더 아이템 | 00010 |
| VBELN | CHAR(10) | 판매 문서 | 5000001234 |
| POSNR | NUMC(6) | 판매 문서 아이템 | 000010 |
| XREF1 | CHAR(12) | 참조 키 1 | REF001 |
| XREF2 | CHAR(12) | 참조 키 2 | REF002 |
| XREF3 | CHAR(12) | 참조 키 3 | REF003 |

</br>

### 3. BSIS (G/L Open Item)
- 미결 상태(Open)인 G/L 계정 전표 라인아이템을 저장하는 테이블 (정산되면 BSAS로 이동)

```
BSIS (미결 전표)

전표 1900001234
계정 : 140000 (비용)
금액 : 500000
상태 : 아직 정산 안됨

↓

지급 / 정산 발생

↓

BSAS (정산 완료 전표)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| HKONT | CHAR(10) | G/L 계정 | 140000 |
| BELNR | CHAR(10) | 전표 번호 | 1900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| BUDAT | DATS | 전기 날짜 | 20260301 |
| BLDAT | DATS | 전표 날짜 | 20260301 |
| AUGDT | DATS | 정산 날짜 | 00000000 |
| AUGBL | CHAR(10) | 정산 전표 번호 |  |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 500000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 500000 |
| WAERS | CUKY | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Office Expense |
| ZUONR | CHAR(18) | Assignment 번호 | EXP202603 |
| KOSTL | CHAR(10) | 코스트센터 | 100010 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |

</br>

### 4. BSAS (G/L Cleared Item)
- 정산(Clearing) 완료된 G/L 계정 전표 라인아이템을 저장하는 테이블 (미결 상태는 BSIS에 저장)

```
BSIS
(미결 G/L 전표)

↓

지급 / 정산 처리

↓

BSAS
(정산 완료 G/L 전표)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| HKONT | CHAR(10) | G/L 계정 | 140000 |
| BELNR | CHAR(10) | 전표 번호 | 1900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| BUDAT | DATS | 전기 날짜 | 20260301 |
| BLDAT | DATS | 전표 날짜 | 20260301 |
| AUGDT | DATS | 정산 날짜 | 20260310 |
| AUGBL | CHAR(10) | 정산 전표 번호 | 2000000456 |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 500000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 500000 |
| WAERS | CUKY | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Office Expense |
| ZUONR | CHAR(18) | Assignment 번호 | EXP202603 |
| KOSTL | CHAR(10) | 코스트센터 | 100010 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |

</br>

### 5. BSIK (Vendor Open Item)
- 지급되지 않은 공급업체(AP) 미결 전표 라인아이템을 저장하는 테이블 (지급/정산되면 BSAK로 이동)

```
BSIK
(공급업체 미지급 전표)

↓

지급 처리 (F-53 등)

↓

BSAK
(공급업체 지급 완료 전표)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| LIFNR | CHAR(10) | 공급업체 번호 | 30000001 |
| BELNR | CHAR(10) | 전표 번호 | 1900003456 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| BUDAT | DATS | 전기 날짜 | 20260305 |
| BLDAT | DATS | 전표 날짜 | 20260305 |
| ZFBDT | DATS | 지급 기준일 | 20260305 |
| ZTERM | CHAR(4) | 지급 조건 | 0001 |
| AUGDT | DATS | 정산 날짜 | 00000000 |
| AUGBL | CHAR(10) | 정산 전표 번호 |  |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 850000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 850000 |
| WAERS | CUKY | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 표시 | H |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Raw Material Purchase |
| EBELN | CHAR(10) | 구매오더 번호 | 4500001234 |
| EBELP | NUMC(5) | 구매오더 아이템 | 00010 |
| ZUONR | CHAR(18) | Assignment 번호 | PO4500001234 |
| KOSTL | CHAR(10) | 코스트센터 | 200020 |

</br>

### 6. 
