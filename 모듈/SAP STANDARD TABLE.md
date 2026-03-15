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

### Purchasing (구매)

| Table | Description |
|------|-------------|
| EKKO | 구매오더 헤더 (PO 전체 기본정보 관리) |
| EKPO | 구매오더 아이템 (PO 자재 및 수량 등 상세 정보 관리) |
| EKET | 구매오더 스케줄라인 (납기일 및 납품 일정 관리) |
| EKBE | 구매오더 이력 (입고 및 송장 처리 이력 관리) |
| EBAN | 구매요청(PR) 데이터 (구매요청 생성 정보 관리) |
| EBKN | PR Account Assignment (PR의 계정 지정 정보 관리) |
| EINA | 구매정보레코드 일반 데이터 (자재-공급업체 기본 구매정보 관리) |
| EINE | 구매정보레코드 구매조직 데이터 (구매조직 기준 자재-공급업체 구매조건 관리) |

### Inventory Management (재고)

| Table | Description |
|------|-------------|
| MKPF | 자재문서 헤더 (입고/출고 등 자재 이동 문서 헤더 관리) |
| MSEG | 자재문서 아이템 (자재 이동 상세 데이터 관리) |
| MATDOC | S/4HANA 자재 문서 통합 테이블 |
| MARD | 저장위치 재고 (저장위치 기준 재고 수량 관리) |
| MCHB | Batch 재고 (Batch 단위 자재 재고 관리) |
| RESB | Reservation 데이터 (자재 예약 정보 관리) |
| RKPF | Reservation Header (자재 예약 문서 헤더 관리) |

### Valuation / Accounting

| Table | Description |
|------|-------------|
| MBEW | 자재 평가 데이터 (자재 가격 및 재고 평가 관련 정보 관리) |
| T001K | Valuation Area 설정 데이터 |

### Logistics / Plant

| Table | Description |
|------|-------------|
| T001W | 플랜트 정보 (플랜트 조직 정보 관리) |
| T001L | 저장위치 정보 (저장위치 조직 정보 관리) |

</br>
</br>
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
