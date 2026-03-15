# < FI (Financial Accounting) >

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

### 3. BSET (Tax Data Document Segment)
- 회계 전표에서 발생한 세금(VAT 등) 정보를 저장하는 전표 세금 데이터 테이블

```
BKPF (전표 헤더)
   ↓
BSEG (전표 라인)
   ↓
BSET (세금 정보)

ex)
전표번호 : 1900001234
매출 금액 : 100,000
부가세 : 10,000
세금코드 : A1
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| BELNR | CHAR(10) | 회계 전표 번호 | 1900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| MWSKZ | CHAR(2) | 세금 코드 | A1 |
| MWSTS | CURR(13,2) | 세금 금액 | 10000 |
| HWBAS | CURR(13,2) | 세금 기준 금액 | 100000 |
| FWSTE | CURR(13,2) | 외화 세금 금액 | 10000 |
| FWBAS | CURR(13,2) | 외화 기준 금액 | 100000 |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| HKONT | CHAR(10) | 세금 계정 | 210000 |
| KTOSL | CHAR(3) | 계정 키 | MWS |
| TXGRP | NUMC(3) | 세금 그룹 | 001 |
| TXJCD | CHAR(15) | 세금 관할 코드 | KR-SEOUL |

</br>

### 4. BSEC (One-Time Account Data)

- 일회성 거래처(One-Time Vendor/Customer)로 전표를 입력할 때 사용되는 이름·주소 등 거래처 정보를 저장하는 테이블
```
BSEG
(전표 라인아이템)
     ↓
BSEC
(일회성 거래처 정보 저장)

ex)
일반 거래처 → KNA1 / LFA1 사용
일회성 거래처 → BSEC에 이름/주소 저장
```
| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| BELNR | CHAR(10) | 회계 전표 번호 | 1900004321 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| NAME1 | CHAR(35) | 거래처 이름 | One-Time Vendor |
| NAME2 | CHAR(35) | 추가 이름 | Temp Supplier |
| ORT01 | CHAR(35) | 도시 | Seoul |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소 | Teheran-ro 123 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| BANKN | CHAR(18) | 은행 계좌 번호 | 110123456789 |
| BANKL | CHAR(15) | 은행 코드 | 004 |
| BVTYP | CHAR(4) | 파트너 은행 유형 | MAIN |

</br>

### 5. BSIS (G/L Open Item)
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

### 6. BSAS (G/L Cleared Item)
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

### 7. BSIK (Vendor Open Item)
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

### 8. BSAK (Vendor Cleared Items)
- 지급이 완료되어 정산(Clearing)된 공급업체(AP) 전표 라인아이템을 저장하는 테이블 (미지급 상태는 BSIK에 저장)

```
BSIK (공급업체 미지급 전표)
      ↓ 지급 처리
BSAK (공급업체 지급 완료 전표)
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
| AUGDT | DATS | 정산 날짜 | 20260315 |
| AUGBL | CHAR(10) | 정산 전표 번호 | 2000005678 |
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

### 9. BSID (Customer Open Items)

- 수금되지 않은 고객 매출채권(AR) 미결 전표 라인아이템을 저장하는 테이블 (수금/정산되면 BSAD로 이동)

```
BSID (고객 미수금 전표)
        ↓ 수금 처리
BSAD (고객 수금 완료 전표)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| KUNNR | CHAR(10) | 고객 번호 | 20000001 |
| BELNR | CHAR(10) | 전표 번호 | 1900005678 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| BUDAT | DATS | 전기 날짜 | 20260308 |
| BLDAT | DATS | 전표 날짜 | 20260308 |
| ZFBDT | DATS | 지급 기준일 | 20260308 |
| ZTERM | CHAR(4) | 지급 조건 | 0001 |
| AUGDT | DATS | 정산 날짜 | 00000000 |
| AUGBL | CHAR(10) | 정산 전표 번호 |  |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 1200000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 1200000 |
| WAERS | CUKY | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Product Sales |
| ZUONR | CHAR(18) | Assignment 번호 | INV20260308 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| VBELN | CHAR(10) | 판매 문서 번호 | 5000001234 |
| POSNR | NUMC(6) | 판매 문서 아이템 | 000010 |

</br>

### 10. BSAD (Customer Cleared Items)

- 수금 처리되어 정산(Clearing) 완료된 고객 매출채권(AR) 전표 라인아이템을 저장하는 테이블 (미수 상태는 BSID에 저장)

```
BSID (고객 미수금 전표)
        ↓ 수금 처리
BSAD (고객 수금 완료 전표)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| KUNNR | CHAR(10) | 고객 번호 | 20000001 |
| BELNR | CHAR(10) | 전표 번호 | 1900005678 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| BUDAT | DATS | 전기 날짜 | 20260308 |
| BLDAT | DATS | 전표 날짜 | 20260308 |
| ZFBDT | DATS | 지급 기준일 | 20260308 |
| ZTERM | CHAR(4) | 지급 조건 | 0001 |
| AUGDT | DATS | 정산 날짜 | 20260320 |
| AUGBL | CHAR(10) | 정산 전표 번호 | 2000007890 |
| DMBTR | CURR(13,2) | 회사 코드 통화 금액 | 1200000 |
| WRBTR | CURR(13,2) | 문서 통화 금액 | 1200000 |
| WAERS | CUKY | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 표시 | S |
| SGTXT | CHAR(50) | 전표 라인 텍스트 | Product Sales |
| ZUONR | CHAR(18) | Assignment 번호 | INV20260308 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| VBELN | CHAR(10) | 판매 문서 번호 | 5000001234 |
| POSNR | NUMC(6) | 판매 문서 아이템 | 000010 |

</br>

### 11. ACDOCA (Universal Journal Entry Line Items)

- S/4HANA에서 FI·CO·AA·ML 등 여러 회계 모듈의 라인아이템 데이터를 통합 저장하는 Universal Journal 테이블

```
ECC 시스템

BKPF (전표 헤더)
BSEG (전표 라인)
COEP (CO 라인)
ANEP (자산 라인)

↓

S/4HANA

ACDOCA
(모든 회계 라인아이템 통합)

ex)
전표번호 : 1900001234
계정 : 400000 (매출)
금액 : 150000
코스트센터 : 100010
Profit Center : PC1000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| RCLNT | CLNT(3) | 클라이언트 (레코드 기준) | 100 |
| RBUKRS | CHAR(4) | 회사 코드 | 1000 |
| BELNR | CHAR(10) | 회계 전표 번호 | 1900001234 |
| DOCLN | NUMC(6) | 전표 라인 번호 | 000001 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUDAT | DATS | 전기 날짜 (Posting Date) | 20260301 |
| BLART | CHAR(2) | 전표 유형 | SA |
| HKONT | CHAR(10) | G/L 계정 | 400000 |
| RACCT | CHAR(10) | 계정 번호 (Universal Journal 기준) | 400000 |
| KUNNR | CHAR(10) | 고객 번호 | 20000001 |
| LIFNR | CHAR(10) | 공급업체 번호 | 30000001 |
| MATNR | CHAR(40) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| KOSTL | CHAR(10) | 코스트센터 | 100010 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| AUFNR | CHAR(12) | 내부 오더 | 500000123456 |
| SEGMENT | CHAR(10) | 세그먼트 | SEG01 |
| DMBTR | CURR(23,2) | 회사 코드 통화 금액 | 150000 |
| WRBTR | CURR(23,2) | 문서 통화 금액 | 150000 |
| WAERS | CUKY | 통화 | KRW |
| HSL | CURR(23,2) | 회사 코드 통화 금액 | 150000 |
| KSL | CURR(23,2) | 그룹 통화 금액 | 150000 |
| TSL | CURR(23,2) | 거래 통화 금액 | 150000 |

</br>

### 12. SKA1 (G/L Account Master – Chart of Accounts Level)

- Chart of Accounts 기준으로 G/L 계정의 기본 정보(계정명, 계정 유형 등)를 저장하는 G/L 계정 마스터 테이블

```
SKA1
(Chart of Accounts 기준 G/L 계정 기본 정보)
        ↓
SKB1
(회사코드 기준 G/L 계정 설정)

ex)
SKA1
계정번호 : 400000
계정명 : Sales Revenue

SKB1
회사코드 : 1000
통화 : KRW
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KTOPL | CHAR(4) | Chart of Accounts | INT |
| SAKNR | CHAR(10) | G/L 계정 번호 | 400000 |
| XBILK | CHAR(1) | Balance Sheet 계정 여부 | X |
| GVTYP | CHAR(1) | 손익계산서 계정 유형 | X |
| TXT20 | CHAR(20) | G/L 계정 단축 이름 | Sales Rev |
| TXT50 | CHAR(50) | G/L 계정 설명 | Sales Revenue |
| ERDAT | DATS | 계정 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 계정 생성 사용자 | SAPUSER |
| KTOKS | CHAR(4) | 계정 그룹 | PL01 |
| XLOEV | CHAR(1) | 삭제 표시 |  |

</br>

### 13. SKB1 (G/L Account Master – Company Code Level)

- 회사코드 기준으로 G/L 계정의 설정(통화, 계정 관리 방식 등)을 저장하는 G/L 계정 마스터 테이블

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| SAKNR | CHAR(10) | G/L 계정 번호 | 400000 |
| ERDAT | DATS | 계정 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 계정 생성 사용자 | SAPUSER |
| WAERS | CUKY | 계정 통화 | KRW |
| MITKZ | CHAR(1) | 보조원장 계정 유형 | D |
| XOPVW | CHAR(1) | Open Item 관리 여부 | X |
| XSPEB | CHAR(1) | 특별 G/L 허용 여부 |  |
| FSTAG | CHAR(4) | 필드 상태 그룹 | G001 |
| ZUAWA | CHAR(3) | 정산 키 | 001 |
| FDLEV | CHAR(2) | 재무 관리 레벨 | 01 |
| XINTB | CHAR(1) | 이자 계산 대상 여부 | X |
| ALTKT | CHAR(10) | 대체 계정 번호 | 410000 |

</br>

### 14. KNA1 (Customer Master – General Data)

- 고객의 기본 정보(이름, 주소, 국가 등)를 저장하는 고객 마스터 일반 데이터 테이블

```
KNA1 (고객 기본 정보)
      ↓
KNB1 (회사코드 기준 고객 회계 정보)
      ↓
KNVV (판매영역 기준 고객 판매 정보)

ex)
고객번호 : 20000001
고객명 : ABC Trading
국가 : KR
도시 : Seoul
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KUNNR | CHAR(10) | 고객 번호 | 20000001 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| NAME1 | CHAR(35) | 고객 이름 | ABC Trading |
| NAME2 | CHAR(35) | 고객 추가 이름 | Seoul Branch |
| ORT01 | CHAR(35) | 도시 | Seoul |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소(거리) | Teheran-ro 123 |
| TELF1 | CHAR(16) | 전화번호 | 0212345678 |
| TELFX | CHAR(31) | 팩스 번호 | 0212345679 |
| STCD1 | CHAR(16) | 사업자 등록 번호 | 1234567890 |
| STCEG | CHAR(20) | VAT 번호 | KR123456789 |
| KTOKD | CHAR(4) | 고객 계정 그룹 | Z001 |
| LOEVM | CHAR(1) | 삭제 표시 |  |
| ERDAT | DATS | 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |

</br>

### 15. LFA1 (Vendor Master – General Data)

- 공급업체(거래처)의 기본 정보(이름, 주소, 국가 등)를 저장하는 공급업체 마스터 일반 데이터 테이블

```
LFA1 (공급업체 기본 정보)
      ↓
LFB1 (회사코드 기준 공급업체 회계 정보)
      ↓
LFM1 (구매조직 기준 공급업체 구매 정보)

ex)
공급업체번호 : 30000001
공급업체명 : ABC Chemical
국가 : KR
도시 : Seoul
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| LIFNR | CHAR(10) | 공급업체 번호 | 30000001 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| NAME1 | CHAR(35) | 공급업체 이름 | ABC Chemical |
| NAME2 | CHAR(35) | 공급업체 추가 이름 | Seoul Branch |
| ORT01 | CHAR(35) | 도시 | Seoul |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소(거리) | Teheran-ro 123 |
| TELF1 | CHAR(16) | 전화번호 | 0212345678 |
| TELFX | CHAR(31) | 팩스 번호 | 0212345679 |
| STCD1 | CHAR(16) | 사업자 등록 번호 | 1234567890 |
| STCEG | CHAR(20) | VAT 번호 | KR123456789 |
| KTOKK | CHAR(4) | 공급업체 계정 그룹 | Z001 |
| LOEVM | CHAR(1) | 삭제 표시 |  |
| ERDAT | DATS | 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |

</br>

### 16. T001 (Company Code)

- 회사코드(Company Code)의 기본 정보(회사명, 통화, 국가 등)를 저장하는 조직구조 마스터 테이블

```
Client
   ↓
Company Code (T001)
   ↓
Plant / Sales Org / Purchasing Org

ex)
회사코드 : 1000
회사명 : ABC Cosmetics
통화 : KRW
국가 : KR
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| BUTXT | CHAR(25) | 회사 이름 | ABC Cosmetics |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| WAERS | CUKY | 회사 코드 통화 | KRW |
| KTOPL | CHAR(4) | Chart of Accounts | INT |
| PERIV | CHAR(2) | 회계 기간 Variant | K4 |
| ORT01 | CHAR(35) | 도시 | Seoul |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소 | Teheran-ro 123 |
| XFMCO | CHAR(1) | 재무 관리 활성화 여부 | X |

</br>

### 17. T001W (Plant)

- 플랜트(공장·창고 등 물류 거점)의 기본 정보(플랜트명, 위치, 회사코드 등)를 저장하는 조직구조 마스터 테이블

```
Client
   ↓
Company Code (T001)
   ↓
Plant (T001W)
   ↓
Storage Location

ex)
회사코드 : 1000
플랜트 : 1100
플랜트명 : Seoul Plant
도시 : Seoul
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| WERKS | CHAR(4) | 플랜트 코드 | 1100 |
| NAME1 | CHAR(30) | 플랜트 이름 | Seoul Plant |
| BWKEY | CHAR(4) | Valuation Area (평가 영역) | 1100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| ORT01 | CHAR(35) | 도시 | Seoul |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소 | Teheran-ro 123 |
| ADRNR | CHAR(10) | 주소 번호 | 0000123456 |

</br>
</br>

---

</br>

# < CO (Controlling) >

### 1. CSKA (Cost Element Master – Chart of Accounts)

- Chart of Accounts 기준으로 원가요소(Cost Element)의 기본 정보(원가요소 유형, 계정 연결 등)를 저장하는 CO 마스터 테이블

```
G/L Account (FI)
      ↓
Cost Element (CSKA)
      ↓
Cost Center / Internal Order

ex)
원가요소 : 400000
유형 : Primary Cost Element
설명 : Sales Cost
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KTOPL | CHAR(4) | Chart of Accounts | INT |
| KSTAR | CHAR(10) | 원가요소 번호 | 400000 |
| ERDAT | DATS | 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| KATYP | NUMC(2) | 원가요소 유형 (1: Primary, 42: Secondary 등) | 01 |
| SAKNR | CHAR(10) | 연결된 G/L 계정 | 400000 |
| KTOKS | CHAR(4) | 원가요소 그룹 | Z001 |
| TXT20 | CHAR(20) | 원가요소 단축 이름 | Sales Cost |
| TXT50 | CHAR(50) | 원가요소 설명 | Sales Cost Element |

</br>

### 2. SKB (Cost Element Master – Company Code Level)

- 회사코드 기준으로 원가요소(Cost Element)의 설정(통화, 비용 관리 방식 등)을 저장하는 CO 마스터 테이블

```
CSKA
(Chart of Accounts 기준 원가요소)
      ↓
CSKB
(회사코드 기준 원가요소 설정)
      ↓
COEP / COSS / COSP
(실제 원가 데이터)

ex)
원가요소 : 400000
회사코드 : 1000
통화 : KRW
원가유형 : Primary Cost Element
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| KSTAR | CHAR(10) | 원가요소 번호 | 400000 |
| ERDAT | DATS | 생성 날짜 | 20260101 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| WAERS | CUKY | 통화 | KRW |
| KATYP | NUMC(2) | 원가요소 유형 | 01 |
| FSTAG | CHAR(4) | 필드 상태 그룹 | G001 |
| XINTB | CHAR(1) | 이자 계산 대상 여부 |  |
| ZUAWA | CHAR(3) | 정산 키 | 001 |
| FDLEV | CHAR(2) | 재무 관리 레벨 | 01 |

</br>

### 3. CSKS (Cost Center Master – Basic Data)

- 코스트센터(Cost Center)의 기본 정보(코스트센터 이름, 책임자, 기간 등)를 저장하는 CO 마스터 테이블

```
Cost Element (CSKA / CSKB)
        ↓
Cost Center (CSKS)
        ↓
Actual Cost Posting (COEP)

ex)
Controlling Area : A000
Cost Center : CC1000
Cost Center Name : Admin Cost
Manager : Kim Manager
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KOKRS | CHAR(4) | Controlling Area | A000 |
| KOSTL | CHAR(10) | 코스트센터 | CC1000 |
| DATBI | DATS | 유효 종료일 | 99991231 |
| DATAB | DATS | 유효 시작일 | 20260101 |
| VERAK | CHAR(20) | 책임자 | Kim Manager |
| BKZKP | CHAR(1) | 계획 통화 여부 | X |
| WAERS | CUKY | 통화 | KRW |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| KHINR | CHAR(12) | 코스트센터 그룹 | ADMIN |
| KTEXT | CHAR(20) | 코스트센터 이름 | Admin Cost |
| LTEXT | CHAR(40) | 코스트센터 설명 | Administration Dept Cost Center |

</br>

### 4. COEP (CO Object Line Items)

- 코스트센터, 내부오더 등 CO 객체에 실제 발생한 원가(Actual Cost) 라인아이템을 저장하는 CO 핵심 테이블

```
Cost Element (CSKA / CSKB)
        ↓
Cost Center (CSKS)
        ↓
CO Posting
        ↓
COEP (Actual Cost 저장)

ex)
코스트센터 : CC1000
원가요소 : 400000
금액 : 150000
기간 : 2026 / 03
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KOKRS | CHAR(4) | Controlling Area | A000 |
| BELNR | CHAR(10) | CO 전표 번호 | 4900001234 |
| BUZEI | NUMC(3) | 전표 라인 번호 | 001 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| PERIO | NUMC(3) | 회계 기간 | 003 |
| KSTAR | CHAR(10) | 원가요소 | 400000 |
| KOSTL | CHAR(10) | 코스트센터 | CC1000 |
| AUFNR | CHAR(12) | 내부 오더 | 500000123456 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| OBJNR | CHAR(22) | CO 객체 번호 | KSCC1000 |
| WERTN | CURR(13,2) | 금액 | 150000 |
| WAERS | CUKY | 통화 | KRW |
| BUDAT | DATS | 전기 날짜 | 20260301 |
| SGTXT | CHAR(50) | 전표 텍스트 | Office Expense |

</br>

### 5. COBK (CO Document Header)

- CO 전표의 헤더 정보(Controlling Area, 전표번호, 전기일자 등)를 저장하는 CO 전표 헤더 테이블

```
COBK (CO 전표 헤더)
        ↓
COEP (CO 전표 라인아이템)

ex)
CO 전표번호 : 4900001234
Controlling Area : A000
전기일자 : 2026-03-01
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KOKRS | CHAR(4) | Controlling Area | A000 |
| BELNR | CHAR(10) | CO 전표 번호 | 4900001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUDAT | DATS | 전기 날짜 | 20260301 |
| BLART | CHAR(2) | 전표 유형 | R4 |
| CPUDT | DATS | 전표 입력 날짜 | 20260301 |
| CPUTM | TIMS | 전표 입력 시간 | 103000 |
| USNAM | CHAR(12) | 전표 생성 사용자 | SAPUSER |
| TCODE | CHAR(20) | 전표 생성 T-Code | KB11N |
| VRGNG | CHAR(4) | CO 비즈니스 트랜잭션 | RKU1 |
| AWTYP | CHAR(5) | 참조 문서 유형 | BKPF |
| AWKEY | CHAR(20) | 참조 문서 키 | 19000012342026 |
| WAERS | CUKY | 통화 | KRW |

</br>

### 6. AUFK (Order Master Data)

- 내부오더(Internal Order) 등 오더의 기본 정보(오더 유형, 담당자, 회사코드 등)를 저장하는 CO 오더 마스터 테이블

```
Internal Order 생성
        ↓
AUFK (오더 기본 정보 저장)
        ↓
COEP (오더에 발생한 실제 원가 저장)

ex)
오더번호 : 500000123456
오더유형 : ZINT
설명 : Marketing Campaign
코스트센터 : CC1000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFNR | CHAR(12) | 오더 번호 | 500000123456 |
| AUTYP | NUMC(2) | 오더 유형 | 01 |
| AUART | CHAR(4) | 오더 타입 | ZINT |
| KOKRS | CHAR(4) | Controlling Area | A000 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| ERDAT | DATS | 생성 날짜 | 20260301 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| AEDAT | DATS | 마지막 변경 날짜 | 20260310 |
| KTEXT | CHAR(40) | 오더 설명 | Marketing Campaign |
| KOSTV | CHAR(10) | 책임 코스트센터 | CC1000 |
| PRCTR | CHAR(10) | Profit Center | PC1000 |
| OBJNR | CHAR(22) | 오브젝트 번호 | OR500000123456 |
| LOEKZ | CHAR(1) | 삭제 표시 |  |

</br>

### 7. CE1XXXX (Profitability Analysis Line Items – Actual)

- CO-PA(수익성 분석)에서 실제 매출·원가·수익 데이터를 저장하는 라인아이템 테이블  
※ XXXX = Operating Concern (예: CE1A000)

```
SD Billing (매출 발생)
        ↓
CO-PA 전송
        ↓
CE1XXXX (수익성 분석 Actual 데이터 저장)

CE1XXXX → Actual 수익성 데이터
CE2XXXX → 계획 데이터
CE3XXXX → 요약 데이터

ex)
고객 : 20000001
제품 : MAT-10001
매출 : 1,500,000
원가 : 900,000
이익 : 600,000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PAOBJNR | NUMC(10) | Profitability Segment 번호 | 0000123456 |
| VRGAR | CHAR(2) | CO-PA 레코드 유형 | F |
| PERIO | NUMC(3) | 회계 기간 | 003 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUDAT | DATS | 전기 날짜 | 20260301 |
| ARTNR | CHAR(18) | 자재 번호 | MAT-10001 |
| KNDNR | CHAR(10) | 고객 번호 | 20000001 |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 01 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| ERLOS | CURR(15,2) | 매출 금액 | 1500000 |
| KOSTEN | CURR(15,2) | 원가 금액 | 900000 |
| WAERS | CUKY | 통화 | KRW |

</br>

### 8. CE2XXXX (Profitability Analysis – Plan Data)

- CO-PA(수익성 분석)에서 계획(Plan) 매출·원가·수익 데이터를 저장하는 테이블  
※ XXXX = Operating Concern (예: CE2A000)

```
계획 매출 / 원가 수립
        ↓
CO-PA Plan 데이터 저장
        ↓
CE2XXXX (계획 수익성 데이터)

ex)
고객 : 20000001
제품 : MAT-10001
계획 매출 : 1,800,000
계획 원가 : 1,100,000
계획 이익 : 700,000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PAOBJNR | NUMC(10) | Profitability Segment 번호 | 0000123456 |
| VRGAR | CHAR(2) | CO-PA 레코드 유형 | B |
| PERIO | NUMC(3) | 회계 기간 | 003 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| ARTNR | CHAR(18) | 자재 번호 | MAT-10001 |
| KNDNR | CHAR(10) | 고객 번호 | 20000001 |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 01 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| ERLOS | CURR(15,2) | 계획 매출 금액 | 1800000 |
| KOSTEN | CURR(15,2) | 계획 원가 금액 | 1100000 |
| WAERS | CUKY | 통화 | KRW |

</br>
</br>

---

</br>
