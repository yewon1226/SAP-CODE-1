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

### 2. CSKB (Cost Element Master – Company Code Level)

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

# < SAP MM 주요 Standard Tables >
</br>

## < Material Master >

### 1. MARA (Material Master – General Data)

- 자재(Material)의 기본 정보(자재 유형, 단위, 그룹 등)를 저장하는 자재 마스터 일반 데이터 테이블

```
MARA (자재 기본 정보)
        ↓
MARC (플랜트별 자재 정보)
        ↓
MARD (저장 위치별 재고 정보)

ex)
자재번호 : MAT-10001
자재유형 : FERT
자재그룹 : COSMETIC
기본단위 : EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| MTART | CHAR(4) | 자재 유형 | FERT |
| MATKL | CHAR(9) | 자재 그룹 | COSMETIC |
| MEINS | UNIT(3) | 기본 단위 | EA |
| ERSDA | DATS | 생성 날짜 | 20260301 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| BISMT | CHAR(18) | 이전 자재 번호 | OLD-10001 |
| BRGEW | QUAN(13,3) | 총 중량 | 0.500 |
| NTGEW | QUAN(13,3) | 순 중량 | 0.450 |
| GEWEI | UNIT(3) | 중량 단위 | KG |
| VOLUM | QUAN(13,3) | 부피 | 0.001 |
| VOLEH | UNIT(3) | 부피 단위 | M3 |
| SPART | CHAR(2) | 제품군 | 01 |
| XCHPF | CHAR(1) | Batch 관리 여부 | X |
| LVORM | CHAR(1) | 삭제 표시 |  |

</br>

### 2. MARC (Material Master – Plant Data)

- 플랜트(Plant) 기준으로 자재의 관리 정보(MRP, 생산, 구매 등)를 저장하는 자재 마스터 테이블

```
자재번호 : MAT-10001
플랜트 : 1100
MRP 유형 : PD
최소 재고 : 100
구매 그룹 : 001
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| DISMM | CHAR(2) | MRP 유형 | PD |
| DISPO | CHAR(3) | MRP 담당자 | 001 |
| DISLS | CHAR(2) | Lot Size | EX |
| BESKZ | CHAR(1) | 조달 유형 (내부/외부 생산) | E |
| SOBSL | CHAR(2) | Special Procurement |  |
| PLIFZ | NUMC(3) | 구매 리드타임 | 005 |
| WEBAZ | NUMC(3) | 입고 처리 시간 | 001 |
| MINBE | QUAN(13,3) | 최소 재고 | 100 |
| MABST | QUAN(13,3) | 최대 재고 | 1000 |
| LGPRO | CHAR(4) | 생산 저장 위치 | 0001 |
| EKGRP | CHAR(3) | 구매 그룹 | 001 |
| MMSTA | CHAR(2) | 자재 상태 | 01 |

</br>

### 3. MARD (Material Master – Storage Location Data)

- 플랜트 내 저장위치(Storage Location) 기준으로 자재의 재고 수량 정보를 저장하는 자재 재고 테이블

```
자재번호 : MAT-10001
플랜트 : 1100
저장위치 : 0001
사용가능 재고 : 500 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| LABST | QUAN(13,3) | 사용 가능 재고(Unrestricted Stock) | 500 |
| UMLME | QUAN(13,3) | 이동 중 재고 | 50 |
| INSME | QUAN(13,3) | 검사 중 재고 | 20 |
| SPEME | QUAN(13,3) | Blocked 재고 | 10 |
| LGPBE | CHAR(10) | Storage Bin | BIN-01 |
| LFGJA | NUMC(4) | 마지막 재고 변경 연도 | 2026 |
| LFMON | NUMC(2) | 마지막 재고 변경 월 | 03 |

</br>

### 4. MBEW (Material Valuation)

- 자재의 평가 정보(평가가격, 이동평균가, 표준가격 등 자재 원가)를 저장하는 자재 평가 테이블

```
자재번호 : MAT-10001
플랜트 : 1100
표준가격 : 12,000
총 재고 수량 : 500
총 재고 금액 : 5,900,00
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| BWKEY | CHAR(4) | 평가 영역 (보통 Plant) | 1100 |
| BWTAR | CHAR(10) | 평가 유형 |  |
| STPRS | CURR(13,2) | 표준 가격 (Standard Price) | 12000 |
| VERPR | CURR(13,2) | 이동 평균 가격 (Moving Average Price) | 11800 |
| PEINH | DEC(5) | 가격 단위 | 1 |
| VPRSV | CHAR(1) | 가격 결정 방법 (S:표준, V:이동평균) | S |
| LBKUM | QUAN(13,3) | 총 재고 수량 | 500 |
| SALK3 | CURR(13,2) | 총 재고 금액 | 5900000 |
| LFGJA | NUMC(4) | 마지막 재고 변경 연도 | 2026 |
| LFMON | NUMC(2) | 마지막 재고 변경 월 | 03 |

</br>

### 5. MARM (Material Units of Measure)

- 자재(Material)의 기본 단위 외에 추가 단위(BOX, KG 등)와 단위 변환 비율을 저장하는 테이블

```
MARA (자재 기본 정보 / 기본 단위)
        ↓
MARM (대체 단위 및 단위 변환 정보)

ex)
자재번호 : MAT-10001
기본단위 : EA
대체단위 : BOX
변환비율 : 1 BOX = 10 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| MEINH | UNIT(3) | 대체 단위 | BOX |
| UMREZ | DEC(5) | 변환 분자 | 10 |
| UMREN | DEC(5) | 변환 분모 | 1 |
| EAN11 | CHAR(18) | 바코드 번호 | 8801234567890 |
| NUMTP | CHAR(2) | 바코드 유형 | E1 |
| LAENG | QUAN(13,3) | 길이 | 10 |
| BREIT | QUAN(13,3) | 너비 | 5 |
| HOEHE | QUAN(13,3) | 높이 | 3 |
| MEABM | UNIT(3) | 치수 단위 | CM |

</br>
</br>
</br>

## < Vendor Master >

### 1. LFA1 (Vendor Master – General Data)
- 공급업체(Vendor)의 기본 정보(이름, 주소, 국가, 연락처 등)를 저장하는 공급업체 마스터 일반 데이터 테이블

```
LFA1 (공급업체 기본 정보)
        ↓
LFB1 (회사코드 기준 공급업체 정보)
        ↓
LFM1 (구매조직 기준 공급업체 정보)

ex)
공급업체번호 : 100000
공급업체명 : COSMETIC SUPPLIER
국가 : KR
도시 : SEOUL
전화번호 : 0212345678
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| NAME1 | CHAR(35) | 공급업체 이름 | COSMETIC SUPPLIER |
| NAME2 | CHAR(35) | 공급업체 추가 이름 | KOREA BRANCH |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| ORT01 | CHAR(35) | 도시 | SEOUL |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| STRAS | CHAR(35) | 주소 | TEHERAN-RO |
| TELF1 | CHAR(16) | 전화번호 | 0212345678 |
| STCD1 | CHAR(16) | 사업자 등록번호 | 1234567890 |
| KTOKK | CHAR(4) | 공급업체 계정 그룹 | ZVEN |
| LOEVM | CHAR(1) | 삭제 플래그 |  |
| SPERR | CHAR(1) | 거래 차단 여부 |  |

</br>

### 2. LFB1 (Vendor Master – Company Code Data)

- 회사코드 기준으로 공급업체의 회계 정보(지불조건, 통화, 채무 관리 등)를 저장하는 공급업체 회계 마스터 테이블

```
LFA1 (공급업체 기본 정보)
        ↓
LFB1 (회사코드 기준 회계 정보)
        ↓
LFM1 (구매조직 기준 구매 정보)

ex)
공급업체번호 : 100000
회사코드 : 1000
지불조건 : 0001
통화 : KRW
연결 G/L 계정 : 300000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| BUKRS | CHAR(4) | 회사코드 | 1000 |
| AKONT | CHAR(10) | 연결 G/L 계정 (Recon Account) | 300000 |
| ZTERM | CHAR(4) | 지불 조건 | 0001 |
| ZWELS | CHAR(10) | 지불 방법 | T |
| WAERS | CUKY(5) | 통화 | KRW |
| ZUAWA | CHAR(3) | 정산 기준 | 001 |
| BUSAB | CHAR(4) | 회계 담당자 | AP01 |
| LOEVM | CHAR(1) | 삭제 표시 |  |
| SPERR | CHAR(1) | 회계 차단 |  |

</br>

### 3. LFM1 (Vendor Master – Purchasing Organization Data)

- 구매조직(Purchasing Organization) 기준으로 공급업체의 구매 관련 정보(구매 담당자, 통화, Incoterms 등)를 저장하는 공급업체 구매 마스터 테이블

```
LFA1 (공급업체 기본 정보)
        ↓
LFB1 (회사코드 기준 회계 정보)
        ↓
LFM1 (구매조직 기준 구매 정보)

ex)
공급업체번호 : 100000
구매조직 : 1000
구매그룹 : 001
통화 : KRW
Incoterms : FOB BUSAN
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| EKORG | CHAR(4) | 구매조직 | 1000 |
| EKGRP | CHAR(3) | 구매 그룹 | 001 |
| WAERS | CUKY(5) | 구매 통화 | KRW |
| ZTERM | CHAR(4) | 지불 조건 | 0001 |
| INCO1 | CHAR(3) | Incoterms (운송 조건) | FOB |
| INCO2 | CHAR(28) | Incoterms 위치 | BUSAN |
| VERKF | CHAR(30) | 공급업체 담당자 | Kim |
| TELF1 | CHAR(16) | 담당자 전화번호 | 0212345678 |
| LOEVM | CHAR(1) | 삭제 표시 |  |
| SPERM | CHAR(1) | 구매 차단 여부 |  |

</br>

### 4. BUT000 (Business Partner – General Data)

- SAP S/4HANA에서 Business Partner(BP)의 기본 정보(이름, 유형, 생성일 등)를 저장하는 마스터 데이터 테이블

```
BUT000 (Business Partner 기본 정보)
        ↓
LFA1 (Vendor 데이터)
        ↓
KNA1 (Customer 데이터)

ex)
BP 번호 : 100000
BP 유형 : Organization
이름 : COSMETIC SUPPLIER
생성일 : 20260310
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 100000 |
| TYPE | CHAR(1) | BP 유형 (Person/Organization/Group) | 2 |
| BU_GROUP | CHAR(4) | BP 그룹 | ZVEN |
| NAME_ORG1 | CHAR(40) | 조직 이름 | COSMETIC SUPPLIER |
| NAME_ORG2 | CHAR(40) | 추가 조직 이름 | KOREA BRANCH |
| NAME_FIRST | CHAR(40) | 이름 (Person) | JAEHWAN |
| NAME_LAST | CHAR(40) | 성 (Person) | KIM |
| BIRTHDT | DATS | 생년월일 | 19990101 |
| CREATED_BY | CHAR(12) | 생성 사용자 | SAPUSER |
| CREATED_ON | DATS | 생성 날짜 | 20260310 |
| CHANGED_BY | CHAR(12) | 변경 사용자 | SAPUSER |
| CHANGED_ON | DATS | 변경 날짜 | 20260311 |
| XDELE | CHAR(1) | 삭제 표시 |  |

</br>

### 5. BUT020 (Business Partner – Address Data)

- Business Partner(BP)의 주소 정보(주소 번호, 기본 주소 여부 등)를 저장하는 테이블

```
BUT000 (BP 기본 정보)
        ↓
BUT020 (BP 주소 정보)
        ↓
ADR* 테이블 (상세 주소 정보)

ex)
BP 번호 : 100000
주소 번호 : 0000123456
기본 주소 : X
유효기간 : 20200101 ~ 99991231
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 100000 |
| ADDRNUMBER | CHAR(10) | 주소 번호 | 0000123456 |
| ADDR_VALID_FROM | DATS | 주소 유효 시작일 | 20200101 |
| ADDR_VALID_TO | DATS | 주소 유효 종료일 | 99991231 |
| XDFADR | CHAR(1) | 기본 주소 여부 | X |
| NATION | CHAR(1) | 국가 버전 |  |
| DATE_FROM | DATS | 주소 시작일 | 20200101 |
| DATE_TO | DATS | 주소 종료일 | 99991231 |

</br>

### 6. BUT100 (Business Partner – Roles)

- Business Partner(BP)가 어떤 역할(Role)을 가지는지 저장하는 테이블  
(예: Customer, Vendor, Employee 등)

```
BUT000 (BP 기본 정보)
        ↓
BUT100 (BP 역할 정보)
        ↓
LFA1 / KNA1 (Vendor / Customer 데이터)

ex)
BP 번호 : 100000
역할 : FLVN00 (Supplier)
유효기간 : 20200101 ~ 99991231
```

```
FLCU01 → Customer (SD)
FLCU00 → Customer (FI)
FLVN01 → Vendor (MM)
FLVN00 → Vendor (FI)
```

| BP Role | Meaning |
|--------|--------|
| FLVN00 | Supplier (FI Vendor) |
| FLVN01 | Supplier (Purchasing) |
| FLCU00 | Customer (FI) |
| FLCU01 | Customer (Sales) |
| BUP001 | Contact Person |
| BUP003 | Employee |

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 100000 |
| RLTYP | CHAR(6) | BP 역할 유형 | FLVN00 |
| VALID_FROM | DATS | 역할 시작일 | 20200101 |
| VALID_TO | DATS | 역할 종료일 | 99991231 |
| CREATED_BY | CHAR(12) | 생성 사용자 | SAPUSER |
| CREATED_ON | DATS | 생성 날짜 | 20260310 |
| CHANGED_BY | CHAR(12) | 변경 사용자 | SAPUSER |
| CHANGED_ON | DATS | 변경 날짜 | 20260311 |

</br>
</br>
</br>

## < Purchasing (구매) >

### 1. EKKO (Purchasing Document Header)

- 구매오더(PO) 등의 구매 문서 헤더 정보(공급업체, 구매조직, 통화, 생성일 등)를 저장하는 MM 구매 헤더 테이블

```
PR (EBAN)
      ↓
PO Header (EKKO)
      ↓
PO Item (EKPO)

ex)
PO 번호 : 4500001234
공급업체 : 100000
구매조직 : 1000
통화 : KRW
생성일 : 20260310
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| EBELN | CHAR(10) | 구매 문서 번호 (PO 번호) | 4500001234 |
| BSTYP | CHAR(1) | 구매 문서 유형 | F |
| BSART | CHAR(4) | 구매 문서 타입 | NB |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| EKORG | CHAR(4) | 구매 조직 | 1000 |
| EKGRP | CHAR(3) | 구매 그룹 | 001 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| WAERS | CUKY(5) | 통화 | KRW |
| BEDAT | DATS | 문서 생성일 | 20260310 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| KDATB | DATS | 유효 시작일 | 20260310 |
| KDATE | DATS | 유효 종료일 | 20260331 |
| INCO1 | CHAR(3) | Incoterms | FOB |
| INCO2 | CHAR(28) | Incoterms 위치 | BUSAN |

</br>

### 2. EKPO (Purchasing Document Item)

- 구매오더(PO)의 **품목(Item) 단위 정보**(자재, 수량, 가격 등)를 저장하는 MM 구매 문서 아이템 테이블

```
PR (EBAN)
      ↓
PO Header (EKKO)
      ↓
PO Item (EKPO)
      ↓
GR (MSEG / MATDOC)

ex)
PO 번호 : 4500001234
품목 : 00010
자재 : MAT-10001
수량 : 100 EA
가격 : 12,000
납기일 : 20260320
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| EBELN | CHAR(10) | 구매 문서 번호 (PO) | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 번호 | 00010 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| MENGE | QUAN(13,3) | 주문 수량 | 100 |
| MEINS | UNIT(3) | 주문 단위 | EA |
| NETPR | CURR(11,2) | 순 가격 | 12000 |
| PEINH | DEC(5) | 가격 단위 | 1 |
| BPRME | UNIT(3) | 주문 가격 단위 | EA |
| EINDT | DATS | 납기일 | 20260320 |
| ELIKZ | CHAR(1) | 납품 완료 표시 |  |
| LOEKZ | CHAR(1) | 삭제 표시 |  |

</br>

### 3. EKET (Purchasing Document Schedule Line)

- 구매오더(PO) 품목의 납기 일정(Schedule Line: 납기일, 수량 등)을 저장하는 MM 스케줄 라인 테이블

```
PR (EBAN)
      ↓
PO Header (EKKO)
      ↓
PO Item (EKPO)
      ↓
Schedule Line (EKET)
      ↓
GR (MSEG / MATDOC)

ex)
PO 번호 : 4500001234
품목 : 00010
스케줄 라인 : 0001
납기일 : 20260320
납품 예정 수량 : 100
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| EBELN | CHAR(10) | 구매 문서 번호 (PO) | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 번호 | 00010 |
| ETENR | NUMC(4) | 스케줄 라인 번호 | 0001 |
| EINDT | DATS | 납기일 | 20260320 |
| MENGE | QUAN(13,3) | 납품 예정 수량 | 100 |
| WEMNG | QUAN(13,3) | 입고된 수량 | 50 |
| DABMG | QUAN(13,3) | 납품 완료 수량 | 100 |
| LPEIN | DEC(5) | 가격 단위 | 1 |
| FIXKZ | CHAR(1) | 납기 고정 여부 | X |
| BEDAT | DATS | 문서 생성일 | 20260310 |

</br>

### 4. EKBE (Purchasing Document History)

- 구매오더(PO)의 이력 정보(입고, 송장, 반품 등 구매 문서와 관련된 모든 후속 처리 기록)를 저장하는 MM 구매 이력 테이블

```
PR (EBAN)
↓
PO Header (EKKO)
↓
PO Item (EKPO)
↓
Schedule Line (EKET)
↓
PO History (EKBE)
↓
GR (MSEG / MATDOC) / Invoice (RBKP, RSEG)

ex)
PO 번호 : 4500001234  
품목 : 00010  
이력 유형 : GR (Goods Receipt)  
입고 수량 : 100  
문서 번호 : 5000012345
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| EBELN | CHAR(10) | 구매 문서 번호 (PO) | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 번호 | 00010 |
| ZEKKN | NUMC(2) | 계정 지정 번호 | 01 |
| VGABE | CHAR(1) | 이력 유형 (GR, Invoice 등) | 1 |
| BELNR | CHAR(10) | 문서 번호 | 5000012345 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 문서 아이템 번호 | 001 |
| BUDAT | DATS | 전기일 | 20260320 |
| MENGE | QUAN(13,3) | 수량 | 100 |
| DMBTR | CURR(13,2) | 금액 | 1200000 |
| WAERS | CUKY(5) | 통화 | KRW |
| SHKZG | CHAR(1) | 차변/대변 구분 | S |
</br>

### 5. EBAN (Purchase Requisition)
- 구매요청(PR)의 품목(Item) 정보(요청 자재, 수량, 요청일, 플랜트 등)를 저장하는 MM 구매요청 테이블

```
PR Item (EBAN)
↓
PO Header (EKKO)
↓
PO Item (EKPO)
↓
Schedule Line (EKET)
↓
PO History (EKBE)

ex)
PR 번호 : 1000001234  
PR 품목 : 00010  
자재 : MAT-10001  
요청 수량 : 100  
요청 플랜트 : 1100  
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BANFN | CHAR(10) | 구매요청 번호 (PR) | 1000001234 |
| BNFPO | NUMC(5) | 구매요청 품목 번호 | 00010 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| MENGE | QUAN(13,3) | 요청 수량 | 100 |
| MEINS | UNIT(3) | 기본 단위 | EA |
| BADAT | DATS | PR 생성일 | 20260310 |
| LFDAT | DATS | 요청 납기일 | 20260320 |
| EKGRP | CHAR(3) | 구매 그룹 | 001 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| LOEKZ | CHAR(1) | 삭제 표시 |  |

</br>

### 6. EINA (Purchasing Info Record – General Data)
- 공급업체와 자재 간의 구매 정보 레코드(Info Record) 기본 정보를 저장하는 MM 테이블  
  (어떤 공급업체가 어떤 자재를 공급하는지 관리)

```
Vendor (LFA1)
↓
Material (MARA)
↓
Purchasing Info Record (EINA)
↓
Purchasing Org Data (EINE)
↓
PO (EKKO / EKPO)

ex)
Info Record : 5300000123  
공급업체 : 100000  
자재 : MAT-10001
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| INFNR | CHAR(10) | 구매 정보 레코드 번호 | 5300000123 |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| ERDAT | DATS | 생성 날짜 | 20260310 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| LOEKZ | CHAR(1) | 삭제 표시 |  |
| URZLA | CHAR(1) | 출처 표시 |  |
| IDNLF | CHAR(35) | 공급업체 자재 번호 | SUP-MAT-01 |

</br>

### 7. EINE (Purchasing Info Record – Purchasing Organization Data)
- 구매 정보 레코드(Info Record)의 구매조직 기준 구매 조건 정보 
  (가격, 통화, Incoterms, 최소 주문 수량 등)을 저장하는 MM 테이블

```
Vendor (LFA1)
↓
Material (MARA)
↓
Purchasing Info Record (EINA)
↓
Purchasing Org Data (EINE)
↓
PO (EKKO / EKPO)

ex)
Info Record : 5300000123  
구매조직 : 1000  
구매 가격 : 12,000  
통화 : KRW  
최소 주문 수량 : 100
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| INFNR | CHAR(10) | 구매 정보 레코드 번호 | 5300000123 |
| EKORG | CHAR(4) | 구매 조직 | 1000 |
| ESOKZ | CHAR(1) | 구매 정보 유형 | 0 |
| NETPR | CURR(11,2) | 순 가격 | 12000 |
| WAERS | CUKY(5) | 통화 | KRW |
| PEINH | DEC(5) | 가격 단위 | 1 |
| MOQTY | QUAN(13,3) | 최소 주문 수량 | 100 |
| INCO1 | CHAR(3) | Incoterms | FOB |
| INCO2 | CHAR(28) | Incoterms 위치 | BUSAN |
| LOEKZ | CHAR(1) | 삭제 표시 |  |

</br>
</br>
</br>

## < Invoice (송장) >

### 1. RBKP (Invoice Document Header)

- 송장(Invoice) 문서의 헤더 정보(송장번호, 공급업체, 금액, 전기일 등)를 저장하는 MM 송장 헤더 테이블

```
PR (EBAN)
      ↓
PO (EKKO / EKPO)
      ↓
GR (MSEG / MATDOC)
      ↓
RBKP (송장 헤더)
      ↓
RSEG (송장 품목)

ex)
송장번호 : 5100001234  
공급업체 : 100000  
송장금액 : 1,200,000  
통화 : KRW  
전기일 : 20260321
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BELNR | CHAR(10) | 송장 문서 번호 | 5100001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| LIFNR | CHAR(10) | 공급업체 번호 | 100000 |
| BLDAT | DATS | 송장 문서일 | 20260321 |
| BUDAT | DATS | 전기일 | 20260321 |
| CPUDT | DATS | 입력 날짜 | 20260321 |
| USNAM | CHAR(12) | 입력 사용자 | SAPUSER |
| WAERS | CUKY(5) | 통화 | KRW |
| RMWWR | CURR(13,2) | 송장 총 금액 | 1200000 |
| XBLNR | CHAR(16) | 참조 송장 번호 | INV-2026-001 |

</br>

### 2. RSEG (Invoice Document Item)
- 송장(Invoice)의 품목(Item) 단위 정보(PO 품목, 수량, 금액 등)를 저장하는 MM 송장 아이템 테이블

```
PR (EBAN)
      ↓
PO (EKKO / EKPO)
      ↓
GR (MSEG / MATDOC)
      ↓
RBKP (송장 헤더)
      ↓
RSEG (송장 품목)

ex)
송장번호 : 5100001234  
품목 : 001  
PO 번호 : 4500001234  
자재 : MAT-10001  
수량 : 100 EA  
금액 : 1,200,000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| BELNR | CHAR(10) | 송장 문서 번호 | 5100001234 |
| GJAHR | NUMC(4) | 회계 연도 | 2026 |
| BUZEI | NUMC(3) | 송장 품목 번호 | 001 |
| EBELN | CHAR(10) | 구매 문서 번호 (PO) | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 번호 | 00010 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| MENGE | QUAN(13,3) | 송장 수량 | 100 |
| MEINS | UNIT(3) | 단위 | EA |
| WRBTR | CURR(13,2) | 송장 금액 | 1200000 |
| WAERS | CUKY(5) | 통화 | KRW |

</br>
</br>
</br>

## < Inventory Management (재고) >

### 1. MATDOC (Material Document)

- S/4HANA에서 자재 이동(Goods Movement) 발생 시 생성되는 **Inventory Management 핵심 테이블**로, 입고(GR), 출고(GI), 재고이동 등 모든 재고 변동을 기록

```
PO (EKKO / EKPO)
      ↓
Goods Receipt (MATDOC 생성)
      ↓
재고 증가 (MARD / MBEW 반영)

ex)
자재문서 : 5000001234  
자재 : MAT-10001  
이동유형 : 101 (입고)  
수량 : 100 EA  
플랜트 : 1100  
저장위치 : 0001
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MBLNR | CHAR(10) | 자재 문서 번호 | 5000001234 |
| MJAHR | NUMC(4) | 자재 문서 연도 | 2026 |
| ZEILE | NUMC(4) | 문서 품목 번호 | 0001 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| BWART | CHAR(3) | 이동 유형 (Movement Type) | 101 |
| SHKZG | CHAR(1) | 차변/대변 구분 | S |
| BUDAT | DATS | 전기일 | 20260320 |
| CPUDT | DATS | 입력 날짜 | 20260320 |
| MENGE | QUAN(13,3) | 이동 수량 | 100 |
| MEINS | UNIT(3) | 기본 단위 | EA |
| DMBTR | CURR(13,2) | 금액 | 1200000 |
| WAERS | CUKY(5) | 통화 | KRW |
| EBELN | CHAR(10) | 구매 문서 번호 (PO) | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 | 00010 |
| LIFNR | CHAR(10) | 공급업체 | 100000 |
| CHARG | CHAR(10) | Batch 번호 | BATCH001 |

</br>

### 2. MARD (Storage Location Stock)

- 플랜트 내 저장위치(Storage Location) 기준으로 자재의 재고 수량을 관리하는 Inventory Management 재고 테이블

```
MATDOC (자재 이동 발생)
      ↓
재고 수량 변경
      ↓
MARD (저장위치 재고 반영)

ex)
자재 : MAT-10001  
플랜트 : 1100  
저장위치 : 0001  
사용가능 재고 : 500 EA  
검사중 재고 : 20 EA  
Blocked 재고 : 10 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| LABST | QUAN(13,3) | 사용 가능 재고(Unrestricted Stock) | 500 |
| UMLME | QUAN(13,3) | 이동 중 재고 (Stock in Transfer) | 50 |
| INSME | QUAN(13,3) | 검사 중 재고 (Quality Inspection Stock) | 20 |
| SPEME | QUAN(13,3) | Blocked 재고 | 10 |
| LFGJA | NUMC(4) | 마지막 재고 변경 연도 | 2026 |
| LFMON | NUMC(2) | 마지막 재고 변경 월 | 03 |


</br>

### 3. MCHA (Batch Master)

- 자재의 배치(Batch) 정보를 저장하는 테이블로, 동일 자재라도 생산일·유통기한 등 배치별 특성을 관리

```
Material (MARA)
      ↓
Batch 생성 (MCHA)
      ↓
재고 관리 (MCHB / MATDOC)

ex)
자재 : MAT-10001  
배치 : BATCH001  
플랜트 : 1100  
제조일 : 20260301  
유통기한 : 20270301
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| CHARG | CHAR(10) | 배치 번호 | BATCH001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| ERSDA | DATS | 배치 생성일 | 20260310 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| HSDAT | DATS | 제조일 | 20260301 |
| VFDAT | DATS | 유통기한 | 20270301 |
| ZUSCH | CHAR(1) | 배치 상태 | A |

</br>

### 4. MCHB (Batch Stock)

- 자재의 배치(Batch) 기준 재고 수량을 저장하는 Inventory Management 테이블  
(같은 자재라도 Batch별로 재고를 따로 관리)

```
MARA (자재)
      ↓
MCHA (Batch 정보)
      ↓
MCHB (Batch별 재고)
      ↓
MATDOC (자재 이동 기록)

ex)
자재 : MAT-10001  
Batch : BATCH001  
플랜트 : 1100  
저장위치 : 0001  
사용가능 재고 : 300 EA  
검사중 재고 : 50 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| CHARG | CHAR(10) | Batch 번호 | BATCH001 |
| CLABS | QUAN(13,3) | 사용 가능 재고 | 300 |
| CINSM | QUAN(13,3) | 품질 검사 재고 | 50 |
| CSPEM | QUAN(13,3) | Blocked 재고 | 10 |
| CEINM | QUAN(13,3) | 입고 예정 수량 | 40 |
| LFGJA | NUMC(4) | 마지막 재고 변경 연도 | 2026 |
| LFMON | NUMC(2) | 마지막 재고 변경 월 | 03 |

</br>
</br>
</br>

## < Logistics / Plant >

### 1. T001W (Plant)
- SAP에서 플랜트(Plant) 조직 정보를 저장하는 Logistics 조직구조 테이블  
(생산, 재고, 구매, 판매 등 물류 활동이 수행되는 기본 단위)

```
Company Code (T001)
        ↓
Plant (T001W)
        ↓
Storage Location (T001L)

ex)
플랜트 : 1100  
플랜트 이름 : SEOUL PLANT  
회사코드 : 1000  
국가 : KR  
도시 : SEOUL
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| WERKS | CHAR(4) | 플랜트 코드 | 1100 |
| NAME1 | CHAR(30) | 플랜트 이름 | SEOUL PLANT |
| BWKEY | CHAR(4) | 평가 영역 | 1100 |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| ADRNR | CHAR(10) | 주소 번호 | 0000123456 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| REGIO | CHAR(3) | 지역 | 11 |
| PSTLZ | CHAR(10) | 우편번호 | 04524 |
| ORT01 | CHAR(35) | 도시 | SEOUL |

</br>

### 2. T001L (Storage Location)
- 플랜트(Plant) 내에서 자재를 실제로 보관하는 저장위치(Storage Location) 정보를 저장하는 Logistics 조직 테이블

```
Company Code (T001)
        ↓
Plant (T001W)
        ↓
Storage Location (T001L)
        ↓
Stock (MARD)

ex)
플랜트 : 1100  
저장위치 : 0001  
저장위치 이름 : MAIN WAREHOUSE  
회사코드 : 1000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| LGOBE | CHAR(16) | 저장 위치 이름 | MAIN WAREHOUSE |
| BUKRS | CHAR(4) | 회사 코드 | 1000 |
| XBLGO | CHAR(1) | 음수 재고 허용 여부 |  |
| XLONG | CHAR(1) | 저장 위치 활성화 여부 | X |
| KUNNR | CHAR(10) | 고객 번호 (특수 재고) |  |
| LIFNR | CHAR(10) | 공급업체 번호 (특수 재고) |  |

</br>
</br>
</br>


## < Quality Management (QM) >

### 1. QALS (Inspection Lot)
- 품질 검사 시 생성되는 검사 Lot(Inspection Lot) 기본 정보를 저장하는 QM 핵심 테이블  
(GR 검사, 공정 검사, 출하 검사 등 모든 품질검사의 기준 문서)

```
PO (EKKO / EKPO)
      ↓
Goods Receipt (MATDOC)
      ↓
Inspection Lot 생성 (QALS)
      ↓
품질 검사 수행
      ↓
Usage Decision (품질 승인/불량)

ex)
Inspection Lot : 100000123456  
자재 : MAT-10001  
플랜트 : 1100  
Batch : BATCH001  
검사 수량 : 100 EA  
PO : 4500001234
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PRUEFLOS | CHAR(12) | 검사 Lot 번호 | 100000123456 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| CHARG | CHAR(10) | Batch 번호 | BATCH001 |
| HERKUNFT | CHAR(2) | 검사 Lot 생성 유형 (Inspection Origin) | 01 |
| ERSTELDAT | DATS | 검사 Lot 생성일 | 20260320 |
| MENGE | QUAN(13,3) | 검사 대상 수량 | 100 |
| MEINS | UNIT(3) | 단위 | EA |
| STAT35 | CHAR(1) | 사용 결정 완료 여부 | X |
| LIFNR | CHAR(10) | 공급업체 | 100000 |
| EBELN | CHAR(10) | 구매 문서 번호 | 4500001234 |
| EBELP | NUMC(5) | 구매 문서 품목 | 00010 |

</br>

### 2. QAMV (Inspection Characteristic Results)
- 검사 Lot에서 수행되는 검사 항목(Characteristic) 정보를 저장하는 QM 테이블  
(각 검사 항목의 목표값, 검사 방식, 허용 범위 등)

```
Goods Receipt (MATDOC)
      ↓
Inspection Lot 생성 (QALS)
      ↓
Inspection Characteristics (QAMV)
      ↓
Inspection Results (QASE)

ex)
Inspection Lot : 100000123456  
검사 항목 : pH Level  
목표값 : 7.0  
허용 범위 : 6.5 ~ 7.5  
샘플 수량 : 10
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PRUEFLOS | CHAR(12) | 검사 Lot 번호 | 100000123456 |
| VORGLFNR | NUMC(4) | 검사 작업 번호 | 0010 |
| MERKNR | NUMC(4) | 검사 항목 번호 | 0001 |
| MERKBEZ | CHAR(40) | 검사 항목 설명 | pH Level |
| SOLLWERT | CHAR(20) | 목표 값 | 7.0 |
| UNTOL | CHAR(10) | 최소 허용 값 | 6.5 |
| OBERTOL | CHAR(10) | 최대 허용 값 | 7.5 |
| PRUEFMETHODE | CHAR(8) | 검사 방법 | PH_TEST |
| STICHPR | QUAN(13,3) | 샘플 수량 | 10 |

</br>

### 3. AMR (Inspection Result)
- 검사 Lot에서 수행된 실제 검사 결과값(Result)을 저장하는 QM 테이블  
(검사 항목별 측정값, 합격/불합격 판단 등)

```
Goods Receipt (MATDOC)
      ↓
Inspection Lot 생성 (QALS)
      ↓
Inspection Characteristic (QAMV)
      ↓
Inspection Result 저장 (QAMR)
      ↓
Usage Decision

ex)
Inspection Lot : 100000123456  
검사 항목 : pH Level  
측정값 : 7.1  
판정 : 합격 (A)  
검사자 : QMUSER
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PRUEFLOS | CHAR(12) | 검사 Lot 번호 | 100000123456 |
| VORGLFNR | NUMC(4) | 검사 작업 번호 | 0010 |
| MERKNR | NUMC(4) | 검사 항목 번호 | 0001 |
| PRUEFER | CHAR(12) | 검사 수행자 | QMUSER |
| ISTWERT | CHAR(20) | 실제 측정값 | 7.1 |
| KURZTEXT | CHAR(40) | 검사 결과 설명 | OK |
| BEWERTUNG | CHAR(1) | 결과 평가 (합격/불합격) | A |
| ERDAT | DATS | 결과 입력일 | 20260321 |

</br>

### 4. QAVE (Usage Decision)
- 검사 Lot에 대한 최종 사용결정(Usage Decision) 결과를 저장하는 QM 테이블  
(검사 결과에 따라 승인, 조건부 승인, 폐기 등의 판정을 기록)

```
Goods Receipt (MATDOC)
      ↓
Inspection Lot 생성 (QALS)
      ↓
Inspection Characteristics (QAMV)
      ↓
Inspection Results (QAMR)
      ↓
Usage Decision (QAVE)

ex)
Inspection Lot : 100000123456  
사용결정 : Accepted  
승인 수량 : 100 EA  
불량 수량 : 0 EA  
결정자 : QMUSER
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PRUEFLOS | CHAR(12) | 검사 Lot 번호 | 100000123456 |
| VCODE | CHAR(4) | 사용결정 코드 | A001 |
| VKATART | CHAR(1) | 카탈로그 유형 | A |
| VTEXT | CHAR(40) | 사용결정 설명 | Accepted |
| ERSTELLER | CHAR(12) | 사용결정 수행자 | QMUSER |
| ERDAT | DATS | 사용결정 날짜 | 20260321 |
| VEGRU | CHAR(3) | 사용결정 그룹 | UD1 |
| LMENGE | QUAN(13,3) | 승인 수량 | 100 |
| AMENGE | QUAN(13,3) | 불량 수량 | 0 |

</br>

### 5. QMAT (Inspection Setup for Material)
- 자재(Material)에 대해 어떤 품질검사를 수행할지 정의하는 **검사 설정(Inspection Setup)** 정보를 저장하는 QM 테이블

```
Material (MARA)
      ↓
Inspection Setup (QMAT)
      ↓
Goods Receipt 발생
      ↓
Inspection Lot 생성 (QALS)
      ↓
품질 검사 수행

ex)
자재 : MAT-10001  
플랜트 : 1100  
검사 유형 : 01 (GR Inspection)  
검사 활성화 : X
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| ART | CHAR(2) | 검사 유형 (Inspection Type) | 01 |
| AKTIV | CHAR(1) | 검사 활성화 여부 | X |
| STICHPRVER | CHAR(8) | 샘플링 절차 | SPC001 |
| DYNREGEL | CHAR(4) | 동적 수정 규칙 | DR01 |
| ERSTELLER | CHAR(12) | 생성 사용자 | QMUSER |
| ERDAT | DATS | 생성 날짜 | 20260301 |

</br>

### 6. QPAM (Inspection Characteristic Master)
- 품질 검사에서 사용되는 검사 특성(Inspection Characteristic) 마스터 데이터를 저장하는 QM 테이블  
(검사 항목 이름, 단위, 검사 방식 등 품질 검사 기준 정의)

```
Material Master (MARA)  
  ↓  
Inspection Setup (QMAT)  
  ↓  
Inspection Lot 생성 (QALS)  
  ↓  
Inspection Characteristic 정의 (QPAM)  
  ↓  
Inspection Result 입력 (QAMR / QAMV)

ex)
검사 특성 : 점도 검사  
단위 : CPS  
목표값 : 500  
허용 범위 : 450 ~ 550
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MERKNR | NUMC(4) | 검사 특성 번호 | 0001 |
| KURZTEXT | CHAR(40) | 검사 특성 설명 | 점도 검사 |
| ATNAM | CHAR(30) | 특성 이름 | VISCOSITY |
| MSEHI | UNIT(3) | 측정 단위 | CPS |
| SOLLWERT | DEC(13,3) | 목표 값 | 500 |
| TOLERANZOB | DEC(13,3) | 상한 허용치 | 550 |
| TOLERANZUN | DEC(13,3) | 하한 허용치 | 450 |
| PRUEFART | CHAR(4) | 검사 유형 | 01 |
| ERDAT | DATS | 생성일 | 20260325 |

</br>

### 7. # QPMK (Master Inspection Characteristic)
- 품질 검사에서 사용하는 검사 특성(Master Inspection Characteristic, MIC) 정보를 저장하는 QM 핵심 마스터 테이블  
(검사 항목 이름, 측정 단위, 목표값 등 품질 검사 기준 정의)

```
Material Master (MARA)  
  ↓  
Inspection Setup (QMAT)  
  ↓  
Inspection Characteristic Master (QPMK)  
  ↓  
Inspection Lot 생성 (QALS)  
  ↓  
Inspection Result 입력 (QAMR / QAMV)

ex)
검사 특성 : 점도 검사  
MIC 번호 : MIC00001  
단위 : CPS  
목표값 : 500  
허용 범위 : 450 ~ 550
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MKNR | CHAR(8) | 검사 특성 번호 (MIC 번호) | MIC00001 |
| KURZTEXT | CHAR(40) | 검사 특성 설명 | 점도 검사 |
| ATNAM | CHAR(30) | 특성 이름 | VISCOSITY |
| MSEHI | UNIT(3) | 측정 단위 | CPS |
| SOLLWERT | DEC(13,3) | 목표 값 | 500 |
| TOLERANZOB | DEC(13,3) | 상한 허용치 | 550 |
| TOLERANZUN | DEC(13,3) | 하한 허용치 | 450 |
| PRUEFART | CHAR(4) | 검사 유형 | 01 |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | QMUSER |

</br>
</br>

---

</br>

# < SAP PP 주요 Standard Tables >
</br>

## < Production Order >

### 1. AFKO (Production Order Header)
- 생산오더(Production Order)의 헤더 정보(생산 자재, 수량, 일정 등)를 저장하는 PP 생산오더 헤더 테이블

```
MRP 실행
      ↓
Production Order 생성 (AFKO)
      ↓
Operation 정보 (AFVC)
      ↓
Component 예약 (RESB)
      ↓
Goods Issue
      ↓
Goods Receipt (MATDOC)

ex)
생산오더 : 5000012345  
자재 : MAT-10001  
플랜트 : 1100  
생산 수량 : 1000 EA  
계획 시작일 : 20260320  
계획 종료일 : 20260325
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFNR | CHAR(12) | 생산 오더 번호 | 5000012345 |
| AUART | CHAR(4) | 오더 유형 | PP01 |
| MATNR | CHAR(18) | 생산 자재 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| GAMNG | QUAN(13,3) | 생산 총 수량 | 1000 |
| GMEIN | UNIT(3) | 생산 단위 | EA |
| GSTRP | DATS | 계획 시작일 | 20260320 |
| GLTRP | DATS | 계획 종료일 | 20260325 |
| DISPO | CHAR(3) | MRP 담당자 | 001 |
| RSNUM | NUMC(10) | 예약 번호 | 2000003456 |

</br>

### 2. AFPO (Production Order Item)
- 생산오더의 품목(Item) 정보를 저장하는 PP 테이블로, 생산 자재·수량·플랜트·판매오더 연계 등 생산 대상 정보를 관리

```
MRP 실행
      ↓
Production Order 생성 (AFKO)
      ↓
Production Order Item (AFPO)
      ↓
Component 예약 (RESB)
      ↓
Goods Issue
      ↓
Goods Receipt (MATDOC)

ex)
생산오더 : 5000012345  
품목 : 0001  
자재 : MAT-10001  
플랜트 : 1100  
생산 수량 : 1000 EA  
입고 수량 : 500 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFNR | CHAR(12) | 생산 오더 번호 | 5000012345 |
| POSNR | NUMC(4) | 오더 품목 번호 | 0001 |
| MATNR | CHAR(18) | 생산 자재 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| PSMNG | QUAN(13,3) | 생산 오더 수량 | 1000 |
| WEMNG | QUAN(13,3) | 입고 완료 수량 | 500 |
| MEINS | UNIT(3) | 기본 단위 | EA |
| DWERK | CHAR(4) | 납품 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| CHARG | CHAR(10) | Batch 번호 | BATCH001 |
| KDAUF | CHAR(10) | 판매 오더 번호 | 6000001234 |
| KDPOS | NUMC(6) | 판매 오더 품목 | 000010 |
| DAUAT | CHAR(4) | 오더 유형 | PP01 |

</br>

### 3. AFVC (Production Order Operation)
- 생산오더의 공정(Operation) 정보를 저장하는 PP 테이블로, 작업순서·워크센터·공정번호 등 생산 작업 단위를 관리

```
Production Order (AFKO)
      ↓
Production Order Item (AFPO)
      ↓
Operations (AFVC)
      ↓
Component Reservation (RESB)
      ↓
Confirmation (AFRU)
      ↓
Goods Receipt (MATDOC)

ex)
생산오더 : 5000012345  
공정 : 0010  
공정 설명 : Mixing  
플랜트 : 1100  
워크센터 : MIX_LINE_01
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFPL | NUMC(10) | 작업 계획 번호 (Routing 연결 키) | 1000123456 |
| APLZL | NUMC(8) | 공정 내부 카운터 | 00000001 |
| VORNR | CHAR(4) | 공정 번호 | 0010 |
| LTXA1 | CHAR(40) | 공정 설명 | Mixing |
| ARBID | NUMC(8) | Work Center 내부 ID | 50000123 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| STEUS | CHAR(4) | 제어 키 (Control Key) | PP01 |
| LAR01 | CHAR(6) | 활동 유형 | LABOR |
| OBJNR | CHAR(22) | 오브젝트 번호 | OR000500001234 |

</br>

### 4. AFVV (Operation Quantities / Times)
- 생산오더 공정(Operation)의 시간·수량 관련 정보(가동시간, 준비시간, 작업시간 등)를 저장하는 PP 테이블

```
Production Order (AFKO)
      ↓
Operation (AFVC)
      ↓
Operation Time / Quantity (AFVV)
      ↓
Confirmation (AFRU)

ex)
생산오더 : 5000012345  
공정 : 0010  
준비시간 : 1.5 H  
가동시간 : 4.0 H  
작업시간 : 3.0 H
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFPL | NUMC(10) | 작업 계획 번호 | 1000123456 |
| APLZL | NUMC(8) | 공정 내부 카운터 | 00000001 |
| VGE01 | QUAN(13,3) | 준비 시간 (Setup Time) | 1.5 |
| VGE02 | QUAN(13,3) | 가동 시간 (Machine Time) | 4.0 |
| VGE03 | QUAN(13,3) | 작업 시간 (Labor Time) | 3.0 |
| VGW01 | QUAN(13,3) | 준비 작업 값 | 1.5 |
| VGW02 | QUAN(13,3) | 가동 작업 값 | 4.0 |
| VGW03 | QUAN(13,3) | 작업 값 | 3.0 |
| MEINH | UNIT(3) | 시간 단위 | H |

</br>

### 5. AUFK (Order Master Data)
- 생산오더·내부오더 등 오더의 기본 마스터 정보(오더 유형, 생성일, 상태 등)를 저장하는 CO/PP 공통 오더 마스터 테이블

```
Order Master (AUFK)
      ↓
Production Order Header (AFKO)
      ↓
Production Order Item (AFPO)
      ↓
Operations (AFVC)

ex)
오더번호 : 5000012345  
오더유형 : PP01  
플랜트 : 1100  
오더 설명 : Cosmetic Production  
생성일 : 20260320
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| AUFNR | CHAR(12) | 오더 번호 | 5000012345 |
| AUART | CHAR(4) | 오더 유형 | PP01 |
| ERDAT | DATS | 오더 생성일 | 20260320 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
| KTEXT | CHAR(40) | 오더 설명 | Cosmetic Production |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| KOSTV | CHAR(10) | 책임 코스트센터 | CC1000 |
| OBJNR | CHAR(22) | 오브젝트 번호 | OR000500001234 |
| PHAS0 | CHAR(1) | 오더 생성 상태 | X |
| PHAS1 | CHAR(1) | 오더 릴리즈 상태 | X |
| PHAS2 | CHAR(1) | 오더 완료 상태 |  |

</br>
</br>
</br>

## < BOM (Bill of Material) >

### 1. STKO (BOM Header)
- BOM(Bill of Material)의 헤더 정보를 저장하는 PP 테이블로, 어떤 자재의 BOM인지와 사용 용도, 유효기간 등을 관리

```
Material (MARA)
      ↓
BOM Header (STKO)
      ↓
BOM Item (STPO)
      ↓
Production Order (AFKO)

ex)
자재 : MAT-10001  
BOM 번호 : 00001234  
대안 BOM : 01  
기준 수량 : 1 EA  
유효 시작일 : 20260301
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| STLTY | CHAR(1) | BOM 카테고리 | M |
| STLNR | CHAR(8) | BOM 번호 | 00001234 |
| STLAL | CHAR(2) | BOM 대안(Alternative BOM) | 01 |
| DATUV | DATS | 유효 시작일 | 20260301 |
| AENNR | CHAR(12) | 변경 번호 | ECN00001 |
| BMENG | QUAN(13,3) | 기준 수량 | 1 |
| BMEIN | UNIT(3) | 기준 단위 | EA |
| STLAN | CHAR(1) | BOM 사용 용도 | 1 |
| STKTX | CHAR(40) | BOM 설명 | Lipstick BOM |

</br>

### 2. STPO (BOM Item)
- BOM(Bill of Material)의 구성 품목(Component) 정보를 저장하는 PP 테이블로, 생산에 필요한 원자재·부품 목록과 수량을 관리

```
Material (MARA)
      ↓
BOM Header (STKO)
      ↓
BOM Item (STPO)
      ↓
Production Order 생성 (AFKO)

ex)
완제품 : MAT-10001  
구성 자재 : MAT-20001  
필요 수량 : 2 EA  
BOM Item : 0010
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| STLTY | CHAR(1) | BOM 카테고리 | M |
| STLNR | CHAR(8) | BOM 번호 | 00001234 |
| STLKN | NUMC(8) | BOM Item Node 번호 | 00000001 |
| POSNR | NUMC(4) | BOM Item 번호 | 0010 |
| IDNRK | CHAR(18) | 구성 자재 번호 | MAT-20001 |
| MENGE | QUAN(13,3) | 필요 수량 | 2 |
| MEINS | UNIT(3) | 단위 | EA |
| POSTP | CHAR(1) | 품목 유형 (Item Category) | L |
| DATUV | DATS | 유효 시작일 | 20260301 |

</br>

### 3. MAST (Material to BOM Link)
- 자재(Material)와 BOM(Bill of Material)을 연결하는 테이블로, 어떤 자재가 어떤 BOM을 사용하는지와 플랜트 기준 BOM 정보를 관리

```
Material (MARA)
      ↓
Material ↔ BOM 연결 (MAST)
      ↓
BOM Header (STKO)
      ↓
BOM Item (STPO)
      ↓
Production Order 생성 (AFKO)

ex)
자재 : MAT-10001  
플랜트 : 1100  
BOM 번호 : 00001234  
대안 BOM : 01  
BOM 사용 : 생산용
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| STLAN | CHAR(1) | BOM 사용 용도 | 1 |
| STLNR | CHAR(8) | BOM 번호 | 00001234 |
| STLAL | CHAR(2) | 대안 BOM | 01 |
| LOSVN | QUAN(13,3) | 최소 Lot Size | 1 |
| LOSBS | QUAN(13,3) | 최대 Lot Size | 1000 |

</br>
</br>
</br>

## < Routing (제품을 생산하기 위한 작업 순서와 공정 정보를 정의한 생산 공정 데이터) >

### 1. PLKO (Routing Header)
- Routing의 헤더 정보를 저장하는 PP 테이블로, 어떤 자재가 어떤 공정(Routing)을 사용하는지와 기본 정보(플랜트, 사용용도 등)를 관리

```
Material (MARA)
      ↓
Routing Header (PLKO)
      ↓
Routing Operation (PLPO)
      ↓
Production Order 생성 (AFKO)

ex)
자재 : MAT-10001  
Routing 그룹 : 00001234  
플랜트 : 1100  
유효 시작일 : 20260301  
Routing 설명 : Lipstick Production Routing
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PLNTY | CHAR(1) | Task List 타입 (Routing 등) | N |
| PLNNR | CHAR(8) | Routing 그룹 번호 | 00001234 |
| PLNAL | CHAR(2) | Routing 그룹 카운터 | 01 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| DATUV | DATS | 유효 시작일 | 20260301 |
| AENNR | CHAR(12) | 변경 번호 | ECN00001 |
| VERWE | CHAR(1) | 사용 용도 | 1 |
| STATU | CHAR(1) | 상태 | 4 |
| KTEXT | CHAR(40) | Routing 설명 | Lipstick Production Routing |

</br>

### 2. PLPO (Routing Operation)
- Routing의 공정(Operation) 정보를 저장하는 PP 테이블로, 각 공정 단계와 작업내용·워크센터·소요시간 등을 관리

```
Material (MARA)
      ↓
Routing Header (PLKO)
      ↓
Routing Operation (PLPO)
      ↓
Production Order 생성 (AFKO)

ex)
Routing 그룹 : 00001234  
공정 : 0010  
공정 설명 : Mixing  
워크센터 : MIX_LINE_01  
준비시간 : 1.5 H  
가동시간 : 4.0 H  
작업시간 : 3.0 H
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PLNTY | CHAR(1) | Task List 타입 | N |
| PLNNR | CHAR(8) | Routing 그룹 번호 | 00001234 |
| PLNAL | CHAR(2) | Routing 그룹 카운터 | 01 |
| VORNR | CHAR(4) | 공정 번호 | 0010 |
| ARBID | NUMC(8) | Work Center 내부 ID | 50000123 |
| LTXA1 | CHAR(40) | 공정 설명 | Mixing |
| STEUS | CHAR(4) | 제어 키 (Control Key) | PP01 |
| VGW01 | QUAN(13,3) | 준비 시간 | 1.5 |
| VGW02 | QUAN(13,3) | 가동 시간 | 4.0 |
| VGW03 | QUAN(13,3) | 작업 시간 | 3.0 |
| MEINH | UNIT(3) | 시간 단위 | H |

</br>

### 3. MAPL (Material ↔ Routing Link)
- 자재(Material)와 Routing(Task List)을 연결하는 테이블로, 어떤 자재가 어떤 Routing을 사용하는지 관리

```
Material (MARA)  
      ↓  
Material ↔ Routing 연결 (MAPL)  
      ↓  
Routing Header (PLKO)  
      ↓  
Routing Operation (PLPO)  
      ↓  
Production Order 생성 (AFKO)

ex)
자재 : MAT-10001  
플랜트 : 1100  
Routing 그룹 : 00001234  
Routing 카운터 : 01  
유효 시작일 : 20260301 
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| PLNTY | CHAR(1) | Task List 타입 | N |
| PLNNR | CHAR(8) | Routing 그룹 번호 | 00001234 |
| PLNAL | CHAR(2) | Routing 그룹 카운터 | 01 |
| LOSVN | QUAN(13,3) | 최소 Lot Size | 1 |
| LOSBS | QUAN(13,3) | 최대 Lot Size | 1000 |
| DATUV | DATS | 유효 시작일 | 20260301 |

</br>
</br>
</br>

## < MRP / Planning >

### 1. MDKP (MRP Document Header)
- MRP 실행 시 생성되는 MRP 문서의 헤더 정보를 저장하는 테이블로, 어떤 플랜트에서 언제 MRP가 실행되었는지 관리

```
Material (MARA)
      ↓
MRP 실행
      ↓
MRP Document Header (MDKP)
      ↓
MRP Planning Elements (MDTB / MDVM)

ex)
MRP 문서 : 0000123456  
플랜트 : 1100  
MRP 담당자 : 001  
실행 날짜 : 20260321  
실행 시간 : 10:30:15
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| DTNUM | NUMC(10) | MRP 문서 번호 | 0000123456 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| DISPO | CHAR(3) | MRP 담당자 | 001 |
| DATUM | DATS | MRP 실행 날짜 | 20260321 |
| UZEIT | TIMS(6) | MRP 실행 시간 | 103015 |
| ERNAM | CHAR(12) | 생성 사용자 | MRP_RUN |
| PLSCN | NUMC(3) | 시뮬레이션 번호 | 000 |
| PLMOD | CHAR(1) | 계획 모드 | 1 |

</br>

### 2. MDTB (MRP Planning Elements)
- MRP 실행 결과 생성되는 MRP 계획 요소(Planning Elements) 정보를 저장하는 테이블로, 자재의 수요·공급 계획 데이터를 관리

```
Material (MARA)
      ↓
MRP 실행
      ↓
MRP Document Header (MDKP)
      ↓
MRP Planning Elements (MDTB)
      ↓
PR 생성 (EBAN) / Production Order 생성 (AFKO)

ex)
MRP 문서 : 0000123456  
자재 : MAT-10001  
플랜트 : 1100  
계획 수량 : 1000 EA  
MRP 요소 : PA (Planned Order)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| DTNUM | NUMC(10) | MRP 문서 번호 | 0000123456 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| BEDAT | DATS | 계획 생성일 | 20260321 |
| MNG01 | QUAN(13,3) | 계획 수량 | 1000 |
| MEINS | UNIT(3) | 단위 | EA |
| DELKZ | CHAR(2) | MRP 요소 유형 | PA |
| BANFN | CHAR(10) | 구매요청 번호 | 1000001234 |
| AUFNR | CHAR(12) | 생산 오더 번호 | 5000012345 |

</br>

### 3. RESB (Reservation / Dependent Requirements)
- 생산오더, MRP, 네트워크 등에서 자재가 사용될 예정인 예약(Reservation) 자재 정보를 저장하는 PP/MM 핵심 테이블  
(어떤 오더에서 어떤 자재가 얼마 필요하고 어느 창고에서 사용할지 관리)

```
MRP 실행  
      ↓  
생산오더 생성 (AFKO / AFPO)  
      ↓  
필요 자재 예약 생성 (RESB)  
      ↓  
자재 출고 (MSEG / MATDOC)

ex)
생산오더 : 100000123456  
필요 자재 : RM-10001 (원료)  
필요 수량 : 50 KG  
저장 위치 : 0001  
출고 유형 : 261
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| RSNUM | NUMC(10) | 예약 번호 (Reservation Number) | 0001234567 |
| RSPOS | NUMC(4) | 예약 품목 번호 | 0001 |
| MATNR | CHAR(18) | 자재 번호 | RM-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| BDTER | DATS | 필요일 (Requirement Date) | 20260401 |
| BDMNG | QUAN(13,3) | 요구 수량 | 50 |
| MEINS | UNIT(3) | 기본 단위 | KG |
| AUFNR | CHAR(12) | 생산오더 번호 | 100000123456 |
| BWART | CHAR(3) | 이동 유형 | 261 |


</br>
</br>
</br>

## < Production Execution (생산계획(PP)에서 만든 생산오더를 실제로 작업하고 결과를 기록하는 단계) >

### 1. AFRU (Confirmation)
- 생산오더 공정(Operation)에 대한 작업 완료 확인(Confirmation) 정보를 저장하는 PP Production Execution 테이블  
(작업 완료 시간, 생산 수량, 불량 수량 등 기록)

```
Production Order (AFKO)
      ↓
Operations (AFVC)
      ↓
Operation Time (AFVV)
      ↓
Confirmation (AFRU)
      ↓
Goods Receipt (MATDOC)

ex)
생산오더 : 5000012345  
공정 : 0010  
생산 수량 : 900 EA  
불량 수량 : 100 EA
전기일 : 20260322
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| RUECK | NUMC(10) | 확인 번호 (Confirmation Number) | 0000123456 |
| RMZHL | NUMC(8) | 확인 카운터 | 00000001 |
| AUFNR | CHAR(12) | 생산 오더 번호 | 5000012345 |
| VORNR | CHAR(4) | 공정 번호 | 0010 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| ARBID | NUMC(8) | Work Center 내부 ID | 50000123 |
| BUDAT | DATS | 전기일 | 20260322 |
| ISM01 | QUAN(13,3) | 실제 생산 수량 | 900 |
| XMNGA | QUAN(13,3) | 불량 수량 | 100 |
| MEINH | UNIT(3) | 단위 | EA |
| ERNAM | CHAR(12) | 입력 사용자 | PROD_USER |

</br>

### 2. CRHD (Work Center Header)
- 생산 공정에서 사용되는 워크센터(Work Center)의 기본 정보를 저장하는 PP 테이블  
(작업장, 설비, 생산라인 등 생산 작업이 수행되는 장소)

```
Work Center (CRHD)
      ↓
Routing Operation (PLPO)
      ↓
Production Order Operation (AFVC)
      ↓
Confirmation (AFRU)

ex)
워크센터 : MIX_LINE  
플랜트 : 1100  
설명 : Mixing Line  
애플리케이션 : PP
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| OBJID | NUMC(8) | Work Center 내부 ID | 50000123 |
| ARBPL | CHAR(8) | Work Center 코드 | MIX_LINE |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| VERWE | CHAR(1) | 사용 용도 | 1 |
| KTEXT | CHAR(40) | Work Center 설명 | Mixing Line |
| KAPPL | CHAR(2) | 애플리케이션 | PP |
| LVORM | CHAR(1) | 삭제 표시 |  |
| AEDAT | DATS | 마지막 변경일 | 20260320 |

</br>

### 3. CRCA (Work Center Capacity Allocation)
- 워크센터(Work Center)에 할당된 Capacity(작업 가능 능력) 정보를 저장하는 PP 테이블  
(작업장에 몇 명/몇 시간/얼마의 생산능력이 있는지 관리)

```
Work Center (CRHD)
      ↓
Capacity Allocation (CRCA)
      ↓
Routing (PLPO)
      ↓
Production Order (AFKO)
      ↓
Operation Scheduling

ex)
워크센터 : MIX_LINE  
플랜트 : 1100  
Capacity : 8 H  
Capacity Category : Machine
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| OBJID | NUMC(8) | Work Center 내부 ID | 50000123 |
| KAPID | NUMC(8) | Capacity ID | 70000001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| KAPART | CHAR(3) | Capacity Category | 001 |
| KAPAZ | QUAN(13,3) | 가용 Capacity | 8 |
| MEINS | UNIT(3) | Capacity 단위 | H |
| DATUV | DATS | 유효 시작일 | 20260301 |

</br>
</br>

---

</br>

# < SAP SD 주요 Standard Tables >

</br>

## < Sales Document (판매 문서) >

### 1. VBAK (Sales Document Header)
- 판매 문서(Sales Order, Quotation 등)의 헤더 정보를 저장하는 SD 핵심 테이블  
(고객, 판매조직, 문서유형, 주문일 등 판매 문서의 기본 정보 관리)

```
Customer (KNA1)  
      ↓  
Sales Order 생성 (VBAK)  
      ↓  
Sales Order Item (VBAP)  
      ↓  
Delivery (LIKP / LIPS)  
      ↓  
Billing (VBRK / VBRP)

ex)
판매문서 : 6000001234  
문서유형 : OR (Sales Order)  
고객 : 200000  
판매조직 : 1000  
주문금액 : 1,500,000 KRW
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| AUART | CHAR(4) | 판매 문서 유형 | OR |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 00 |
| ERDAT | DATS | 문서 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |
| KUNNR | CHAR(10) | 고객 번호 | 200000 |
| NETWR | CURR(15,2) | 총 주문 금액 | 1500000 |
| WAERK | CUKY(5) | 통화 | KRW |

</br>

### 2. VBAP (Sales Document Item)
- 판매 문서(Sales Order 등)의 품목(Item) 정보를 저장하는 SD 핵심 테이블  
(판매 자재, 주문 수량, 플랜트 등 판매 품목 데이터를 관리)

```
Customer (KNA1)
      ↓
Sales Order Header (VBAK)
      ↓
Sales Order Item (VBAP)
      ↓
Delivery (LIKP / LIPS)
      ↓
Billing (VBRK / VBRP)

ex)
판매문서 : 6000001234  
품목 : 000010  
자재 : MAT-10001  
주문 수량 : 100 EA  
플랜트 : 1100
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| POSNR | NUMC(6) | 판매 문서 품목 번호 | 000010 |
| MATNR | CHAR(18) | 자재 번호 | MAT-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| KWMENG | QUAN(13,3) | 주문 수량 | 100 |
| VRKME | UNIT(3) | 판매 단위 | EA |
| NETWR | CURR(15,2) | 품목 금액 | 500000 |
| WAERK | CUKY(5) | 통화 | KRW |
| ERDAT | DATS | 생성일 | 20260325 |


</br>

### 3. VBEP (Sales Document Schedule Line)
- 판매 문서 품목(VBAP)의 납기 일정(Schedule Line: 납기일, 수량 등) 정보를 저장하는 SD 스케줄 라인 테이블

```
Sales Order Header (VBAK)  
  ↓  
Sales Order Item (VBAP)  
  ↓  
Schedule Line (VBEP)  
  ↓  
Delivery (LIKP / LIPS)  
  ↓  
Billing (VBRK / VBRP)

ex)
판매문서 : 6000001234  
품목 : 000010  
스케줄 라인 : 0001  
납기일 : 20260401  
납품 예정 수량 : 100 EA
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| POSNR | NUMC(6) | 판매 문서 품목 번호 | 000010 |
| ETENR | NUMC(4) | 스케줄 라인 번호 | 0001 |
| EINDT | DATS | 납기일 | 20260401 |
| BMENG | QUAN(13,3) | 확정 수량 | 100 |
| WMENG | QUAN(13,3) | 주문 수량 | 100 |
| VRKME | UNIT(3) | 판매 단위 | EA |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| EDATU | DATS | 고객 요청 납기일 | 20260401 |


</br>

### 4. VBUK (Sales Document Header Status)
- 판매 문서(Sales Order, Delivery, Billing 등)의 전체 상태(Status) 정보를 저장하는 SD 헤더 상태 테이블  
(문서 처리 상태, 납품 상태, 청구 상태 등 판매 문서 진행 상태 관리)

```
Sales Order (VBAK / VBAP)  
  ↓  
Sales Document Status (VBUK)  
  ↓  
Delivery 생성 여부 판단  
  ↓  
Billing 가능 여부 판단

ex)
판매문서 : 6000001234  
전체 상태 : C (완료)  
납품 상태 : B (부분 납품)  
청구 상태 : A (미청구)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| GBSTK | CHAR(1) | 전체 처리 상태 | C |
| LFSTK | CHAR(1) | 납품 상태 | B |
| FKSTK | CHAR(1) | 청구 상태 | A |
| UVALL | CHAR(1) | 전체 불완전 상태 | C |
| UVVLK | CHAR(1) | 납품 관련 불완전 상태 | C |
| UVFAK | CHAR(1) | 청구 관련 불완전 상태 | C |
| COSTA | CHAR(1) | 비용 계산 상태 | C |
| CMGST | CHAR(1) | 신용 상태 | A |


</br>

### 5. VBUP (Sales Document Item Status)
- 판매 문서(Sales Order, Delivery 등)의 품목(Item) 단위 상태(Status) 정보를 저장하는 SD 상태 테이블  
(품목별 납품 상태, 청구 상태, 처리 상태 등을 관리)

```
Sales Order Header (VBAK)  
  ↓  
Sales Order Item (VBAP)  
  ↓  
Item Status 관리 (VBUP)  
  ↓  
Delivery / Billing 진행 여부 판단

ex)
판매문서 : 6000001234  
품목 : 000010  
품목 처리 상태 : C (완료)  
납품 상태 : B (부분 납품)  
청구 상태 : A (미청구)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| POSNR | NUMC(6) | 판매 문서 품목 번호 | 000010 |
| GBSTA | CHAR(1) | 품목 전체 처리 상태 | C |
| LFSTA | CHAR(1) | 납품 상태 | B |
| FKSTA | CHAR(1) | 청구 상태 | A |
| BESTA | CHAR(1) | 구매 상태 | C |
| KOSTA | CHAR(1) | 비용 계산 상태 | C |
| UVALL | CHAR(1) | 전체 불완전 상태 | C |
| UVVLK | CHAR(1) | 납품 관련 불완전 상태 | C |

</br>

### 6. VBFA (Sales Document Flow)
- SAP SD에서 판매 문서 간 연결 관계(문서 흐름)를 저장하는 테이블  
(예: Sales Order → Delivery → Billing 간 문서 연결 추적)

```
Sales Order (VBAK / VBAP)  
  ↓  
Document Flow (VBFA)  
  ↓  
Delivery (LIKP / LIPS)  
  ↓  
Billing (VBRK / VBRP)

ex)
선행 문서 : 6000001234 (Sales Order)  
후속 문서 : 8000005678 (Delivery)  

선행 문서 : 8000005678 (Delivery)  
후속 문서 : 9000004321 (Billing)
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELV | CHAR(10) | 선행 문서 번호 (Preceding Document) | 6000001234 |
| POSNV | NUMC(6) | 선행 문서 품목 번호 | 000010 |
| VBELN | CHAR(10) | 후속 문서 번호 (Subsequent Document) | 8000005678 |
| POSNN | NUMC(6) | 후속 문서 품목 번호 | 000010 |
| VBTYP_V | CHAR(1) | 선행 문서 유형 | C |
| VBTYP_N | CHAR(1) | 후속 문서 유형 | J |
| ERDAT | DATS | 생성일 | 20260325 |
| ERZET | TIMS | 생성 시간 | 103500 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |

</br>

### 7. VBPA (Sales Document Partner)
- 판매 문서(Sales Order, Delivery, Billing 등)에 연결된 파트너(고객, 배송처, 청구처 등) 정보를 저장하는 SD 파트너 테이블

```
Customer Master (KNA1)  
  ↓  
Sales Order 생성 (VBAK)  
  ↓  
Partner 정보 저장 (VBPA)  
  ↓  
Delivery / Billing 문서에도 동일 파트너 정보 사용

ex)
판매문서 : 6000001234  
파트너 역할 : AG (Sold-to Party)  
고객 : 200000  

판매문서 : 6000001234  
파트너 역할 : WE (Ship-to Party)  
배송처 고객 : 200010
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| POSNR | NUMC(6) | 판매 문서 품목 번호 (000000 = 헤더) | 000000 |
| PARVW | CHAR(2) | 파트너 역할 (AG 고객, WE 배송처 등) | AG |
| KUNNR | CHAR(10) | 고객 번호 | 200000 |
| LIFNR | CHAR(10) | 공급업체 번호 | 300000 |
| PERNR | NUMC(8) | 담당 직원 번호 | 10000123 |
| ADRNR | CHAR(10) | 주소 번호 | 50001234 |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| ERDAT | DATS | 생성일 | 20260325 |

</br>

### 8. VBKD (Sales Document Business Data)
- 판매 문서(Sales Order 등)의 비즈니스 거래 정보(결제조건, Incoterms, 고객 주문번호 등)를 저장하는 SD 테이블

```
Customer Master (KNA1)  
  ↓  
Sales Order 생성 (VBAK / VBAP)  
  ↓  
Business Data 저장 (VBKD)  
  ↓  
Delivery (LIKP / LIPS)  
  ↓  
Billing (VBRK / VBRP)

ex)
판매문서 : 6000001234  
고객 주문번호 : PO-90001  
결제조건 : 0001 (30일 지급)  
Incoterms : FOB BUSAN
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 판매 문서 번호 | 6000001234 |
| POSNR | NUMC(6) | 판매 문서 품목 번호 (000000 = Header) | 000000 |
| BSTKD | CHAR(35) | 고객 구매 주문 번호 | PO-90001 |
| BSTDK | DATS | 고객 주문일 | 20260325 |
| ZTERM | CHAR(4) | 지급 조건 (Payment Terms) | 0001 |
| INCO1 | CHAR(3) | Incoterms 코드 | FOB |
| INCO2 | CHAR(28) | Incoterms 위치 | BUSAN |
| KDGRP | CHAR(2) | 고객 그룹 | 01 |
| BZIRK | CHAR(6) | 영업 지역 | SEOUL |

</br>

### 9. PRCD_ELEMENTS (Pricing Condition Elements)
- S/4HANA에서 판매 문서 및 청구 문서의 가격 조건(가격, 할인, 세금 등)을 저장하는 핵심 Pricing 테이블  
(기존 ECC의 KONV 테이블을 대체)

```
Sales Order (VBAK / VBAP)  
  ↓  
Pricing Procedure 실행  
  ↓  
Pricing 결과 저장 (PRCD_ELEMENTS)  
  ↓  
Delivery / Billing 시 동일 가격 사용

ex)
판매문서 : 6000001234  
품목 : 000010  

조건)
PR00 → 기본 가격 : 10,000 KRW  
K007 → 고객 할인 : -1,000 KRW  
MWST → 세금 : 10%
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KNUMV | CHAR(10) | Pricing Document 번호 | 0000123456 |
| KPOSN | NUMC(6) | 판매 문서 품목 번호 | 000010 |
| STUNR | NUMC(3) | 조건 단계 번호 (Pricing Step) | 010 |
| ZAEHK | NUMC(2) | 조건 카운터 | 01 |
| KSCHL | CHAR(4) | 조건 유형 (가격/할인/세금 등) | PR00 |
| KBETR | CURR(11,2) | 조건 금액 (가격/할인율 등) | 10000 |
| KPEIN | DEC(5) | 가격 기준 수량 | 1 |
| KMEIN | UNIT(3) | 가격 단위 | EA |
| KWERT | CURR(15,2) | 조건 금액 값 | 100000 |
| WAERS | CUKY(5) | 통화 | KRW |
| KAWRT | CURR(15,2) | 조건 기준 금액 | 100000 |
| KRECH | CHAR(1) | 계산 유형 (금액/퍼센트 등) | C |
| KOAID | CHAR(1) | 조건 계정 키 | ERL |
| KSTAT | CHAR(1) | 통계 조건 여부 | X |
| KINAK | CHAR(1) | 조건 비활성 여부 |  |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |

</br>
</br>
</br>

## < Delivery (출고 / 배송) >

### 1. LIKP (Delivery Document Header)
- 출고/배송 문서(Delivery)의 헤더 정보를 저장하는 SD 핵심 테이블  
(출고 문서 번호, 출고일, 배송처, 배송 상태 등 관리)

```
Sales Order (VBAK / VBAP)  
      ↓  
Delivery 생성 (LIKP / LIPS)  
      ↓  
Goods Issue (출고 처리)  
      ↓  
Billing (VBRK / VBRP)

ex)
판매오더 : 6000001234  
배송문서 : 8000001234  
출하지점 : 1000  
출고일 : 20260326
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 배송 문서 번호 (Delivery Number) | 8000001234 |
| ERDAT | DATS | 문서 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |
| LFART | CHAR(4) | 배송 문서 유형 | LF |
| VSTEL | CHAR(4) | 출하지점 (Shipping Point) | 1000 |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 00 |
| KUNNR | CHAR(10) | 배송 고객 (Ship-to Party) | 200000 |
| WADAT | DATS | 실제 출고일 | 20260326 |

</br>

### 2. LIPS (Delivery Document Item)
- 출고/배송 문서(Delivery)의 품목(Item) 정보를 저장하는 SD 핵심 테이블  
(출고 자재, 출고 수량, 플랜트, 저장위치 등 배송 상세 데이터 관리)

```
Sales Order (VBAK / VBAP)  
  ↓  
Delivery 생성  
  ↓  
Delivery Header (LIKP)  
  ↓  
Delivery Item (LIPS)  
  ↓  
Goods Issue (재고 감소, MATDOC)

ex)
판매오더 : 6000001234  
배송문서 : 8000001234  
품목 : 000010  
자재 : FG-10001  
출고 수량 : 100 EA  
플랜트 : 1100
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 배송 문서 번호 (Delivery Number) | 8000001234 |
| POSNR | NUMC(6) | 배송 문서 품목 번호 | 000010 |
| MATNR | CHAR(18) | 자재 번호 | FG-10001 |
| WERKS | CHAR(4) | 플랜트 | 1100 |
| LGORT | CHAR(4) | 저장 위치 | 0001 |
| LFIMG | QUAN(13,3) | 실제 출고 수량 | 100 |
| VRKME | UNIT(3) | 판매 단위 | EA |
| CHARG | CHAR(10) | Batch 번호 | BATCH-01 |
| BWART | CHAR(3) | 이동 유형 (Movement Type) | 601 |
| VGBEL | CHAR(10) | 선행 문서 번호 (Sales Order) | 6000001234 |
| VGPOS | NUMC(6) | 선행 문서 품목 번호 | 000010 |

</br>
</br>
</br>

## < Billing (청구) >

### 1. VBRK (Billing Document Header)
- 청구 문서(Billing / Invoice)의 헤더 정보를 저장하는 SD 핵심 테이블  
(청구 문서 번호, 고객, 청구일, 통화 등 청구 문서 기본 정보 관리)

```
Sales Order (VBAK / VBAP)  
  ↓  
Delivery (LIKP / LIPS)  
  ↓  
Billing 생성 (VBRK / VBRP)

ex)
판매오더 : 6000001234  
배송문서 : 8000001234  
청구문서 : 9000001234  
청구일 : 20260326  
총 금액 : 1,500,000 KRW
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 청구 문서 번호 (Billing Document) | 9000001234 |
| FKART | CHAR(4) | 청구 문서 유형 | F2 |
| FKDAT | DATS | 청구일 (Billing Date) | 20260326 |
| ERDAT | DATS | 문서 생성일 | 20260326 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |
| KUNRG | CHAR(10) | 청구 고객 (Payer) | 200000 |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 00 |
| WAERK | CUKY(5) | 통화 | KRW |
| NETWR | CURR(15,2) | 총 청구 금액 | 1500000 |

</br>

### 2. VBRP (Billing Document Item)
- 청구 문서(Billing / Invoice)의 품목(Item) 정보를 저장하는 SD 핵심 테이블  
(청구 자재, 청구 수량, 금액 등 청구 상세 데이터 관리)

```
Sales Order (VBAK / VBAP)  
      ↓  
Delivery (LIKP / LIPS)  
      ↓  
Billing Header (VBRK)  
      ↓  
Billing Item (VBRP)

ex)
판매오더 : 6000001234  
배송문서 : 8000001234  
청구문서 : 9000001234  
자재 : FG-10001  
청구 수량 : 100 EA  
품목 금액 : 500,000 KRW
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| VBELN | CHAR(10) | 청구 문서 번호 (Billing Document) | 9000001234 |
| POSNR | NUMC(6) | 청구 문서 품목 번호 | 000010 |
| MATNR | CHAR(18) | 자재 번호 | FG-10001 |
| FKIMG | QUAN(13,3) | 청구 수량 | 100 |
| VRKME | UNIT(3) | 판매 단위 | EA |
| NETWR | CURR(15,2) | 품목 금액 | 500000 |
| WAERK | CUKY(5) | 통화 | KRW |
| VGBEL | CHAR(10) | 선행 문서 번호 (Delivery) | 8000001234 |
| VGPOS | NUMC(6) | 선행 문서 품목 번호 | 000010 |
| AUBEL | CHAR(10) | 판매 문서 번호 (Sales Order) | 6000001234 |
| AUPOS | NUMC(6) | 판매 문서 품목 번호 | 000010 |

</br>
</br>
</br>

## < Customer Master (고객 마스터) >

### 1. KNA1 (Customer Master General Data)
- 고객(Customer)의 일반 마스터 데이터(이름, 주소, 국가 등 기본 정보)를 저장하는 SD/FI 공통 고객 마스터 테이블

```
Customer Master 생성 (KNA1)  
      ↓  
Sales Order 생성 (VBAK / VBAP)  
      ↓  
Delivery (LIKP / LIPS)  
      ↓  
Billing (VBRK / VBRP)

ex)
고객 번호 : 200000  
고객 이름 : ABC COSMETIC  
국가 : KR  
도시 : SEOUL
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KUNNR | CHAR(10) | 고객 번호 | 200000 |
| NAME1 | CHAR(35) | 고객 이름 | ABC COSMETIC |
| NAME2 | CHAR(35) | 고객 이름2 | ABC Korea |
| ORT01 | CHAR(35) | 도시 | SEOUL |
| LAND1 | CHAR(3) | 국가 코드 | KR |
| REGIO | CHAR(3) | 지역 | 11 |
| PSTLZ | CHAR(10) | 우편번호 | 06236 |
| STRAS | CHAR(35) | 주소 | Teheran-ro 123 |
| TELF1 | CHAR(16) | 전화번호 | 0212345678 |
| STCD1 | CHAR(16) | 사업자 등록번호 | 1234567890 |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SDUSER |


</br>

### 2. KNB1 (Customer Company Code Data)
- 고객(Customer)의 회사코드 기준 회계 정보(지급 조건, 채권 계정 등)를 저장하는 FI 테이블이며 SD에서 Billing 후 FI 전표 생성 시 고객 회계 데이터를 참조할 때 사용됨

```
Customer Master (KNA1)  
  ↓  
Sales Area Data (KNVV)  
  ↓  
Billing 생성 (VBRK / VBRP)  
  ↓  
FI 전표 생성 시 고객 회계 정보 참조 (KNB1)  
  ↓  
Accounting Document (BKPF / BSEG)

ex)
고객 : 200000  
회사코드 : 1000  
지급조건 : 0001 (30일 지급)  
채권 계정 : 110000
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KUNNR | CHAR(10) | 고객 번호 | 200000 |
| BUKRS | CHAR(4) | 회사코드 | 1000 |
| AKONT | CHAR(10) | 고객 채권 G/L 계정 (Recon Account) | 110000 |
| ZTERM | CHAR(4) | 지급 조건 (Payment Terms) | 0001 |
| ZUAWA | CHAR(3) | 정산 키 | 001 |
| BUSAB | CHAR(4) | 사업 영역 | 1000 |
| FDGRV | CHAR(4) | 신용 관리 그룹 | 0001 |
| ALTKN | CHAR(10) | 이전 고객 번호 | 200000 |
| ERDAT | DATS | 생성일 | 20260325 |

</br>

### 3. KNVV (Customer Master Sales Area Data)
- 고객(Customer)의 판매영역(Sales Area) 기준 판매 정보를 저장하는 SD 고객 마스터 테이블  
(판매조직, 유통채널, 가격그룹, 배송조건 등 판매 관련 데이터 관리)

```
Customer Master 생성  
  ↓  
General Data (KNA1)  
  ↓  
Company Code Data (KNB1)  
  ↓  
Sales Area Data (KNVV)  
  ↓  
Sales Order 생성 (VBAK / VBAP)

ex)
고객 : 200000  
판매조직 : 1000  
유통채널 : 10  
제품군 : 00  
배송조건 : 01
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| KUNNR | CHAR(10) | 고객 번호 | 200000 |
| VKORG | CHAR(4) | 판매 조직 | 1000 |
| VTWEG | CHAR(2) | 유통 채널 | 10 |
| SPART | CHAR(2) | 제품군 | 00 |
| KDGRP | CHAR(2) | 고객 그룹 | 01 |
| BZIRK | CHAR(6) | 영업 지역 | SEOUL |
| VSBED | CHAR(2) | 배송 조건 | 01 |
| WAERS | CUKY(5) | 통화 | KRW |
| KALKS | CHAR(1) | 가격 결정 절차 | A |
| ZTERM | CHAR(4) | 지급 조건 | 0001 |

</br>

### 4. BUT000 (Business Partner General Data)
- SAP S/4HANA에서 Business Partner(BP)의 기본 정보(이름, 유형, 생성일 등)를 저장하는 핵심 BP 마스터 테이블  
(Customer, Vendor 등 모든 비즈니스 파트너의 기본 데이터를 관리)

```
Business Partner 생성 (BUT000)  
  ↓  
BP 주소 정보 (BUT020)  
  ↓  
BP 역할 설정 (BUT100)  
  ↓  
Customer / Vendor 연결  
  ↓  
SD / MM / FI에서 사용

ex)
BP 번호 : 300000  
유형 : Organization  
회사 이름 : ABC COSMETIC  
생성일 : 20260325
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 300000 |
| TYPE | CHAR(1) | BP 유형 (Person/Organization/Group) | 2 |
| BU_GROUP | CHAR(4) | BP 그룹 | Z001 |
| NAME_ORG1 | CHAR(40) | 회사 이름 | ABC COSMETIC |
| NAME_ORG2 | CHAR(40) | 회사 이름2 | ABC Korea |
| NAME_FIRST | CHAR(40) | 이름 | JAEHWAN |
| NAME_LAST | CHAR(40) | 성 | KIM |
| BIRTHDT | DATS | 생년월일 | 19980101 |
| BU_SORT1 | CHAR(20) | 검색 키 | ABC |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |

</br>

### 5. BUT020 (Business Partner Address Data)
- Business Partner(BP)의 주소 정보(주소 번호, 주소 유형 등)를 저장하는 BP 주소 테이블  
(BP 기본 정보 BUT000과 연결되어 실제 주소 데이터를 관리)

```
Business Partner 생성 (BUT000)  
  ↓  
주소 정보 저장 (BUT020)  
  ↓  
실제 주소 상세 (ADRC 테이블)  
  ↓  
Customer / Vendor 문서에서 주소 사용

ex)
BP 번호 : 300000  
주소 번호 : 0000123456  
기본 주소 : X  
유효 기간 : 20260301 ~ 99991231
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 300000 |
| ADDRNUMBER | CHAR(10) | 주소 번호 | 0000123456 |
| ADDRTYPE | CHAR(1) | 주소 유형 | 1 |
| VALID_FROM | DATS | 주소 유효 시작일 | 20260301 |
| VALID_TO | DATS | 주소 유효 종료일 | 99991231 |
| STDADDRESS | CHAR(1) | 기본 주소 여부 | X |
| XDFADR | CHAR(1) | 주소 삭제 플래그 |  |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |

</br>

### 6. BUT100 (Business Partner Role)
- Business Partner(BP)에 부여된 역할(Role: Customer, Vendor 등) 정보를 저장하는 BP 역할 테이블  
(BP가 어떤 비즈니스 역할을 수행하는지 관리)

```
Business Partner 생성 (BUT000)  
  ↓  
BP 주소 정보 (BUT020)  
  ↓  
BP 역할 설정 (BUT100)  
  ↓  
Customer / Vendor 마스터 생성

ex)
BP 번호 : 300000  
역할 : FLCU01 (Customer)  
유효 기간 : 20260301 ~ 99991231
```

| Field | Type | Description | Example |
|------|------|-------------|---------|
| MANDT | CLNT(3) | 클라이언트 | 100 |
| PARTNER | CHAR(10) | Business Partner 번호 | 300000 |
| RLTYP | CHAR(6) | BP 역할 유형 | FLCU01 |
| VALID_FROM | DATS | 역할 유효 시작일 | 20260301 |
| VALID_TO | DATS | 역할 유효 종료일 | 99991231 |
| ERDAT | DATS | 생성일 | 20260325 |
| ERNAM | CHAR(12) | 생성 사용자 | SAPUSER |
