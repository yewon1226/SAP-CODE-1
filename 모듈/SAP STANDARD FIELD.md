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

### 6. BSAK (Vendor Cleared Items)
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

### 7. BSID (Customer Open Items)

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

### 8. BSAD (Customer Cleared Items)

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

### 9. SKA1 (G/L Account Master – Chart of Accounts Level)

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
