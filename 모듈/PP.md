# SAP ERP - PP (Production Planning) Module 정리
- SAP PP 모듈은 제품의 생산 계획 수립부터 최종 완제품 생산까지의 모든 과정을 관리하는 모듈
- 제조업의 핵심인 "무엇을, 언제, 얼마나 생산할 것인가"를 시스템적으로 제어

</br>

## 1) Direct Shipment (직송 수출)
- 미국 사람이 미국 지사에서 물건을 샀는데, 물건은 한국 공장에서 바로 날아가는 방식

#### 1) 프로세스 요약
- 주문: 미국 법인이 주문 접수
- 배송: 한국 공장에서 미국 고객으로 직수출
- 경로: 한국 공장 ➔ 인천공항 ➔ 미국 SFO공항 ➔ 미국 고객

#### 2) 역할 (SAP 모듈)
- 판매(SD): 미국 법인이 주문서(IC Sales Order) 생성
- 창고(EWM): 한국 공장에서 물건 집기(피킹) 및 포장(패킹)
- 운송(TM): 비행기 배차, 통관, 운송비 관리
</br>

<img width="940" height="495" alt="image" src="https://github.com/user-attachments/assets/ceea73de-4b9e-42c9-8c0d-5f01e01cfe9f" />
</br>
</br>
</br>
</br>

## 1-2) Direct Shipment 상세 프로세스 

#### 1) 주문/출하 (S/4 HANA ERP)
- 미국 법인이 IC Sales Order를 만들면 한국 법인에서 Delivery Order(D/O)가 생성

#### 2) 창고 작업 (S/4 HANA EWM)
- D/O 정보를 받아 물건을 찾고(피킹/포장), 트럭에 싣고(상차), 마지막으로 출고 확정(GI 확정)

#### 3) 운송 관리 (S/4 HANA TM)
- 국내: 공장에서 공항(ICN)까지 차량 배정 및 운송을 관리
- 국제: 항공사 예약(Freight Booking), 통관 신고, 운송 추적(Tracking)을 담당
- 현지: 미국 공항(SFO) 도착 후 고객 집까지 배송을 마무리
  
<img width="944" height="483" alt="image" src="https://github.com/user-attachments/assets/fa30e80a-a217-48a7-b259-36d77caa32d1" />
</br>
</br>
</br>
</br>

## 2) SAP S/4HANA 생산 전략 (Production Strategies)
- MTS(기성품) → ATO(조립) → MTO(주문) → ETO(맞춤 설계) 순으로 고객 맞춤화 수준이 높아짐

#### 1) MTS (Make to Stock, 재고 생산)
- 표준 제품을 미리 대량 생산하여 재고를 쌓아두고 판매하는 방식
- 제품별 고정된 BOM 구조를 가짐
- 프로세스: 판매계획 ➔ 구매 ➔ 생산 ➔ 시험 ➔ 출하
  
#### 2) ATO (Assemble to Order, 주문 조립)
- 주요 부품(모듈)은 미리 생산해두고, 고객 주문이 들어오면 조립만 하는 방식
- MTS와 MTO의 중간 형태
- 프로세스: 수주 ➔ 구매 ➔ 생산 ➔ 시험 ➔ 출하

#### 3) MTO (Make to Order, 주문 생산)
- 고객 주문이 확정된 후 생산을 시작하는 방식
- 다양한 옵션을 반영하기 위해 Variant Configuration(선택 사양 구성) 체계와 Super BOM/Routing을 활용
- 프로세스: 수주 ➔ 구매 ➔ 생산 ➔ 시험 ➔ 출하

#### 4) ETO (Engineer to Order, 수주 설계 생산)
- 설계 단계부터 고객의 요구사항을 반영하여 생산하는 방식
- 프로젝트 생산 체계로 관리하며, WBS(작업 분할 구조)와 연동\
- 프로세스: 견적 ➔ 수주 ➔ 설계 ➔ 구매 ➔ 생산 ➔ 시험 ➔ 출하 ➔ 설치 ➔ C/S
</br>

<img width="1083" height="544" alt="image" src="https://github.com/user-attachments/assets/cccc0721-3862-42b0-b523-92fc98323421" />
</br>
</br>

<img width="1090" height="580" alt="image" src="https://github.com/user-attachments/assets/e82f9581-8ec2-4af0-90ea-c66610b1825a" />
</br>
</br>
</br>
</br>

## SAP 생산관리(PP)의 4대 핵심 요소
#### 1) 생산 계획 (Planning)
- 비즈니스 특성에 맞춰 언제, 무엇을, 얼마나 만들지 결정
- 수요 관리(DM)와 자재 소요 계획(MRP)이 이 단계의 핵심

#### 2) 생산 실행 (Execution)
- 실제 공장에서 제품을 만드는 단계
- 작업 지시, 생산 실적 관리 등이 포함되며 조립/장치 산업 등 공정 특성에 맞게 지원됨

#### 3) 기준 정보 (Master Data)
- 생산의 기본이 되는 데이터
- 어떤 부품이 들어가는지(BOM), 어떤 순서로 만드는지(Routing) 등을 관리

#### 4) 결과 분석 (Analytics)
- 생산이 잘 되었는지 실시간으로 모니터링하고 분석

</br>

<img width="1090" height="584" alt="image" src="https://github.com/user-attachments/assets/cd2f353b-9b58-48b1-9307-30f9cd18edae" />
</br>
</br>
</br>
</br>


<img width="976" height="514" alt="image" src="https://github.com/user-attachments/assets/bb53589f-3728-4911-8068-7382081b4a06" />
