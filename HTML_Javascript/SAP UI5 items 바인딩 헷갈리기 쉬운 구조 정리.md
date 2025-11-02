# 정상 코드
### 1) 객체 안에 배열
- 가장 일반적인 형태
- 모델 생성 코드
- ```java
  { list: [ {name: "A"}, {name: "B"} ] }
  ```
- XML 바인딩 방법
- ```html
  items="{model>/list}"
  ```
- Text 바인딩
- ```html
  {model>name}
  ```
</br>

### 2) 객체 안에 숫자 배열
- 숫자, 문자열은 `{.}`
- 모델 생성 코드
- ```java
  { list: [1, 2, 3] }
  ```
- XML 바인딩 방법
- ```html
  items="{model>/list}"
  ```
- Text 바인딩
- ```html
  {.}
  ```
</br>

### 3) 루트가 배열
- 루트 배열은 `/`로 접근
- 모델 생성 코드
- ```java
  [ {name: "A"}, {name: "B"} ]
  ```
- XML 바인딩 방법
- ```html
  items="{model>/}"
  ```
- Text 바인딩
- ```html
  {model>name}
  ```
</br>

### 4) 루트가 배열
- 가장 단순한 형태
- 모델 생성 코드
- ```java
  [1, 2, 3]
  ```
- XML 바인딩 방법
- ```html
  items="{model>/}"
  ```
- Text 바인딩
- ```html
  {.}
  ```
</br>

# 잘못된 코드
### 1) 객체 자체가 배열처럼 생긴 객체
- 문법 오류
- ```java
  {[1, 2]}
  ```
</br>

### 2) 바인딩 경로 틀림
- 존재하지 않는 경로
- ```java
  [1, 2, 3]인데 items="{model>/list}"
  ```
</br>
