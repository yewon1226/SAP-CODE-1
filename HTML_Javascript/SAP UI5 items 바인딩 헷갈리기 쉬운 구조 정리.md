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
