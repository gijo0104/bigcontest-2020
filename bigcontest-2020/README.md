# 홈쇼핑 편성 최적화 프로젝트

# Introduction

---

**주제** | 홈쇼핑 판매 실적 예측 모델을 개발하고 이를 활용해 매출액을 극대화할 수 있는 방송 편성표 제안

**기간** | 2020.08 ~ 2021.09 (2개월)

**성과** | 모델 에러율은 baseline 모델 대비 28%감소, 최적 방송편성표를 통한 예상매출액 5% 증가

**역할** | 

- 홈쇼핑 홈페이지의 상품 정보 및 별점 데이터 크롤링하여 Feature로 사용
- 외부 데이터 수집 및 파생변수 생성, 검증 후 Feature로 사용
- R로 다양한 XGBoost 모델을 앙상블하여 매출액 예측 모델 개발
- 헝가리안 알고리즘으로 예상 매출액을 최대화 하는 방송 편성표 제안
- 방송 상품 수가 2개 이상인 경우, 각 상품별 노출시간 최적화

**결과** | 2020 빅콘테스트 데이터분석분야 챔피언리그 대상 수상 (1위)

# Directories

---

```bash
├── EDA_plot
├── code
│   ├── main.R
│   ├── Hungary_algorithm.R
│   ├── EDA_plot.R
│   ├── crawling_nsshop.R
│   ├── crawling_nielsen.R
│   └── api_holiday.R
├── data
│   ├── 01_제공데이터
│   ├── External Data
│   └── final
├── problems.pdf
└── report.pdf
```

- code/
    - **main.R : 데이터 전처리부터 모델링 후 prediction 값을 저장하는 메인 코드**
    - **Hungary_algorithm.R : 헝가리안 알고리즘을 이용한 최적 방송 편성표 산출 코드**
    - EDA_plot.R : 보고서에 필요한 EDA plot을 그리고 저장하는 파일로 EDA_plot 디렉토리에 저장
    - crawling_nsshop.R : 홈쇼핑 홈페이지의 상품정보 및 별점 데이터를 크롤링하는 코드
    - crawling_nielsen.R : 닐슨의 일별 시청률 데이터를 크롤링하는 코드
    - api_holiday.R : 공휴일 데이터를 가지고 오는 코드
- problems.pdf : 문제 출제 내용(detail)
- **report.pdf : 최종 보고 자료**