# 자동차 재구매 유형 구분 로직 및 재구매 가능성(Scroe) 예측 모델 개발

# Introduction

---

**주제** | 고객 구매 데이터를 통해 자동차 재구매 유형(대차, 추가구매)을 구분 및 재구매 가능성 점수 모델링

**기간** | 2021.04~2021.06(2개월)

**성과** | 예측 모델을 통한 재구매 긍정 반응 비율이 3.7%에서 8.3%로 상승. 2.24배 효과적인 타겟 마케팅 가능.

**역할** | 

- 최초구매나 추가구매 이후 일련의 대차 과정을 하나로 묶어 섹션(section)을 정의
- 섹션 정의 후 섹션 종료일자와 재구매 출고일자를 기준으로 대차와 추가구매 구분 로직 개발
- python으로 XGBoost 예측 모델을 통해 재구매 가능성 점수 모델 개발

**결과** | 1차 심사 통과 (2차 참가 X)

# Directories

---

```bash
├── problems.pdf
├── code_problem1.ipynb
├── code_problem2.ipynb
├── report_problem1.pptx
└── report_problem2.pptx
```

- problems.pdf : 문제출제내용 (detail)
- **code_problem1.ipynb : 자동차 재구매 유형을 구분하기 위한 로직 개발 및 EDA 과정 코드**
- **code_problem2.ipynb : 재구매 가능성 예측 모델 개발 코드**
- **report_problem1.pptx : 자동차 재구매 유형 구분 로직 최종 보고 자료**
- **report_problem2.pptx : 재구매 가능성 예측 모델 개발 최종 보고 자료**