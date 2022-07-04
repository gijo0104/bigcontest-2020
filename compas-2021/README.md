# 서울시 도시데이터 센서(S-DoT) 위치선정

## Introduction

**주제** | 도시데이터 센서 현황정보 등을 활용하여 추가로 필요한 센서의 위치와 측정 항목 도출

**기간** | 2021.08~2021.08 (1개월)

**성과** | 기본 센서가 측정하는 항목의 불확실성을 줄이고 보다 정확하고 필요한 지점의 위치 선정

**역할** | 

- 공간 회귀 모델인 크리깅 모델을 통해 기본 센서가 측정하는 항목들의 불확실성 측정
- 기본 센서가 측정하는 항목별 가중치를 옵션으로 주어 전문가 의견에 따른 조정 가능
- 불확실성이 높은 곳을 최대한 커버할 수 있도록 추가 센서 위치를 선정하기 위해 MCLP(최대지역커버문제)를 Mixed Integer Linear Programming 으로 최적화

**결과** | COMPAS (서울시) 도시데이터 센서(S-DoT) 위치선정 우수상(2위)

## Directories

```bash
├── notebook code.ipynb
├── report.pdf
└── table of content.txt
```

- **notebook code.ipynb : 도시데이터 센서 위치 선정 코드**
- **report.pdf : 최종 보고 자료**
- table of content.txt : 주피터 노트북 파일의 목차 명시
