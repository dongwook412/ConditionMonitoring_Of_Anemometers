# 이상치 탐지를 이용한 구간 고장 탐지

### Motivation
* 2019학년도 2학기 한양대학교 산업공학과 내 신뢰성 및 보전공학 수업 프로젝트
* 높이가 같은 두 풍속계 데이터들을 통해, 정상 구간만을 가지고 모델을 생성하여 고장 구간을 예측하였다.

### Data
* [phm11 data challenge - condition monitoring of anemometers](https://www.phmsociety.org/competition/phm/11/problem)
* 데이터는 전처리를 통해 정상 구간을 추출한다. 대부분의 데이터가 정상 구간이므로 이들을 통해 이상치 탐지 모델을 생성하고, 고장 구간과 정상 구간을 사용하여 모델의 성능을 검증한다.

### Version
* R-studio v3.5.2

### Method
* Wavelet transforms
* Spectrum
* OCSVM

### Report
* [Report](https://drive.google.com/file/d/1j4_wepFaCxXAY0mMBIfpAwMpyBrhtfhe/view?usp=sharing)
