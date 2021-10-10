# Confidence Regions for Information-Theoretic Descriptors of Time Series

#### [Eduarda C. Chagas](mailto:eduarda.chagas@dcc.ufmg.br), Marcelo Queiroz, [Osvaldo A. Rosso](mailto:oarosso@gmail.com), [Heitor S. Ramos](mailto:ramosh@dcc.ufmg.br), Christopher G. S. Freitas, Leonardo V. Pereira, and [Alejandro C. Frery](mailto:acfrery@laccan.ufal.br)

### This repository contains all the data and code used to develop our research in the related [paper submitted](<https://www.dropbox.com/s/k7fnogysrbj8vl4/IT-ConfidenceRegions.pdf?dl=0>) to ISR. 

---

#### Abstract

The Bandt and Pompe methodology has been used successfully in the analysis of time series. It consists of computing information theory descriptors, using a histogram of ordinal patterns, which are found in a 2D variety: the Entropy-Complexity plane. So far, the analysis of the dynamics underlying the time series has been carried out from two reference points: those corresponding to a deterministic time series and a series of white noise. In this article, we provide a first proposal for the construction of empirical confidence regions in the Entropy-Complexity plane for white noise models and we use these regions to ver-
ify whether we can capture the randomness of PRNGs in short sequences. The proposed methodology showed consistency and coherence in its results, managing to discriminate sequences of true random samples, capturing the randomness of generators previously analyzed in the literature and proving to be robust the addition of correlation structures.

The empirical confidence regions resulting from this work are found in the following files: [N = 1000](https://github.com/EduardaChagas/ConfidenceRegions/blob/master/Data/Regions-HC/regions-hc-N1000.csv) and [N = 50000](https://github.com/EduardaChagas/ConfidenceRegions/blob/master/Data/Regions-HC/regions-hc-N50000.csv).

#### Overall Framework

<img src="/Images/methodology.png" />

The structure of our proposal is shown in the figure above. It consists of two steps:

  1. Empirical confidence region: With the data present in Euclidean plane, we can easily calculate empirical regions
that involve the data with a certain level of confidence.
  
  2. Construction of a test statistic: To measure the similarity of new data sequences with the empirical points, a test
statistic was proposed. By acquiring a p-value less than 0.5, we can reject the null hypothesis, which states that
such data belong to the empirical probability distribution used for the construction of the confidence region.

#### Dataset

In this study we used two sources of random numbers, here called true random, both from physical phenomena observation and measurement. The first is based on vacuum states to generate random quantum numbers, the setup consists of an ordinary laser source to generate a local oscillator (LO), a half-wave plate, a polarizing beamsplitter (BPS), and two balanced detectors working together adding or subtracting the photocurrents results in a quadrature measurement of the LO or vacuum state. The probability distribution of the vacuum state is binned into 2 n equal parts (bins of same size), than, assigning a fixed bit combination of length n to each sample point in a given bin (Gabriel et al., 2010). The second one is based on atmospheric noise captured by a cheap radio receiver, started as a gambling engine, the randomness comes from an ordinary radio receiver that has no filter for static unwanted sounds caused by atmospheric noise, but perfect for random purposes, developed over a distributed setup with some radios located at different geographical locations sending random bits to a cloud server who process data and hosts random.org, the history, and some other information could be found at (Haahr, 1998–2018).

#### The repository is organized as follows:

- `/Code` - the scripts used to develop our research; 
- `/Data` - the auxiliary data used during analysis; 
- `/Images`- Illustration of the results obtained throughout the research, alongside the methodology files corresponding to our *overview* figure; 
- `/Reports`- the scientific reports developed during the study. 

The other data necessary for the execution of the algorithms can be found in this link: [Drive/ConfidenceRegions](https://drive.google.com/drive/folders/18ZuFbLZedp5aPWfreBD45nm_SossK3cC?usp=sharing).

### Software requirements

This code version is tested on the Linux operating system Ubuntu 18.10.

**Installing R 3.6.0 on Ubuntu 18.10**

```sh
$ sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu disco-cran35/'
$ sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
$ sudo apt update
$ sudo apt install r-base-dev
```

### Installation Guide

Prior to running the script experiments, we need to install the following required packages for R: 

``
install.packages(c('readr', 'ggplot2', 'ggthemes', 'ggpubr', 'gtools', 'cowplot', 'sp', 'plot3D', 'doParallel', 'fftw', 'ggrepel', 'latex2exp', 'reshape2', 'grid', 'gridExtra', 'dqrng'))
``

The latest version of these packages was used by October 2019:

```
readr          1.4.0
ggplot2        3.3.2   
ggthemes       4.2.0  
ggpubr         0.4.0
gtools         3.8.2
cowplot        1.1.0  
sp             1.4.4
plot3D         1.3
doParallel     1.0.16
fftw           1.0.6
ggrepel        0.8.2
latex2exp      0.4.0
reshape2       1.4.4
grid           3.6.3
gridExtra      2.3
dqrng          0.2.1
```
---


Finally, if you have any questions or you want to report anything, feel free to reach me at: eduarda.chagas@dcc.ufmg.br. 
