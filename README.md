Using solitude for anomaly detection
====================================

-   Author: Srikanth Komala Sheshachala

The R package [solitude](https://cran.r-project.org/package=solitude)
implements **Isolation forest**, an anomaly detection method introduced
by the paper Isolation based Anomaly Detection ([Liu, Ting and
Zhou](https://dl.acm.org/citation.cfm?id=2133363)). Isolation forest is
grown using [ranger](https://cran.r-project.org/package=ranger)[1]
package.

‘Isolation forest’ is a multivariate outlier detection technique for IID
data containing a mix of numeric and categorical variables.

Usage
-----

    library("solitude")

    data("humus", package = "mvoutlier")
    columns_required = setdiff(colnames(humus)
                               , c("Cond", "ID", "XCOO", "YCOO", "LOI")
                               )
    humus2 = humus[ , columns_required]
    dplyr::glimpse(humus2)

    ## Rows: 617
    ## Columns: 39
    ## $ Ag <dbl> 0.124, 0.130, 0.124, 0.309, 0.040, 0.421, 0.235, 0.043, 0.070, 0.0…
    ## $ Al <dbl> 1420, 7190, 1970, 6360, 630, 2060, 1200, 1510, 3380, 1320, 2420, 1…
    ## $ As <dbl> 0.743, 0.973, 1.100, 1.240, 0.533, 1.540, 0.836, 0.657, 1.750, 1.0…
    ## $ B  <dbl> 2.51, 3.45, 1.62, 1.59, 4.33, 1.93, 2.51, 4.46, 5.08, 2.10, 2.18, …
    ## $ Ba <dbl> 74.0, 65.2, 121.0, 63.5, 36.0, 111.0, 109.0, 24.5, 42.2, 51.4, 103…
    ## $ Be <dbl> 0.02, 0.08, 0.04, 0.07, 0.01, 0.07, 0.01, 0.05, 0.06, 0.01, 0.06, …
    ## $ Bi <dbl> 0.108, 0.084, 0.137, 0.135, 0.076, 0.176, 0.145, 0.075, 0.259, 0.1…
    ## $ Ca <dbl> 3900, 1900, 3240, 2470, 3500, 3340, 3010, 3090, 1760, 2810, 3790, …
    ## $ Cd <dbl> 0.298, 0.390, 0.295, 0.222, 0.214, 0.327, 0.294, 0.180, 0.295, 0.2…
    ## $ Co <dbl> 1.28, 3.37, 0.98, 3.91, 0.43, 4.61, 0.75, 0.55, 1.68, 1.12, 1.36, …
    ## $ Cr <dbl> 3.04, 2.02, 1.33, 17.00, 0.79, 3.78, 2.01, 1.63, 10.90, 1.71, 2.20…
    ## $ Cu <dbl> 10.90, 12.30, 5.59, 29.80, 6.23, 47.30, 8.66, 5.55, 10.00, 9.25, 5…
    ## $ Fe <dbl> 2050, 3170, 1100, 6180, 790, 3370, 1150, 1800, 4820, 970, 1570, 15…
    ## $ Hg <dbl> 0.155, 0.263, 0.168, 0.175, 0.197, 0.239, 0.233, 0.217, 0.238, 0.2…
    ## $ K  <dbl> 1100, 900, 1000, 900, 700, 1200, 1000, 600, 800, 1000, 700, 1100, …
    ## $ La <dbl> 2.3, 6.4, 1.7, 5.3, 1.2, 7.0, 1.5, 2.9, 6.3, 1.2, 1.9, 1.8, 1.2, 1…
    ## $ Mg <dbl> 820, 670, 790, 1180, 1770, 800, 660, 1750, 820, 570, 690, 690, 590…
    ## $ Mn <dbl> 202.0, 22.8, 70.3, 118.0, 24.4, 86.1, 1210.0, 16.7, 39.2, 115.0, 1…
    ## $ Mo <dbl> 0.662, 0.286, 0.166, 0.280, 0.248, 0.473, 0.185, 0.372, 0.277, 0.1…
    ## $ Na <dbl> 40, 120, 30, 150, 410, 60, 10, 320, 110, 50, 20, 50, 20, 70, 50, 3…
    ## $ Ni <dbl> 9.41, 14.80, 5.05, 59.50, 1.98, 78.00, 5.70, 2.19, 11.70, 13.20, 5…
    ## $ P  <dbl> 743, 1030, 922, 717, 795, 942, 1120, 790, 779, 745, 869, 1160, 968…
    ## $ Pb <dbl> 15.9, 13.9, 19.0, 16.3, 14.3, 21.7, 15.2, 11.6, 284.0, 14.1, 18.1,…
    ## $ Rb <dbl> 8.14, 2.82, 4.45, 8.13, 2.67, 5.23, 4.94, 2.33, 4.07, 5.35, 3.69, …
    ## $ S  <dbl> 1300, 1950, 1750, 965, 1860, 1410, 1880, 2210, 1260, 1250, 1350, 1…
    ## $ Sb <dbl> 0.120, 0.161, 0.250, 0.017, 0.169, 0.217, 0.189, 0.126, 0.809, 0.2…
    ## $ Sc <dbl> 0.6, 1.2, 0.4, 1.4, 0.2, 0.7, 0.4, 0.6, 1.1, 0.3, 0.7, 0.4, 0.6, 0…
    ## $ Si <dbl> 630, 640, 580, 690, 440, 530, 650, 490, 630, 560, 660, 740, 700, 6…
    ## $ Sr <dbl> 22.2, 34.4, 43.4, 29.2, 46.4, 67.9, 20.7, 59.1, 28.8, 23.2, 27.7, …
    ## $ Th <dbl> 0.413, 0.281, 0.246, 0.816, 0.250, 0.432, 0.371, 0.535, 1.130, 0.1…
    ## $ Tl <dbl> 0.081, 0.068, 0.077, 0.099, 0.060, 0.084, 0.088, 0.042, 0.084, 0.0…
    ## $ U  <dbl> 0.240, 0.120, 0.042, 0.163, 0.127, 0.112, 0.051, 0.265, 0.221, 0.0…
    ## $ V  <dbl> 6.79, 3.89, 2.86, 16.10, 1.63, 7.40, 3.55, 2.57, 13.90, 2.76, 3.37…
    ## $ Y  <dbl> 0.8, 2.4, 0.5, 1.9, 0.8, 1.6, 0.5, 1.8, 1.7, 0.4, 0.8, 0.7, 0.7, 0…
    ## $ Zn <dbl> 59.1, 18.1, 67.6, 43.3, 41.4, 46.0, 103.0, 21.5, 23.2, 38.5, 53.6,…
    ## $ C  <dbl> 39.9, 47.5, 44.2, 19.4, 45.8, 47.8, 45.7, 47.3, 29.5, 46.8, 44.3, …
    ## $ H  <dbl> 5.5, 6.8, 6.3, 3.0, 6.1, 5.8, 6.5, 6.4, 4.4, 6.5, 6.1, 6.3, 5.2, 5…
    ## $ N  <dbl> 1.2, 1.5, 1.5, 0.7, 1.5, 1.3, 1.7, 1.5, 0.7, 1.0, 1.1, 1.6, 1.0, 1…
    ## $ pH <dbl> 3.9, 4.1, 3.8, 4.0, 4.1, 3.7, 4.0, 4.2, 4.0, 3.6, 3.7, 3.9, 3.8, 3…

    set.seed(1)
    index = sample(ceiling(nrow(humus2) * 0.5))

    # initiate an isolation forest
    iso = isolationForest$new(sample_size = length(index))

    # fit for attrition data
    iso$fit(dataset = humus2)

    ## Building Isolation Forest ... done
    ## Computing depth of terminal nodes ... done

    # Obtain anomaly scores on train data
    iso$scores

    ##       id average_depth anomaly_score
    ##   1:   1      21.04908     0.2531709
    ##   2:   2      17.37555     0.3217595
    ##   3:   3      22.20352     0.2347977
    ##   4:   4      14.43256     0.3898910
    ##   5:   5      17.98929     0.3091266
    ##  ---                                
    ## 613: 613      10.20077     0.5139055
    ## 614: 614      14.64205     0.3845970
    ## 615: 615      10.22036     0.5132491
    ## 616: 616      16.33663     0.3443316
    ## 617: 617      20.60641     0.2605914

    # predict outliers on test or unseen data
    iso$predict(humus2[-index, ]) # scores for new data (50% sample)

    ##       id average_depth anomaly_score
    ##   1:   1      19.08789     0.2877392
    ##   2:   2      11.84070     0.4617464
    ##   3:   3      16.77533     0.3346131
    ##   4:   4      20.32612     0.2654021
    ##   5:   5      20.86004     0.2563136
    ##  ---                                
    ## 304: 304      10.20077     0.5139055
    ## 305: 305      14.64205     0.3845970
    ## 306: 306      10.22036     0.5132491
    ## 307: 307      16.33663     0.3443316
    ## 308: 308      20.60641     0.2605914

Anomaly detection
-----------------

The paper suggests the following: If the score is closer to 1 for a some
observations, they are likely outliers. If the score for all
observations hover around 0.5, there might not be outliers at all.

By observing the quantiles, we might arrive at the a threshold on the
anomaly scores and investigate the outlier suspects.

    # quantiles of anomaly scores (of train data)
    quantile(iso$scores$anomaly_score
             , probs = seq(0.5, 1, length.out = 11)
             )

    ##       50%       55%       60%       65%       70%       75%       80%       85% 
    ## 0.2885520 0.2968820 0.3047881 0.3157465 0.3248082 0.3349006 0.3503038 0.3676153 
    ##       90%       95%      100% 
    ## 0.4148054 0.4684593 0.7180871

The understanding of *why is an observation an anomaly* might require a
combination of domain understanding and techniques like lime (Local
Interpretable Model-agnostic Explanations), Rule based systems etc

Installation
------------

    install.packages("solitude")                  # CRAN version
    devtools::install_github("talegari/solitude") # dev version

------------------------------------------------------------------------

[1] Wright MN, Ziegler A (2017). “ranger: A Fast Implementation of
Random Forests for High Dimensional Data in C++ and R.” Journal of
Statistical Software, 77(1), 1–17. doi: 10.18637/jss.v077.i01
