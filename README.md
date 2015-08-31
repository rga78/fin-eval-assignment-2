# Portfolio Optimization: (1) Large/Small/Growth/Value/Gold, (2) US/Japan/Asia-Pacific/Europe

[Financial Evaluation and Investment Strategy](https://www.coursera.org/learn/investments)   
[Assignment #2 (1.8)](https://www.coursera.org/learn/investments/peer/nWsgY/assignment-2)   
Rob Alderman   
Aug 2015   


---

## Part 1: Large/Small/Growth/Value/Gold
### Load the data

Data copied from EfficientFrontier-LargeSmall-ValueGrowth-Gold.xlsx.


```r
source("eff.frontier.R")

assets.cor <- matrix( c(
                        1.00, 0.65, 0.75, 0.93, -0.01,
                        0.65, 1.00, 0.75, 0.69, 0.07,
                        0.75, 0.75, 1.00, 0.65, 0.01,
                        0.93, 0.69, 0.65, 1.00, 0.00,
                        -0.01, 0.07, 0.01, 0.00, 1.00),
                     nrow=5)

assets.sd <- c(
                4.30,
                6.10,
                5.94,
                5.13,
                5.66) / 100

assets.covar <- eff.cor2cov( assets.cor, assets.sd )

assets.er <- matrix( c(
                        1.01,
                        1.35,
                        1.51,
                        0.95,
                        0.54 ) / 100,
                     nrow=1 )

colnames(assets.er) <- c("large", "small", "value", "growth", "gold")


risk.free.rate <- 0.4 / 100

eff.noshort <- eff.frontier(assets.covar, 
                            assets.er , 
                            short="no", 
                            risk.free.rate=risk.free.rate)

eff.short <- eff.frontier(assets.covar, 
                          assets.er , 
                          short="yes", 
                          risk.free.rate=risk.free.rate)
```



### Question 1.
CANNOT short.  
Find the portfolio that maximizes expected return  at same risk as 100% large stocks (sd=4.30%).
What is the expected return / weights ?
How much greater is expected return per month than 100% large stocks.


```r
eff <- eff.varEquals( eff.noshort, sd=4.30/100 )
round(eff, 5)
```

```
##        large   small   value growth    gold Std.Dev Exp.Return  sharpe
## 1814 0.22226 0.10538 0.47983      0 0.19254 0.04291    0.01195 0.18534
## 1815 0.22201 0.10542 0.48012      0 0.19244 0.04292    0.01195 0.18536
## 1816 0.22177 0.10547 0.48042      0 0.19235 0.04292    0.01196 0.18537
## 1817 0.22152 0.10552 0.48071      0 0.19225 0.04293    0.01196 0.18538
## 1818 0.22127 0.10556 0.48101      0 0.19216 0.04294    0.01196 0.18539
## 1819 0.22103 0.10561 0.48130      0 0.19206 0.04295    0.01196 0.18540
## 1820 0.22078 0.10566 0.48159      0 0.19197 0.04296    0.01196 0.18541
## 1821 0.22054 0.10570 0.48189      0 0.19187 0.04297    0.01197 0.18542
## 1822 0.22029 0.10575 0.48218      0 0.19178 0.04298    0.01197 0.18543
## 1823 0.22005 0.10580 0.48248      0 0.19168 0.04299    0.01197 0.18544
## 1824 0.21980 0.10584 0.48277      0 0.19159 0.04299    0.01197 0.18545
## 1825 0.21956 0.10589 0.48307      0 0.19149 0.04300    0.01198 0.18546
## 1826 0.21931 0.10593 0.48336      0 0.19139 0.04301    0.01198 0.18547
## 1827 0.21906 0.10598 0.48365      0 0.19130 0.04302    0.01198 0.18548
## 1828 0.21882 0.10603 0.48395      0 0.19120 0.04303    0.01198 0.18549
## 1829 0.21857 0.10607 0.48424      0 0.19111 0.04304    0.01198 0.18550
## 1830 0.21833 0.10612 0.48454      0 0.19101 0.04305    0.01199 0.18551
## 1831 0.21808 0.10617 0.48483      0 0.19092 0.04306    0.01199 0.18552
## 1832 0.21784 0.10621 0.48513      0 0.19082 0.04307    0.01199 0.18553
## 1833 0.21759 0.10626 0.48542      0 0.19073 0.04307    0.01199 0.18554
## 1834 0.21735 0.10631 0.48572      0 0.19063 0.04308    0.01199 0.18555
## 1835 0.21710 0.10635 0.48601      0 0.19054 0.04309    0.01200 0.18556
```


### Question 2.
CAN short.  
Find the portfolio that maximizes expected return  at same risk as 100% large stocks.
What is the expected return / weights ?
Which asset do you short?
Which asset has the biggest increase in weight over question 1?  Why?
How much greater is expected return over question 1?
Is shorting important in this example?


```r
eff <- eff.varEquals( eff.short, sd=4.30/100 )
round(eff, 5)
```

```
##        large   small   value   growth    gold Std.Dev Exp.Return  sharpe
## 1880 1.11339 0.30325 0.30334 -0.86019 0.14021 0.04290    0.01250 0.19824
## 1881 1.11333 0.30333 0.30360 -0.86036 0.14011 0.04291    0.01251 0.19825
## 1882 1.11326 0.30342 0.30385 -0.86054 0.14000 0.04292    0.01251 0.19825
## 1883 1.11320 0.30351 0.30411 -0.86071 0.13990 0.04293    0.01251 0.19826
## 1884 1.11314 0.30359 0.30436 -0.86088 0.13979 0.04294    0.01251 0.19827
## 1885 1.11307 0.30368 0.30462 -0.86105 0.13969 0.04295    0.01252 0.19827
## 1886 1.11301 0.30376 0.30487 -0.86123 0.13958 0.04296    0.01252 0.19828
## 1887 1.11295 0.30385 0.30513 -0.86140 0.13948 0.04297    0.01252 0.19829
## 1888 1.11288 0.30393 0.30538 -0.86157 0.13937 0.04298    0.01252 0.19829
## 1889 1.11282 0.30402 0.30564 -0.86174 0.13927 0.04299    0.01252 0.19830
## 1890 1.11276 0.30411 0.30589 -0.86192 0.13916 0.04300    0.01253 0.19831
## 1891 1.11269 0.30419 0.30615 -0.86209 0.13906 0.04301    0.01253 0.19831
## 1892 1.11263 0.30428 0.30640 -0.86226 0.13896 0.04302    0.01253 0.19832
## 1893 1.11256 0.30436 0.30666 -0.86243 0.13885 0.04303    0.01253 0.19833
## 1894 1.11250 0.30445 0.30691 -0.86261 0.13875 0.04303    0.01254 0.19833
## 1895 1.11244 0.30453 0.30717 -0.86278 0.13864 0.04304    0.01254 0.19834
## 1896 1.11237 0.30462 0.30742 -0.86295 0.13854 0.04305    0.01254 0.19834
## 1897 1.11231 0.30471 0.30768 -0.86312 0.13843 0.04306    0.01254 0.19835
## 1898 1.11225 0.30479 0.30793 -0.86330 0.13833 0.04307    0.01254 0.19836
## 1899 1.11218 0.30488 0.30819 -0.86347 0.13822 0.04308    0.01255 0.19836
## 1900 1.11212 0.30496 0.30844 -0.86364 0.13812 0.04309    0.01255 0.19837
```


### Question 3.
CANNOT short.  
Find the portfolio that maximizes sharpe ratio.
What are the expected return / standard deviation / weights ?
Is GOLD in the portfolio?  Why or why not?


```r
eff <- eff.maxSharpe( eff.noshort )
round(eff, 5)
```

```
##      large   small   value growth    gold Std.Dev Exp.Return  sharpe
## 2753     0 0.14731 0.75137      0 0.10131 0.05215    0.01388 0.18949
```

---

## Part 2:  US/Japan/Asia-Pacific/Europe

### Load the data

Data copied from EfficientFrontier-US-International.xlsx 


```r
assets.cor <- matrix( c( 
                         1.00, 0.41, 0.71, 0.79,
                         0.41, 1.00, 0.47, 0.50,
                         0.71, 0.47, 1.00, 0.75,
                         0.79, 0.50, 0.75, 1.00 ),
                     nrow=4 )
assets.sd <- c( 
                4.34,
                5.97,
                6.01,
                5.04) / 100

assets.covar <- eff.cor2cov( assets.cor, assets.sd )

        
assets.er <- matrix( c( 
                        0.89,
                        0.21,
                        0.96,
                        0.74) / 100,
                     nrow=1)

colnames(assets.er) <- c("us", "japan", "asia-pacific", "europe")

risk.free.rate <- 0.24 / 100

eff.noshort <- eff.frontier(assets.covar, 
                            assets.er , 
                            short="no", 
                            risk.free.rate=risk.free.rate)
```

### Question 4
CANNOT short.
Find the portfolio that maximizes expected return for the same risk as 100% US stocks.
What are the expected return and weights?


```r
eff <- eff.varEquals( eff.noshort, 4.34 / 100 , tolerance=0.00001)
round(eff, 5)
```

```
##           us   japan asia-pacific europe Std.Dev Exp.Return  sharpe
## 1139 0.93651 0.00217      0.06132      0 0.04339    0.00893 0.15044
## 1140 0.93661 0.00194      0.06145      0 0.04340    0.00893 0.15046
## 1141 0.93672 0.00171      0.06157      0 0.04340    0.00893 0.15049
## 1142 0.93682 0.00148      0.06169      0 0.04341    0.00893 0.15051
```

### Question 5
CANNOT short.
Find the portfolio that maximizes sharpe ratio.  
What are the expected returns, standard deviation, and weights?
Is Japan in the portfolio?  Why or why not?


```r
eff <- eff.maxSharpe( eff.noshort)
round(eff, 5)
```

```
##           us japan asia-pacific europe Std.Dev Exp.Return  sharpe
## 2892 0.86939     0      0.13061      0 0.04366    0.00899 0.15099
```


### Question 6
CANNOT short.
Assume Japan expected return is 0.96% (same as Asia Pacific).
Find the portfolio that maximizes sharpe ratio.  
What are the expected returns, standard deviation, and weights?



```r
assets.er <- matrix( c( 
                        0.89,
                        0.96,
                        0.96,
                        0.74) / 100,
                     nrow=1)

colnames(assets.er) <- c("us", "japan", "asia-pacific", "europe")

eff.noshort <- eff.frontier(assets.covar, 
                            assets.er , 
                            short="no", 
                            risk.free.rate=risk.free.rate)

eff <- eff.maxSharpe( eff.noshort )
round(eff, 5)
```

```
##           us  japan asia-pacific europe Std.Dev Exp.Return  sharpe
## 2521 0.69437 0.2989      0.00673      0 0.04113    0.00911 0.16323
```

