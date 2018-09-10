kidmath

Guofang Liu

This is a R package used to 
> plotting a 50 math formulas for kid exercise and

> calculating solutions in 24-scores game (24点游戏). 

```{R,results="hide",warning=FALSE,message = FALSE}
  #install.packages("kidmath")
  devtools::install_github("liuguofang/kidmath")
  library(kidmath)
```

```{R,warning=FALSE,message = FALSE}
  #install.packages("kidmath")
  devtools::install_github("liuguofang/kidmath")
  library(kidmath)
  score24(x = c(1,2,9,11))
  [1] "(11*(2+1))-9" "((2+1)*11)-9" "(11*(1+2))-9" "((1+2)*11)-9"

```

