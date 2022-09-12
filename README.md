# EfficientClosedGPD


The R package *EfficientClosedGPD* provides several new upper and lower bounds for the Beta function and the quotient of Beta functions proposed in <a style="text-decoration:none" href="https://suthakaranr.github.io/assets/2022FSEES.pdf" target="_blank" rel="noopener noreferrer">From and Ratnasingam (2022)</a>. Please see the examples below.

```{r}
rm(list = ls())
library(devtools) # Make sure that the devtools library is loaded
install_github("suthakaranr/EfficientClosedGPD")
library(EfficientClosedGPD)  # Load the package 
set.seed(650) 
x = rgpd2(40, 2, 2) # Generate sample
Method1(x) # Method 1
Method2(x) # Method 2
Method3(x) # Method 3
MethodQM(x) # Method QM
MethodPOS(x) # Method POS
MethodLCVM(x) # Method LCVM
```
