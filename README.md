## cephalopod

cephalopod provides useful framework to access the Kraken API to monitor, 
place or cancel orders. Currently, the package contains API-calls that I 
personally found useful when trading on Kraken. Feel free to submit an 
issue on Github to discuss new features.

cephalopod is still under `development` and not available through CRAN. If 
you want to test it already, you can install the package directly from 
this repo with devtools: 
`devtools::install_github("https://github.com/dm807cam/cephalopod/dev")`.

cephalopod requires a working private and public key pair that you will 
need to obtain from your Kraken account. A vignette illustrating this 
process will be released soon. 

```
public_key <- "XXXX"
private_key <- "XXXX" 
```

DISCLAIMER: This package is still under development and I am not liable for any issues that may arise from using it. 
