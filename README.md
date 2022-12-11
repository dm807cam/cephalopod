## *cephalopod*

*cephalopod* provides a helpful framework to access the Kraken API to monitor, 
place or cancel orders. Currently, the package contains API calls that I 
found helpful when trading on Kraken. **Feel free to submit an 
issue on Github to discuss new features.**

*cephalopod* is still under `development` and is not yet available through CRAN. If you would like to test *cephalopod*, you can install the package directly from 
this repo with `devtools` or `remotes`: 
`devtools::install_github("https://github.com/dm807cam/cephalopod/dev")`.

*cephalopod* requires a working Kraken API private and public key pair that you will need to register in your Kraken account. A vignette illustrating this process will be released soon. 

```
public_key <- "XXXX"
private_key <- "XXXX" 
```

DISCLAIMER: This package is still under development, open-source, and I am not liable for any issues arising from using it. Please report any issues by opening a ticket.
