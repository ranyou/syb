# Scrap Your Boilerplate

This repo implements the generic programming algorithms presented in: 

[Scrap your boilerplate: a practical design for generic programming](https://dl.acm.org/doi/10.1145/640136.604179)

To test the implementation, first load the Gmap.hs file into the REPL by

```
$ ghci Gmap.hs
```

Then, in the REPL, enter the following commands to test for different 
functionalities: 

* Perform generic transformations: 
```
Main> increase’ 0.1 genCom
```
* Perform generic queries: 
```
Main> salaryBill genCom
```
* Perform monadic transformations: 
```
Main> lookupSalaries genCom
```