# elm-Calculator
calculator with parser on Elm  
visit [here](https://iorin-elmo.github.io/elm-Calculator/)   
## Version  
elm 0.19.1  
## Command
``` sh
elm make src/Main.elm --output=elm.js
```
## Grammar  
### Boolean Operation
greater than  
```2 > 4 gets false```  
less than  
```2 < 4 gets true ```  
equal  
```2 = 4 gets false```  
### Integer Operation  
addition  
```2 + 4 gets 6```  
substraction  
```2 - 4 gets -2```  
multiplication  
```2 * 4 gets 8```  
power  
```2 ^ 4 gets 16```  
division  
```2 / 4 gets 0```  
mod  
```2 % 4 gets 2```  
### Lambda Expression  
abstruction  
usage :  
\ <***type***> <***var***>  : <***term***>  
```  
\int n : n*2  
\int -> int x : x  
```  
apply  
usage : <***term***> <***term***>  
```  
(\int n : n*2) 3                       gets 6  
(\bool->bool m : m)(\bool n : n) true  gets true  
```
### Other Grammar  
let  
usage :  
let <***var***> = <***term***> in <***term***>
```
let not=\bool n : if n then false else true in not true   gets false
let x=1 in let x=x+1 in x                                 gets 2
```
if  
usage :  
if <***condition***> then <***term***> else <***term***>  
```  
if 2 < 4 then 2 else 4          gets 2  
if false then false else true   gets true  
```


