# elm-Calculator  
calculator made by elm  
## Command  
``` sh  
elm make src/Main.elm --output=elm.js  
```
## Grammar  
#### Definition
A/a := Capital and Small letter allowed
a/N := alphabet(small letter) and number allowed

--<var> := A/a (++ a/N ++ a/N ++ ... )

--<term>::= <var>
--        | \ <var> : <type> -> <term>
--        | <term> <term>
--        | true
--        | false
--        | if <term> then <term> else <term>
--        | <int>
--        | <term> <binaryOp> <term>
--        | <term> <compOp> <term>
--        | let <var> = <term> in <term>

--<type>::= <bool>
--        | <int>
--        | <type> -> <type>

--<binaryOp>::= + | - | * | ^
--<compOp>  ::= > | < | =


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
division  
```2 / 4 gets 0```  
mod  
```2 % 4 gets 2```  
### Lambda Expression  
abstruction  
usage :  
\ <***var***> : <***type***> -> <***term***>  
```  
\n:int -> n*2  
\m:bool -> m  
```  
apply  
usage : <***term***> <***term***>  
```  
(\n:int -> n*2) 3                       gets 6  
(\m:bool->bool -> m)(\n:bool -> n)true  gets true  
```
### Other Grammar  
let  
usage :  
let <***var***> = <***term***> in <***term***>
```
let not=\n:bool -> if n then false else true in not true   gets false
let x=1 in let x=x+1 in x                                  gets 2
```
if  
usage :  
if <***condition***> then <***term***> else <***term***>  
```  
if 2 < 4 then 2 else 4          gets 2  
if false then false else true   gets true  
```


