# elm-LogicCalculator  
visit [here](https://iorin-elmo.github.io/elm-LogicCalculator/)  
## Version  
elm 0.19.1  
## Command  
``` sh
elm make Main.elm --output=elm.js
```
## Usage  
### Symbols  
tautology(⊤)  
⊤  == true  

inconsistency(⊥)  
⊥  == false  

negation(¬)  
¬(¬a) == a  

conjunction(∧)  
⊤∧⊤ == true  

disjunction(∨)  
⊤∨⊥ == true  

implication(⇒)  
a⇒b == ¬a∨b  

equivalence(⇔)  
¬(a∨b)⇔¬a∧¬b == true  

### Buttons  
Parse :  
parse input string  
Calculate :  
calculate variable in parsed string  
(checkbox is used for setting Boolean  
, checked = true)  
Solve :  
solve satisfiability problem  


