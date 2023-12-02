# Assembly compiler in Haskell


## Developers

This project was developed by the group T1_G08, composed by:

- João Miguel da Silva Lourenço (up202108863) 50%
- Tomás Filipe Fernandes Xavier (up202108759) 50%

## Instalation and Execution

To run this project, you need to:

  - Have the haskell interpreter **GHCi**. If you don't have it, you can install it [**here**](https://www.haskell.org/ghcup/)
  - Download the **src** folder
  - Open a terminal, navigate to the **src** folder and run `ghci`
  - Load the `main.hs` file by writing `:l main.hs` in the **ghci**

With this, you are all set to run the code. Enjoy!

## TODO List

### First Part

- [x] Define a new type to represent the machine’s stack. The type must be named **Stack**
- [x] Define a new type to represent the machine’s state. The type must be named **State**
- [x] Implement the **createEmptyStack** function which returns an empty machine’s stack
- [x] Implement the **createEmptyState** function which returns an empty machine’s state
- [x] Implement the **stack2Str** function which converts a stack given as input to a string
- [x] Implement the **state2Str** function which converts a machine state given as input to a string
- [x] Implement the **add** function which adds the top two integer values of the stack and pushes the result onto the top of the stack
- [x] Implement the **mult** function which multiplies the top two integer values of the stack and pushes the result onto the top of the stack
- [x] Implement the **sub** function which subtracts the topmost element of the stack with the second topmost element of the stack, and pushes the result onto the top of the stack
- [x] Implement the **eq** function which compares the equality of the top two values of the stack, both integers and boolean, and pushes a boolean with the comparison result onto the top of the stack
- [x] Implement the **le** function which determines whether the topmost stack element is less or equal to the second topmost element, and pushes a boolean with the comparison result onto the top of the stack
- [ ] Implement the **push-n** function which pushes a constant value n onto the stack; true and false push the constants tt and ff, respectively, onto the stack
- [ ] Implement the **fetch-x** function which pushes the value bound to **x** onto the stack, with **x** being a variable in the **Storage**, a.k.a **State**
- [ ] Implement the **store-x** function which pops the topmost element of the stack and updates the storage so that the popped value is bound to **x**
- [ ] Implement the **branch(c1,c2)** function which branches the code flow depending on the topmost value of the stack. If the top of the stack is the value **tt** (that is some boolean expression has been evaluated to true), then the stack is popped and **c1** is to be executed next. Otherwise, if the top element of the stack is **ff**, then it will be popped and **c2** will be executed next. If the top of the stack is not a truth value, the machine will halt as there is no next configuration (since the meaning of branch(···,···) is not defined in that case)
- [ ] Implement the **loop(c1,c2)** function which can be interpreted as a **while** loop. The semantics of this instruction is defined by rewriting it to a combination of other constructs, including the branch instruction and itself. For example, **loop(c1, c2)** may be transformed into **c1 ++ [branch([c2, loop(c1, c2)], [noop])]**.
- [ ] Implement the **noop** function which returns the **Stack** and **Storage**
- [ ] Write an interpreter for programs in the same machine, which given a list of instructions (type defined as **Code**, i.e. type Code = [Inst]), a stack (type defined as **Stack**) and that is initially empty, and a storage (type defined as **State**), runs the list of instructions returning as output an empty code list, a stack and the output values in the storage


### Second Part

- [ ] Define three datas in Haskell to represent expressions and statements of this imperative language:
  - [ ] *Aexp* for arithmetic expressions
  - [ ] *Bexp* for boolean expressions
  - [ ] *Stm* for statements
- [ ] Define a compiler from a program in this small imperative language into a list of machine instructions. 
  - [ ] The main compiler function: **compile**
  - [ ] A function that compiles arithmetic expressions: **compA**
  - [ ] A function that compiles boolean expressions: **compB**
- [ ] Define an auxiliary function called **lexer** that splits the string into a list of words. Example: **lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]**
- [ ] Define a parser which transforms an imperative program represented as a string into its corresponding representation in the ***Stm*** data (a list of statements *Stm*). The string representing the program has the following synctactic constraints:
  - [ ] All statements end with a semicolon (;)
  - [ ] The string must contain spaces between each element, but does not include newlines nor tabs
  - [ ] Variables begin with a lowercase letter (assume that no variable name can contain a reserved keyword as a substring. For instance, anotherVar is an invalid variable name as it contains the keyword not)
  - [ ] Operator precedence in arithmetic expressions is the usual: multiplications are performed before additions and subtractions. Additions and subtractions have the same level of precedence and are executed from left to right (i.e. they are left-associative). Multiplications are also left-associative
  - [ ] Parentheses may be used to add priority to an operation. For instance, 1+2*3 = 7 but (1+2)*3 = 9
  - [ ] In boolean expressions, two instances of the same operator are also computed from left to right. The order of precedence for the operations is (with the first one being executed first): integer inequality (≤), integer equality (==), logical negation (not), boolean equality (=), logical conjunction (and). For instance, not True and 2 ≤ 5 = 3 == 4 is equivalent to (not True) and ((2 ≤ 5) = (3 == 4))
