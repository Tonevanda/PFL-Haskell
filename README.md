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

## Data Structures

We have created several different data structures to support and facilitate the development of this project. These structures include the following:


### Code

The **Inst** data type represents the intructions supported by this small imperative language. It includes the following instructions:

- Push **Integer**
- Add
- Mult
- Sub
- Tru
- Fals
- Equ
- Le
- And
- Neg
- Fetch **String**
- Store **String**
- Noop
- Branch **Code** **Code**
- Loop **Code** **Code**

The type **Code** was defined to represent the list of instructions, in other words, the code that is going to be processed.

More information on the details of each instruction [here](#instructions)


### Stack

The **StackValue** data type represents the values that compose a stack such as **Integers** and both **TT** and **FF**, which represent truth values, **True** and **False**, respectively.

The type **Stack** represents an ordinary stack, composed of **StackValues**, in this case.<br>

We also created 3 functions pertaining to the **Stack**:

- `stackValueToString :: StackValue -> String`. This function utilizes pattern-matching to differentiate how to map a **StackValue** to a printable value on the terminal.
- `createEmptyStack :: Stack`. Very self-explanatory, creates an empty stack.
- `stack2Str :: Stack -> String`. This function iterates through the stack and prints it to the terminal. We used the **intercalate** function in the **Data.List** module to facilitate this.


### State

The **State** type represents the internal storage where variables are stored. To implement this type we used a **HashMap**, which receives a **Key**, which is a string, and a **Value**, which is a **StackValue**.

We created 4 functions pertaining to the **State**:

- `pairToString :: (String, StackValue) -> String`. This functions is similar to the `stackValueToString` function, where its goal is to map a pair from the **State** to a printable value on the terminal, using pattern-matching.
- `createEmptyState :: State`. Also very self-explanatory, creates an empty state using the `HashMap.empty` function
- `insertIntoState :: Key -> Value -> State -> State`. This function calls the `HashMap.insert` function to insert a Key Value pair into the state
- `state2Str :: State -> String`. Similar to the `stack2Str` function. Iterates through the state and prints it to the terminal.


### Program

To represent the **Program** to be compiled we created several different data types, namely **Aexp**, which represents **Arithmetic** expressions, **Bexp**, which represents **Boolean** expressions, and **Stm**, which represents the statements of this imperative language, **Assign**, **If**, **While** and **NoopStm**.

The **Aexp** data type contains the following:

- Num **Integer**, which represents a number
- Var **String**, which represents a variable
- AddExp **Aexp** **Aexp**, which represents the addition of two other Arithmetic expressions
- SubExp **Aexp** **Aexp**, which represents the subtraction of two other Arithmetic expressions
- MultExp **Aexp** **Aexp**, which represents the multiplication of two other Arithmetic expressions

The **Bexp** data type contains the following:

- Tr, which represents TT, or True
- Fls, which represents FF, or False
- Not **Bexp**, which represents the negation of another Boolean expression
- AndExp **Bexp** **Bexp**, which represents the logical **AND** operation between two Boolean expressions
- LeExp **Aexp** **Aexp**, which represents the ***less than or equal*** comparison between two Arithmetic expressions
- EquExp **Bexp** **Bexp**, which represents the ***equality*** comparison between two Boolean expressions, e.g True = True
- DoubleEqu **Aexp** **Aexp**, which represents the ***equality*** comparison between two Arithmetic expressions, e.g 1 == 1

The **Stm** data type contains the following:

- Assign **String** **Aexp**, which represents the variable assignment operation, e.g y:= 1+2
- If **Bexp** **Program** **Program**, which represents an If Then Else statement, e.g If (x<=1) Then x:=0 Else x:=2 
- While **Bexp** **Program**, which represents a While Do statement, e.g While (True) Do x:= x + 1

The **Program** type is a list of statements.

## Interpreter

To implement the interpreter for this imperative language, we applied pattern-matching to the run function, checking the head of the **Code** list for every possible instruction and executing it accordingly.

We also used the same pattern-matching strategy to check if the stack had invalid arguments for a certain instruction, and if so we throw a **Run-time error**.

For example, here is the **Le** run function:

```haskell
run (Le:remainingCode, IntValue i1:IntValue i2:stack, state) = run (remainingCode, value:stack, state)
    where value = if i1 <= i2 then TT else FF
run (Le:remainingCode, _:_:stack, state) = error "Run-time error"
```

Because **Le** uses the two topmost values of the stack, which have to be **IntValue**, the main function that is going to execute the **Le** instruction is the first pattern-matching, which contains **Le** at the top of the **Code** list, meaning it is the next instruction to be processed, and the stack contains **IntValue** i1 and **IntValue** i2 at the top. In this case, we recursively call **run** again with the remaning code and with a new stack, with its topmost element depending on wether or not i1 is less than or equal to i2.

If this pattern for the **Le** run function does not match, it means the two topmost values of the stack are not both **IntValues**, in that case we call the **error** function with the "**Run-time error**" string.

### Instructions 

The following is every instruction along with a more in-depth explanation of how it works:

- The `createEmptyStack` function returns an empty machine’s stack.
- The `createEmptyState` function returns an empty machine’s state, using the **HashMap.empty** function.
- The `stack2Str` function converts a stack given as input to a string using intercalate and map functions.
- The `state2Str` function converts a state given as input to a string using intercalate and map functions along with the sort function, to sort the state in alphabetical order.
- The `add` function adds the top two integer values of the stack and pushes the result onto the top of the stack.
- The `mult` function multiplies the top two integer values of the stack and pushes the result onto the top of the stack.
- The `sub` function subtracts the topmost element of the stack with the second topmost element of the stack, and pushes the result onto the top of the stack.
- The `eq` function compares the equality of the top two values of the stack, both integers and boolean, and pushes a boolean with the comparison result onto the top of the stack. If they are equal, TT will be pushed to the stack, otherwise FF will be pushed.
- The `le` function determines whether the topmost stack element is less or equal to the second topmost element, and pushes a boolean with the comparison result onto the top of the stack. If it is, TT will be pushed to the stack, other FF will be pushed.
- The `and` function which pops the two topmost elements on the stack and pushes onto the stack the result of the logical ***AND*** operation. This only works for booleans.
- The `neg` function pops the topmost element on the stack and pushes onto the stack the negation of that element. This only works for booleans.
- The `push-n` function pushes a constant value n onto the stack; true and false push the constants TT and FF, respectively, onto the stack.
- The `fetch-x` function pushes the value bound to **x** onto the stack, with **x** being a variable in the **Storage**, a.k.a **State**
- The `store-x` function pops the topmost element of the stack and updates the **state** so that the popped value is bound to **x**
- The `branch(c1,c2)` function branches the code flow depending on the topmost value of the stack. If the top of the stack is the value **TT** (that is, some boolean expression has been evaluated to true), then the stack is popped and **c1** is to be executed next. Otherwise, if the top element of the stack is **FF**, then it will be popped and **c2** will be executed next. If the top of the stack is not a truth value, the machine will halt as there is no next configuration (since the meaning of branch(···,···) is not defined in that case)
- The `loop(c1,c2)` function can be interpreted as a **while** loop. The semantics of this instruction are defined by rewriting it to a combination of other constructs, including the branch instruction and itself. For example, `loop(c1, c2)` may be transformed into `c1 ++ [branch([c2, loop(c1, c2)], [noop])]`.
- The `noop` function returns the **Stack** and **State**.

## Extra Information

### Second Part

- [x] Define three datas in Haskell to represent expressions and statements of this imperative language:
  - [x] *Aexp* for arithmetic expressions
  - [x] *Bexp* for boolean expressions
  - [x] *Stm* for statements
- [ ] Define a compiler from a program in this small imperative language into a list of machine instructions. 
  - [ ] The main compiler function: **compile**
  - [x] A function that compiles arithmetic expressions: **compA**
  - [x] A function that compiles boolean expressions: **compB**
- [x] Define an auxiliary function called **lexer** that splits the string into a list of words. Example: **lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]**
- [ ] Define a parser which transforms an imperative program represented as a string into its corresponding representation in the ***Stm*** data (a list of statements *Stm*). The string representing the program has the following synctactic constraints:
  - [ ] All statements end with a semicolon (;)
  - [ ] The string must contain spaces between each element, but does not include newlines nor tabs
  - [ ] Variables begin with a lowercase letter (assume that no variable name can contain a reserved keyword as a substring. For instance, anotherVar is an invalid variable name as it contains the keyword not)
  - [ ] Operator precedence in arithmetic expressions is the usual: multiplications are performed before additions and subtractions. Additions and subtractions have the same level of precedence and are executed from left to right (i.e. they are left-associative). Multiplications are also left-associative
  - [ ] Parentheses may be used to add priority to an operation. For instance, 1+2*3 = 7 but (1+2)*3 = 9
  - [ ] In boolean expressions, two instances of the same operator are also computed from left to right. The order of precedence for the operations is (with the first one being executed first): integer inequality (≤), integer equality (==), logical negation (not), boolean equality (=), logical conjunction (and). For instance, not True and 2 ≤ 5 = 3 == 4 is equivalent to (not True) and ((2 ≤ 5) = (3 == 4))
