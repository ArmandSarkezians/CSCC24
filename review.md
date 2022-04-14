# CSCC24 Midterm Review

Topics we will cover include:

- [CSCC24 Midterm Review](#cscc24-midterm-review)
  - [Programming Languages](#programming-languages)
  - [Parse Trees](#parse-trees)
    - [Translation process](#translation-process)
  - [Regular Expressions](#regular-expressions)
    - [Examples](#examples)
  - [Context Free Grammars](#context-free-grammars)
    - [Example](#example)
  - [**Racket**](#racket)
    - [Simple programming features](#simple-programming-features)
    - [Higher Order Procedures (HOP)](#higher-order-procedures-hop)
    - [Important Functions](#important-functions)
    - [Closure](#closure)
    - [Recursion](#recursion)
    - [Evaluations and Continuations](#evaluations-and-continuations)
    - [Continuation-Passing Style (CPS)](#continuation-passing-style-cps)
  - [**Java**](#java)
    - [Java Streams](#java-streams)
    - [Java Lambda](#java-lambda)
    - [Java Method Referencing](#java-method-referencing)
  - [**Haskell**](#haskell)
    - [Currying](#currying)
    - [Types](#types)
    - [Type Classes](#type-classes)
    - [List Comprehension](#list-comprehension)
    - [Lazy Evaluation](#lazy-evaluation)
    - [Infinite Numbers](#infinite-numbers)
  - [Prolog](#prolog)

## Programming Languages

There are high level languages (Java, C) in which very little registry editing, low level work ever needs to be done. One step down are assembly languages (MIPS, x86) in which registry editing is common work. These give the developer more control, but also provide less support. At the bottom level are machine languages. These are almost never used by developers, compilers create this code for us.

There are two (and a half) different types of programming languages. A language like C is compiled, meaning that the compiler reads the entire source code, translates it into target code, and then runs it. A language like Python is interpreted, meaning that each line goes straight to the compiler one by one. This is why languages like Python (and Racket) can be used in IDEs such as WingIDE or DrRacket, where you can run a single line of code to test something out. Java is a hybrid of these two, the compiler spits out intermediate code, which is interpreted. This intermediate code is called bytecode in Java.

## Parse Trees

Converting a line of code into machine language is easy enough: You must put the code into a Parse Tree. Act as if the line of code is a string. Delimit this string by obvious entities (brackets, spaces, quotes, curly braces, etc). The delimiters change based on the language you are using (In Python, spaces are necessary, but in Java, they can be thrown away). A programming language must have its language defined formally, compilers use this language to determine the parse tree from a line of code. This is how they translate code into machine language. Parsing is defined to be the process of producing a parse tree.

### Translation process

This is the process in which a language translates your higher level code into machine code.

- Lexical Analysis (converting source code into tokens by delimiting)
  - This is just simple delimiting
- Syntactic Analysis (using CFGs and parsing algorithms to develop a Parse Tree)
  - We have learned and practiced this
- Semantic Analysis (annotating a parse tree with semantic actions)
  - We have not learned this, semantic actions are outside the scope of this course
- Code Generation (producing the final machine code)
  - This is also outside the scope of this class

## Regular Expressions

Regular Expressions are used to represent languages. They are very useful for string parsing. (0 + 1)\* represents any string of finite length that consists of 0s or 1s in any order: binary numbers. The Keleene star (\*) denotes 0 or more representations. OR (+) represents choice, or 1 or more relations. Parenthesis are used for grouping, and the empty string epsilon is useful. Extended Regex includes "?" or "{}", which both indicate 0 or 1 occurrences of a string.

### Examples

- All alphanumeric strings beginning with an uppercase letter:
  - (A|B|C|D|...|Z)(A|B|C|...|Z|a|b|c|...|0|1|2|...|9)*
- All binary numbers with exactly three 1s
  -"0\*10\*10\*10\*"

## Context Free Grammars

A Context Free Grammar (CFG) is a combination of terminals, non-terminals, productions, and a starting symbol. It is formally defined as such: A CFG G is a tuple {$\Sigma$, V, S, P} where $\Sigma$ is the set of terminals, V is the set of non-terminals ($\Sigma \:\cup$ V is $\empty$), S is the start symbol (S $\in$ V), and P is the set of production rules, each in the form x -> w where x $\in$ V and w $\in$ $\Sigma \:\cup$ V.

### Example

- A CFG that denotes the decimal system
  - $\Sigma$ = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, .}
  - V = {\<real\>, \<part\>, \<digit\>}
  - S = \<real\>
  - P =
    - \<real\> -> \<part\> . \<part\>
    - \<part\> -> \<digit\> | \<digit\> \<part\> (this is the recursive case)
    - \<digit\> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

- A CFG that denotes the binary system
  - $\Sigma$ = {0, 1}
  - V = {<number\>}
  - S = <number\>
  - P =
    - <num\> = <number\> | <number\> <num\>

You can take a Parse Tree of this CFG. The root of this Parse tree will be the starting symbol of the CFG. Each leaf is a terminal, and every internal node is a non-terminal. There are a lot of important concepts when it comes to CFGs. These are not needed for the midterm, however, so I will not be going into them.

## **Racket**

Racket is a functional programming language. It is a modern dialect of Lisp and a descendant of Scheme. It is used for scripting, CS education and research.

Racket Data types include:

- Integers
- Reals
- Rationals
- Complex (racket can store fractions)
- Symbols
  - The sign for a symbol is '. You can include hyphens, numbers, and question marks in symbol variables
- Boolean
- Pair
  - Denoted '(1 . 2), the first element is "car" (contents of address register) and the second is "cdr" (elements of decrement register)
  - Constructed using "cons" -> (cons 1 2) constructs a pair '(1 . 2)
- List
  - '(1 2 3 4 5 6), every list is a pair except '()
  - You can use cons to construct lists, for a list car is the first element, cdr is the rest of the list (returned as a list)
  - We can also use 'first' and 'rest' for lists, but not for pairs
- Strings
- Vectors
  - Denoted #(1 2 3)
- Hast Tables
  - Denoted #hash(("apple" . red) ("banana" . yellow))
  - Hash tables take in pairs as arguments

Racket uses REPL (Read-Eval-Print-Loop) features in order to develop code. Functional programming allows for parallelization without the need for synchronization. This is sometimes argued to be the future of computer programming, as machines running things in parallel can achieve significantly quicker results than a machine based on synchronicity.

### Simple programming features

- Functions are denoted "(define (func-name var1 var2 ... varn))"
- If statements are denoted "(if (expr1) return-value1 return-value2)"
- Condition statements are denoted "(cond [(expr1) return-value1] [else return-value2])"
- Addition is defined "(+ var1 var2)", subtraction is "(- var1 var2)" and equality is "(equal? var1 var2)"
- The general syntax for a function is "(func-name var1 var2 ... varn)"

### Higher Order Procedures (HOP)

A Higher Order Procedure is a procedure that takes in another procedure as input, or returns another procedure as a result. We have three main HOPs that we explored in class: Map, Foldr, and Apply. An example of a HOP is shown below:

```Scheme
#lang racket
(define (true? elem)
  (cond [(equal? elem #t) #t]
        [else #f]))

(define all-ok? 
    (lambda (ok? lst)
        (cond [(empty? lst) #t]
              [(ok? (first lst))
              (all-ok? true? (rest lst))]
              [else false])))

(all-ok? true? '(#t #t #t))

Output: #t
```

The above procedure may be deceiving: This function returns a lambda procedure. It does not have any arguments, but instead takes in true? and the list for the lambda function. This code checks to see if a list contains all true elements in a functional way.

```Scheme
(define (my-map f xs)
    (if (empty? xs)
        empty
        (cons (f (first xs)) (my-map f (rest xs)))))  

(my-map (lambda (x) (+ x 10)) '(1 2 3))

Output: '(11 12 13)
```

The above procedure is a recursive map implementation. It takes in a function and a list of elements, and applies that function to each element in the list. There is a built-in map function, which takes in a procedure and n arguments (where the procedure requires n amount of arguments). This is shown below

```Scheme
(define (inc x)
  (+ x 10))

(map inc '(1 2 3))
(map max '(1 2 3) '(0 5 42))

Output: '(11 12 13)
        '(1 5 42)
```

The above code represents the built-in function map, which applies inc to each element in the list '(1 2 3). There is another HOP called "fold" which is important. This procedure can be left associative or right associative (foldl or foldr), but the right associative procedure is used more. It has the syntax (foldr op id lst), where it applies "op", a binary procedure (only 2 arguments), to each element right associatively in lst, with id being the identity element.

```Scheme
(define (my-fold f id xs)
  (if (empty? xs)
      id
      (f (first xs) (my-fold f id (rest xs)))))

(my-fold max 35 '(9 20 5 6 8 100 26)) ; Calling using my implemented foldr
(foldr max 35 (cons 9 (cons 20 (cons 5 (cons 6 (cons 8 (cons 100 (cons 26 '())))))))) ; Built-in foldr
(max 9 (max 20 (max 5 (max 6 (max 8 (max 100 (max 26 35))))))) ; Does the same thing as the above calls

Output: 100 (all are the equivalent)
```

foldr is trivially the bottom line. All it does is deconstruct the list, and apply each element to the function recursively. There is another important HOP called Apply. Apply takes in a function and a list, and applies the function to the list (kinda like fold but without an identity element)

```Scheme
(apply + '(1 2 3))

Output: 6
```

### Important Functions

One important racket function is Append. This function will append two lists together. If called with nothing, it returns an empty list. If called with a single list, it returns this list. If called with two lists, it appends the second onto the first.

```Scheme
(append '(1 3 5) '(2 4 6))

Output: '(1 3 5 2 4 6)
```

The last function we will look at is let. Let places information in memory, then executes a body.

```Scheme
(let ([x 5]) (+ x 1))

Output: 6
```

The above code places 5 into a memory location labelled x, then runs the body "(+ x 1)", which outputs 5 + 1 = 6.

### Closure

Closure is an important concept in any programming language. It is defined to be the area in which a variable is known.

```Scheme
(define x 50)
(define (plus-x y)
    (+ x y))

(plus-x 10) 

(let([x 100])
    (plus-x 10)) 
(set! X 200)
(plus-x 10) 

Output: 60, 60, 210
```

The above code shows closure. x is defined to be a function that returns 50. The first answer is obvious. The second is a bit more complicated: once you leave the let statement, the x value is no longer defined, and, therefore when calling plus-x racket goes to the function x and not the value. set! (non-functional) sets a global variable, therefore, the value of x as a global variable is changed, and racket does not need to go to the function to retrieve x. If the (define x 50) did not exist, there would be an unbound exception, as the plus-x function would not know what x is. The scope of these let variables is only in the body of the result.

letrec is a recursive version of let. I am not sure what it does.

Use let\* when you want to use previously defined variables in another variable initialization:

```Scheme
(define (test
  (lambda (x) 
    (let* ([sqr (* x 2)]
          [cube (* sqr x)])
    (+ cube 0))
  )
))
```

This won't work with regular let, sqr won't be defined when cube is initialized, so we need to use let\*.

### Recursion

There are many different types of recursion. Some are listed below:

- Linear Recursion
  - This is where there is at most one recursive call made in any execution of a function (an if-else where both bodies are recursive calls is still linear, as only one ever gets called)
- Flat Recursion
  - This is where recursion is applied over the 'top' items in a list (not nested)
- Deep/Tree Recursion
  - Opposite of flat, where recursion is applied over all items
- Structural Recursion
  - Where you have a recursive data structure (tree/list) and you are only recursing over it
- Mutual Recursion
  - Where functions call eachother rather than themselves
- Tail Recursion
  - Where the recursive call is the last function application in the function body
  - This is the one we looked at the most

A recursive way to implement deep-map is shown below:

```Scheme
(define (deep-map f xs)
  (cond [(empty? xs) '()]
        [(list? (first xs)) (cons (deep-map f (first xs)) (deep-map f (rest xs)))]
        [else (cons (f (first xs)) (deep-map f (rest xs)))]))
```

Tail recursion minimizes the stack, and reduces the opportunity for a stack overflow. Python does not implement this: they want their stack to be intact (tail recursion doesn't support backtraces).

A tail recursive way to check the length of a list is shown below:

```Scheme
(define (length-tail xs)  ; return length[xs]
  (local [(define (my-len-t xs acc)
    (cond 
      ((null? xs) acc)
      (else (my-len-t (cdr xs) (+ acc 1)))))]
    (my-len-t xs 0)))
```

A tail recursive way to map items is shown below:

```Scheme
(define (map1-tail f lst)
  (local [(define (map1-tail-helper lst lst-acc)
            (if (empty? lst)
                lst-acc
                (map1-tail-helper (rest lst) (append lst-acc (list (f (first lst)))))))]
  (map1-tail-helper lst '())))
```

### Evaluations and Continuations

Racket expressions can be partitioned into two parts, a redex (reducable expression) and a continuation. For example, if we have (- 4 (+ 1 1)) we have (+ 1 1) as the redex, and (-4 []) as the continuation.

### Continuation-Passing Style (CPS)

This is a style of programming in which continuations are passed between functions.

A 'normal' definition of a factorial:

```Scheme
(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))
```

A CPS definition:

```Scheme
(define fact-cps                  ; return n!
    (lambda (n)
        (local [(define fcps      ; return k(n!)
            (lambda (n k)         ; k is a continuation
                (if (= n 0)
                    (k 1)
                    (fcps (- n 1)
                        (lambda (v) (k (* n v)))))))]
            (fcps n (lambda (x) x)))))
```

## **Java**

While Java is a OOP programming language, it can be used in a functional sense.

### Java Streams

A Stream in Java is a sequence of elements from a source. For example, the small portions of a video file is a stream, and the whole file is the collection. A collection is an in-memory data structure which holds all the values, while a stream is conceptually a pipeline, in which elements are computed on demand. Streams are designed for lambdas, they are not data structures and therefore do not support indexed accesses, however, they support lazy access and are parallelizable. The ways one can create a stream are shown below:

```Java
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;

public class review {
    public static void main(String[] args) {
        // Creating a stream
        Stream<Integer> stream = Stream.of(1, 2, 3, 4, 5, 6, 7, 8, 9);
        stream.forEach(p -> System.out.println(p));

        // Creating a stream of integers
        Stream<Integer> stream2 = Stream.of(new Integer[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 });
        stream2.forEach(p -> System.out.println(p));

        // Creating a stream from a list
        List<Integer> list = new ArrayList<Integer>();
        for (int i = 1; i < 10; i++) {
            list.add(i);
        }
        Stream<Integer> stream3 = list.stream();
        stream3.forEach(p -> System.out.println(p));

        // Generating a stream
        Stream<Integer> randomNumbers = Stream.generate(() -> (new Random()).nextInt(100));
        randomNumbers.limit(20).forEach(System.out::println);

    }
}
```

And some implementations of streams are shown below:

```Java
    return expressions.stream().reduce(false, (eval, operator) -> eval 
        && operator.evaluate(context), Boolean::logicalAnd);

    return expressions.stream().map(value -> value.evaluate(context)).reduce(identity,
                (eval, value) -> func.apply(eval, value));

    return expressions.stream().map(value -> value.evaluate(context)).reduce(true,
                (eval, value) -> eval || value, Boolean::logicalOr);
```

### Java Lambda

Lambda expressions in Java are defined as such:

```Java
@FunctionalInterface
interface Operator<T> {
  T process(T a, T b);
}
Operator<Integer> addOperation = (a, b) ->  a + b;
System.out.println(addOperation.process(3, 3));     //Prints 6

// Or another way
List<String> pointList = new ArrayList();
pointList.add("1");
pointList.add("2");
pointList.forEach( p ->  { System.out.println(p); } );
```

### Java Method Referencing

In Java we refer to methods like such: "classname::methodname". For example, obtaining the 'max' function from the 'Math' class would look like this: "Math::max", which is equivalent to "Math.max(x, y)".

- Referring to the instance method from instance: "System.out::println" which is equivalent to "System.out.println(x)"
- Referring to the instance method from a class type: "String::length" which is equivalent to "str.length"
- Referring to the constructor: "ArrayList::new" which is equivalent to "new ArrayList()"

## **Haskell**

Racket is type safe, meaning that it will never access memory if there is a type violation. A language like C is not, it will allow memory accesses no matter the type (source of security vulnerabilities)

We want to catch all errors at compile time, but this is very hard if not impossible. Haskell tries to catch as many errors as possible; called type systems

In haskell, Int -> Bool is a type of function that takes in integers as input and returns booleans as output.

Benefits of a type system include:

- Easier to debug
- Static analysis
- Efficiency
- Correctness
- Documentation

Static type checking (like in haskell) is done at compile-time, it allows for faster execution and for the compiler to optimize (some say that is also safer). Dynamic type checking is done at run-time, with slower execution (need to carry type information around to check for type contracts).

Explicit Static Typing (like in java) is when types are declared: int x. Type inference is when types are infered from the code during their assignment (like in python): x = 7.

Haskell Types

- (): unit type
- Bool
- Int
- Float
- Char
- []: list, all elements must have the same type
- (): tuple
- <>: functions

### Currying

Currying means taking a function whose argument is a tuple, and transforming it into a function which takes on a single variable, and returns a function, so on, until the final function returns what the original function returned.

```Haskell
-- Non-curried version
sum :: (Int, Int) -> Int
sum (x, y) = x + y

-- Curried version
sum' :: Int -> Int -> Int
sum' x :: (\y -> x + y)
```

In the second version of the 'sum' function, we are allowed to implement 'Partial Function Application', which is where we can do 'sum 1', which would not be allowed in our original definition, but is allowed in the curried version.

You can also use the function 'curry' to curryify your procedure:

```Haskell
curry :: ((a, b) -> c) -> a -> b -> c

curry sum
```

Curry will return the curryified version of the function you call it with.

### Types

You can create your own types in haskell, the syntax is as follows:

```Haskell
type NewType = OldType
type String = [char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

data NewType = 
  Cons1 Type1
  | Cons2 Type2
  | Cons3 Type3
  ...
  | ConsN TypeN
```

The last bit of code defines a new type. Type1 .. TypeN are previously defined types, and Cons1 .. ConsN are constructors. The type is omitted if the constructor does not need an argument (such constructors are called constants)

### Type Classes

A simple way to create a member of the Eq class is shown below:

```Haskell
data Tree = None | Node (Tree a) a (Tree a) deriving Eq
```

This does not overload Eq, it just uses Eq to compare Btrees, you can overload Eq by doing the following:

```Haskell
instance Eq Tree where
  (Tree (Node Tree a Tree)) == (Tree (Node Tree a Tree)) = a == a

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)
```

Essentially saying if the roots of two trees are the same then the trees are the same. You can also make your own type classes:

```Haskell
instance hello a where
  hello 0 = 0
  hello 1 = 1 
  hello _ = 3
```

### List Comprehension

Haskell supports list comprehension

```Haskell
map f xs = [f x | x <- xs]

filter p xs = [x | x <- xs, p x]
```

### Lazy Evaluation

Don't evaluate before you have to: haskell only evaluates variables when absolutely necessary, so or (True, Error) doesn't crash, it returns True. Evaluations of expressions are deferred until their results are needed by other computations. A **thunk** is an unevaluated value with a recipe that explains how to evaluate it.

### Infinite Numbers

```Haskell
numbers3 = [1 ..]

main = do
  print $ take 101 numbers3
```

## Prolog

There are multiple layers to Prolog, the first is the constants and variables. This is layer 0. The first are the terms. These include constants, variables, and compound terms - applications of any n-ary functor to any n terms - are terms. The second are any atomic formulae. These are n-ary relations applied to n terms. Formulae are true or false, terms denote entities in the world. Atmoic formulae in prolog are facts, queires, or components of more complicated statements.

A query is a proposed fact that is to be proven, it is has no variables it must return true/false, if it has variables, it returns the appropriate values of those variables

```Prolog
% Prolog defines facts and relations

% These are facts
isANumber(1).
isANumber(2).
isANumber(3).
isANumber(4).
isANumber(5).
isANumber(6).
isANumber(7).
isANumber(8).
isANumber(9).
isANumber(10).

% These are relations / rules

both(X, Y) :- isANumber(X), isANumber(Y).
one(X, Y) :- isANumber(X); isANumber(Y).
```

The third layer is the complex formulae, which are formed by combining simpler formulae. A conjunction is ',', and a disjunction is ';'. Implications are written backward (conclusion first) and the conclusion must be an atomic formulae, its written using ':-'. A horn clause is:

c <- h1 ^ h2 ^ h3 ^ h4 ^ ... ^ hn

It means "the consequent is true if all of the antecedents are true". Non-horn clause formulae can be converted into logically equaivalent horn-formulae, using tautologies (DeMorgans Law, Implication Law, Distributive law)

Two atomic formulae unify iff they can be made syntactically indentical by replacing their variables with other terms:

both(X, Y) unifies with both(X, 2) by replacing Y with 2.

A substitution is a function that maps variables ot prolog terms: \{X/sue, Y/bob\} and an instantiation is an application of a substitution to all the free variables in a formula or term.
