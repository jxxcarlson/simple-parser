# About L0

L0 is a simple markup language which we use  to illustrate an approach to fault-tolerant parsing. Here is a short paragraph
in L0:

```
Pythagoras says that for a [i right] triangle, $a^2 + b^2 = c^2$, where 
the letters denote the lengths of the altitude, base, and hypotenuse.
Pythagoras was [i [blue quite] the dude]!
```

In an expression like `[i right]`, the `i` stands for the italicize function and 
`right` serves as an argument to it.  In the expression 
`[i [blue quite] the dude]`, "quite" is rendered in italicized blue text while "the dude" is simply italicized. Apart from special expressions like the one used for 
mathematical text and code,  L0 consists of ordinary text and expressions bounded 
by brackets.The syntax, of course, is inspired by Lisp.

The idea behind the parser is to first transform the source text into 
a list of tokens, then convert the list of tokens into a list of expressions using
a kind of shift-reduce parser.  The shift-reduce parser is a functional loop that
operates on a value of type `State`, one field of which is a stack of tokens:

```
type alias State =
    { step : Int
    , tokens : List Token
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    }
    
run : State -> State
run state =
    loop state nextStep
``` 

The `nextStep` function operates as follows

- Try to get the token at index `state.tokenIndex`; it will be either `Nothing`
  `Just token`.
- If the return value is `Nothing`, examine the stack. If it is empty, 
  the loop is complete.  If it is nonempty, the stack could not be 
  reduced.  This is an error, so we call `recoverFromError state`.
- If the return value is `Just token`, push the token onto the stack or 
  commit it immediately, depending on the nature of the token and 
  whether the stack is empty.  Then increment 
  `state.tokenIndex`, call `reduceStack` and then re-enter the loop.

Below we describe

- tokenizer
- parser
- error recovery

Very briefly, error recovery works by pattern matching on the reversed stack. The push or commit strategy guarantees that the stack begins with a left bracket token. Then we proceed as follows:

- If the reversed stack begins with two left brackets, push an error message onto 
  `stack.committed`, set `state.tokenIndex` to the token index of the second
   left bracket, clear the stack, and re-run the parser on the truncated token list.
   
- If the reversed stack begins with a left bracket followed by a text token which we
  take to be a function name, push an error message onto `state.committed`, 
  set `state.tokenIndex` to the token index of the function name plus one, clear
  the stack, and re-run the parser on the truncated token list.
  
- Etc: a few more patterns.
   
In other words, when an error is encountered, we make note of the fact in `state.committed` and skip forward in the list of tokens in an attempt to recover from the error.  In this way two properties are guaranteed:


- A syntax tree is built based on the full text.

- Errors are signaled in the syntax tree and therefore in the rendered text.

- Text following an error is not messed up.


The last property is a consequence of the "greediness" of the recovery algorithm.









## Tokenizer

The tokenizer converts a string into a list of tokens, where


```
type Token
    = LB Meta
    | RB Meta
    | S String Meta
    | W String Meta
    | VerbatimToken String String Meta
    | TokenError (List (DeadEnd Context Problem)) Meta
    
type alias Meta =
    { begin : Int, end : Int, index: Int }
```

Here `LB` and `RB` stand for left and right-brackets;
`S` stands for string data, which in practice means "words" (no interior spaces)
and `W` stands for whitespace.  A `Verbatim` token is for math or code.  Thus

```
> import Parser.Token exposing(..)
> run "[i foo]" |> List.reverse
  [ LB      { begin = 0, end = 0, index = 0 }
  , S "i"   { begin = 1, end = 1, index = 1 }
  , W (" ") { begin = 2, end = 2, index = 2 }
  , S "foo" { begin = 3, end = 5, index = 3 }
  , RB      { begin = 6, end = 6, index = 4 }
  ]
```

The `Meta` components locates 
the substring tokenized in the source text and also carries an index which locates
a given token in a list of tokens.

The `Token.run` function has a companion which gives less verbose output:

```
> import Simple

> Simple.tokenize "[f a]" |> List.reverse
  [LBS, SS "f", WS (" "), SS "a", RBS]
  
> Simple.tokenize "$a^2 + b^2 = c^2$"
  [VerbatimTokenS "math" ("$a^2 + b^2 = c^2$")]
  
> Simple.tokenize "`a[0] = 1`"
  [VerbatimTokenS "code" ("`a[0] = 1`")]
```

## Parser

The parser converts a list of tokens into a list of expressions, where

```
type Expr
    = Expr String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
    | Error String
```

Parsing is carried out by a functional loop with 


```
type alias State =
    { step : Int
    , tokens : List Token
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    }
```

The `step` variable is a counter for the number of times the loop has been run; it
plays no essential role but is convenient for debugging. Note that the stack is
a list of elements that can be either tokens or expressions.  We will see how
this is used below. The top level code for the parser is

```
parse : String -> State
parse str =
    run (init str)

run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })
```

where `nextStep` operates as follows: 

- If the list of tokens is empty, the `reduce` function is applied to the 
    current state.  Suppose that the stack of the updated state is non-empty.
    Its head is either a `Right expr` or a `Left token`.  In the first case,
    the expression is pushed onto the current list of committed items, and 
    the item just processed is dropped from the stack.  The second case is 
    an unhandled error. In either case, the loop exits with `Done`.
    
- If the list of tokens is non-empty the head token is dropped from the list
  of tokens and either pushed onto the stack as `Left token` or converted to
  an expression and pushed onto the list of committed expressions.  The token
  is pushed onto the stack if the stack is non-empty or if it is a bracket
  or an error.  This is carried out by function `pushToken`.
  

- After the token has been pushed or committed, the state is reduced and the loop is re-entered.

## Reduction

The reduce function operates by pattern matching on the stack, applying one of
five rules, `F`, `A1`, `A2`, `A3`, or `M`, returning a record with the rule used
and the result of applying it: 

```
reduce : List (Either Token Expr) 
         -> { rule : Rule, result : List (Either Token Expr) }
```


- **Rule F,** *Form function:* `Left (S name _) :: Left (LB _) :: rest -> Right (Expr name []):: (Left LB _) :: rest`


- **Rule A1,** *Add argument:* `Left (S content _) :: Right (Expr name exprs) :: rest -> Right (Expr name (Text content :: exprs) :: rest`


- **Rule A2,** *Add argument:* `Left (W content _) :: Right (Expr name exprs) :: rest -> Right (Expr name (Text content :: exprs) :: rest`
 
 
- **Rule A3,** *Add argument:* `Right (E name' exprs' _) :: Right (Expr name exprs) :: rest -> Right (Expr name (E name' exprs' _ :: exprs) :: rest` 

- **Rule M,** *Match brackets:* `Left (RB _) :: rest ->` (1) find the longest prefix of `rest` not containing token `LB`.  Mark the elements of this list as _evaluated_ by 
applying the constructor `EV` to each of them.  (2) Let the suffix be the part of the stack obtained by dropping the prefix plus the next element. (3) return `prefix ++ suffix`.  In the returned value, the left-most pair `RB` ... `LB` has been deleted.

The function `reduceState : State -> State` operates by first applying function 
`reduce` to `state.stack` to update the state.  If rule `M` was applied, the bracket 
count in `state` is decremented and updated. If in addition the bracket count is now zero, the function `reduceStateM` is applied to the updated state.

The function `reduceStateM` commits the stack by reducing the stack, then converting 
to an expression which is prepended to `committed`.


```
finalExpr = (reduce state.stack).result |> List.map unevaluated |> apply

committed = finalExpr :: state.committed
```

The function `unevaluated` removes the wrapper `EV` if present.  The left-most
element of the stack is of the form `Expr name exprs _` and the remaining elements
are, after conversion to an `Expr` if need be, prepended to the list `exprs` to form a new expression.