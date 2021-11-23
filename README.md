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
    { tokens : List Token
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

Below we describe the tokenizer, the parser, and error recovery. Very briefly, error recovery works by pattern matching on the reversed stack. The push or commit strategy guarantees that the stack begins with a left bracket token. Then we proceed as follows:

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

We briefly sketched the operation of the parser in the introduction.  Here we give some more detail.  The functional loop is controlled by the `nextStep` function listed 
below.  If retrieving a new token at index `state.tokenIndex` fails, there are two 
alternatives. If the stack is empty, then all tokens have successfully parsed, and the 
parse tree stored in `state.committed` represents the full input text.  If the stack 
is non-empty, then that is not true, and so an error recovery strategy is invoked.  

If  a new token is acquired, it is either converted to an expression and pushed onto `state.committed`, or pushed onto the stack.  Some tokens, such as those for math or code, are always converted and committed.  Other tokens, such as those representing a word of source text, are pushed to the stack if the stack is non-empty and are converted and pushed to `state.converted` otherwise.  Finally, tokens such as those representing left and right braces are always pushed onto the stack.

Once a token is either pushed or committed, the stack is reduced. We describe this 
process below.

```
run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })
        
nextStep : State -> Step State State
nextStep state =
    case List.Extra.getAt state.tokenIndex state.tokens of
        Nothing ->
            if List.isEmpty state.stack then
                Done state
                
            else
                recoverFromError state

        Just token ->
            pushToken token { state | tokenIndex = state.tokenIndex + 1 }
                |> reduceState
                |> Loop
                
```


## Reducing the Stack


