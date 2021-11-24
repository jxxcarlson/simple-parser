# About L0

L0 is a simple markup language which we use  to illustrate an approach to fault-tolerant parsing. Here is a short paragraph
in L0:

```
Pythagoras says that for a [i right] triangle, $a^2 + b^2 = c^2$, where 
the letters denote the lengths of the altitude, base, and hypotenuse.
Pythagoras was [i [blue quite] the dude]! Still, he didn't know about
code: `fact(n) = if n == 0 then 1 else n*fact(n-1)`.
```

In an expression like `[i right]`, the `i` stands for the italicize function and 
`right` serves as an argument to it.  In the expression 
`[i [blue quite] the dude]`, "quite" is rendered in italicized blue text while "the dude" is simply italicized. Apart from special expressions like the ones used for 
mathematical text and code,  L0 consists of ordinary text and the Lisp-like expressions bounded  by brackets.

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

- Try to get the token at index `state.tokenIndex`; it will be either `Nothing`or
  `Just token`.
- If the return value is `Nothing`, examine the stack. If it is empty, 
  the loop is complete.  If it is nonempty, the stack could not be 
  reduced.  This is an error, so we call `recoverFromError state`.
- If the return value is `Just token`, push the token onto the stack or 
  commit it immediately, depending on the nature of the token and 
  whether the stack is empty.  Then increment 
  `state.tokenIndex`, call `reduceStack` and then re-enter the loop.

Below we describe the tokenizer, the parser, and error recovery. Very briefly, error recovery works by pattern matching on the reversed stack. The push or commit strategy guarantees that the stack begins with a left bracket token, a math token, or a code token. Then we proceed as follows:

- If the reversed stack begins with two left brackets, push an error message onto 
  `stack.committed`, set `state.tokenIndex` to the token index of the second
   left bracket, clear the stack, and re-run the parser on the truncated token list.
   
- If the reversed stack begins with a left bracket followed by a text token which we
  take to be a function name, push an error message onto `state.committed`, 
  set `state.tokenIndex` to the token index of the function name plus one, clear
  the stack, and re-run the parser on the truncated token list.
  
- Etc: a few more patterns, e.g., for code and math.
   
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
    | MathToken Meta
    | CodeToken Meta
    | TokenError (List (DeadEnd Context Problem)) Meta
    
type alias Meta =
    { begin : Int, end : Int, index: Int }
```

Here `LB` and `RB` stand for left and right-brackets;
`S` stands for string data, which in practice means "words" (no interior spaces)
and `W` stands for whitespace.  The string "$" generates a `MathToken`, 
while a backtick generates a `CodeToken.`  Thus

```
> import Parser.Token exposing(..)
> run "[i foo] $x^2$" |> List.reverse
  [  LB        { begin = 0, end = 0, index = 0   }
   , S "i"     { begin = 1, end = 1, index = 1   }
   , W (" ")   { begin = 2, end = 2, index = 2   }
   , S "foo".  { begin = 3, end = 5, index = 3   }
   , RB        { begin = 6, end = 6, index = 4   }
   , W (" ")   { begin = 7, end = 7, index = 5   }
   , MathToken { begin = 8, end = 8, index = 6   }
   , S "x^2"   { begin = 9, end = 11, index = 7  }
   , MathToken { begin = 12, end = 12, index = 8 }
 ]
   
```

The `Meta` components locates 
the substring tokenized in the source text and also carries an index which locates
a given token in a list of tokens.

The `Token.run` function has a companion which gives less verbose output:

```
> import Simple

> Simple.tokenize "[i foo] $x^2$" |> List.reverse
  [LBS,SS "i",WS (" "),SS "foo",RBS,WS (" "),MathTokenS,SS "x^2",MathTokenS]
```

This is useful for debugging.


## Parser

We briefly sketched the operation of the parser in the introduction.  Here we give some more detail.  The functional loop is controlled by the `nextStep` function listed 
below.  If retrieving a new token at index `state.tokenIndex` fails, there are two 
alternatives. If the stack is empty, then all tokens have been successfully parsed, and the 
parse tree stored in `state.committed` represents the full input text.  If the stack 
is non-empty, then that is not true, and so an error recovery strategy is invoked.  

If  a new token is acquired, it is either converted to an expression and pushed onto `state.committed`, or pushed onto the stack.  Tokens such as those representing a word of source text, are pushed to the stack if the stack is non-empty and are converted and pushed to `state.converted` otherwise.  Tokens such as those representing left and right braces or math and code tokens are always pushed onto the stack.

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


## The Reducibility Algorithm

To determine reducibility of a list of tokens, that list is first 
converted to a list of symbols, where

```
type Symbol = L | R | M | C | O
```

That is, left or right bracket, math or code token, or something else. 
Reducibility is a property of the corresponding reversed symbol list:

- If the symbol list starts and ends with `M`, it is reducible.

- If the symbol list starts and ends with `C`, it is reducible.

- If the symbol list starts and ends with `LB`, we apply the function `reducible`.
  If the list is empty, return True.  If it is nonempty, find the first matching `RB`;
  delete it and the initial `LB` and apply `reducible` to what remains.
  
The first matching `RB` is computed as follows.  Assign a value of +1 to `LB`,
-1 to `RB`, and 0 to `O`.  Then compute cumulative sums of the list of values.
The index of the first cumulative sum to be zero, if it exists, defines the match.

Example 1:

```
 L,  O,  R,  L,  O,  O,  R
+1,  0, -1, +1,  0,  0, -1
+1, +1,  0, ...
```

The first `R` is the match.


Example 2:


```
 L,  O,  L,  O,  O,  O,  R,  R
+1,  0, +1,  0,  0,  0, -1, -1
+1, +1, +2, +2, +2, +2, +1,  0
```

The last `R` is the match.

## Reducing the Stack

Function `reduceState` operates as follows.  It first determines, using function
`Parser.Match.reducible`, whether the stack is reducible.  We describe the algorithm
for this below.  If it is reducible, then the we apply 


```
eval : List Token -> List Expr
```

The result of this function application is prepended to `state.committed` and the stack is cleared.  If the stack is not reducible, then the state is passed on unchanged, eventually to be dealt with by the error recovery mechanism.

The `eval` function belies the affinity of L0 with Lisp, albeit at a 
far lower level of sophistication.  It operates as follows.  First, the reversed
stack is examined to see if it begins with `LB` token and ends with the `RB` token.
In that case the reversed token list has the form 

```
  LB _ :: token :: ... args ... :: RB _ :: []
```

If `token` is of the form `S fName _`, then we can form

```
  Expr fName (evalList args)
```

where `evalList : List Token -> List Expr`.  If the reversed stack does not have the 
correct form, then a one-element list of expressions noting an error is returned.  

Function `evalList` 


*NOTE:* `eval` and `evalList` call eachother.





## Error Recovery







