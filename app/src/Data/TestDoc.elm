module Data.TestDoc exposing (..)


text =
    """



| heading 1
L0 Examples

| heading 2
Inline constructions

| heading 3
Usual markup stuff

I [i thought] that this would be a [b good] idea, but I was [red sadly mistaken!].

Note that inline elements can [i [b be composed.]] That was italic
bold. Let's not be boring: we can also do colors: 
[blue blue stuff] and [red red stuff].  Of course, colors compose also: [i  [b [red Merry Christmas!]]]




| heading 3
Inline math

This is math: $a^2 + b^2 = c^2$


| heading 2
Block constructions


| heading 3
Math

[i Display math using pipes:]



|| math
\\int_0^1 x^n dx = \\frac{1}{n+1}

[i Display math using dollar signs:]

$$
\\int_0^\\infty e^{-x} dx = 1

| heading 3
Code

|| code
  a[0] = 1
    b[0] = |3|
       c[0] = b[0]
    || x = a + b + c ||


[i An "indent" block:]

| indent
Pythagoras said that if $a$, $b$, $c$ are the altitude, base, and
hypotenuse of a right triangle, then $a^2 + b^2 = c^2$.

| heading 2
Handling Errors

| heading 3
Inline errors

This line will have an [i incomplete italic element.

This line does not have a closing bracket: [blue blue sky

This line has too many right brackets: [blue blue sky]]

Here is an incomplete math element: $ a^2 + b^2 = c^2 . [i Oops!
I forgot a closing dollar sign. Bad!!]


| heading 3
Blocks errors

[i An unknown block name:]

|| foo bar
la di dah
do day!

[i An incomplete block name:]

| inden
Pythagoras said that if $a$, $b$, $c$ are the altitude, base, and
hypotenuse of a right triangle, then $a^2 + b^2 = c^2$.

[i A missing block name:

|
Pythagoras said that if $a$, $b$, $c$ are the altitude, base, and
hypotenuse of a right triangle, then $a^2 + b^2 = c^2$.




"""
