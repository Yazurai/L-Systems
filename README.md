# L-Systems

A popular way of defining plant-like structures is to use Lindenmayer Systems, or L-Systems for short.

Lindenmayer systems are named after the biologist Aristid Lindenmayer who was interested in describing the apparently fractal nature of plant growth using simple rewriting rules. L-Systems and their variants have been used extensively in a wide variety of computer graphics applications

## How Lindenmayer systems work
L-Systems work by repeatedly replacing the items in a sequence according to a fixed set of rewrite rules. Starting from an initial “seed” (the “axiom”) the sequence grows in size as the rewrites are repeatedly applied. In this exercise the items will be characters so that the rewrite problem becomes one of growing a string from an axiom to some final value. The characters in a sequence include drawing instructions for a turtle - an imaginary device on wheels with a pen underneath.

## Features

- Bracketed L-Systems for simulating branching
- Stochastic grammar, that enabled for seed based randomized generation
- Colour fades to freely colour the L-System (gradient based)
- Symbols at the end of branches (you can make leaves with it)
- 10+ L-System presets (including dragon, peano gosper, various trees)

## Full specification can be [found here](https://tbkhoa.web.elte.hu/projects/L-system-specification.pdf)

## Examples

![1](https://github.com/Yazurai/L-Systems/blob/master/examples/1.png)
![2](https://github.com/Yazurai/L-Systems/blob/master/examples/2.png)
![3](https://github.com/Yazurai/L-Systems/blob/master/examples/3.png)
![4](https://github.com/Yazurai/L-Systems/blob/master/examples/4.png)
![5](https://github.com/Yazurai/L-Systems/blob/master/examples/5.png)
![6](https://github.com/Yazurai/L-Systems/blob/master/examples/6.png)
![7](https://github.com/Yazurai/L-Systems/blob/master/examples/7.png)

A system can be specified in the following format: (turning angle, starting tree, reproduction rules)

```haskell
bush = ( 22.5
        , "X"
        , [ ('X', "M-[[X]+X]+M[+MX]-X")
          , ('M', "MM")
          , ('[', "[")
          , (']', "]")
          , ('+', "+")
          , ('-', "-")
          ])
```
Note, that here M translates into the turtle moving forward, meanwhile X doesn't do anything.
The entire mapping that the system uses by default is: 
```haskell
mapper :: Rules
mapper = [ ('M', "F")
        , ('N', "F")
        , ('X', "")
        , ('Y', "")
        , ('A', "")
        , ('[', "[")
        , (']', "]")
        , ('+', "L")
        , ('-', "R")
        ]
```
### The simplest form of usage is:
```haskell
drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
```
### To use color fades, use:
```haskell
drawLSystem2Fade :: System -> Int -> [Fade] -> Int -> IO ()
drawLSystem2Fade system n fade seed
```
To define a color fade:
```haskell
--(starting color, finish color, duration of fade)
--color can be given by predefined colors, or by giving an rgb triplet (0-1)
rainbow :: [Fade]
rainbow
    = [ (red, orange, 90)
        , (orange, yellow, 90)
        , (yellow, green, 90)
        , (green, cyan, 90)
        , (cyan, blue, 90)
        , (blue, purple, 90)
        , (purple, magenta, 90)
        , (magenta, pink, 90)
        , (pink, red, 90)
        ]
```
### To use symbols at the end of branches:
```haskell
-- prob gives the chances that a symbol will spawn at the end of a branch
-- symbols is a list of chosen symbols, with their probabilities (in total should equal to 1) and their size multiplier (in this order)
-- symbols: 0:long leaves, 1:duplet leaves, 2:circular leaves, 3:flower1, 4:flower2
drawLSystem2FadeSymbol :: System -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> Int -> IO ()
drawLSystem2FadeSymbol system n fade symbols prob seed
```
### To use a probabilistic L-System:
```haskell
drawLSystem2FadeSymbolProb :: ProbSystem -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> Int -> IO ()
drawLSystem2FadeSymbolProb system n fade symbols prob seed
```
To define a probabilistic L-System:
```haskell
treeBush :: ProbSystem
treeBush
    = ( 33
        , "X"
        , [ ('X', 0.5, "M-[[X]+X]+M[+MX]-X")
        , ('X', 0.5, "MM")
        , ('M', 0.5, "MM")
        , ('M', 0.5, "X[-M][+M][XM]")
        , ('[', 1, "[")
        , (']', 1, "]")
        , ('+', 1, "+")
        , ('-', 1, "-")
        ]
    )
```

### You can find a list of presets at the end of LSystems.hs
