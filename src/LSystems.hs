module LSystems where

import Graphics
import System.Random

type Rule = (Char, String)

type Rules = [Rule]

type System = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: System

type Vertex = (Float, Float)

type TurtleState = (Vertex, Float)

type Stack = [TurtleState]

type ColouredLine = (Vertex, Vertex, Colour)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (angle, base, rules) = angle

-- |Returns the base string for the given system.
base :: System -> String
base (angle, base, rules) = base

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (angle, base, rules) = rules

-- |Look up a character in the given set of rules.
-- Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar lookupKey rulesData = snd.head $ filter ((== lookupKey).fst) rulesData

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules command = concat $ map (`lookupChar` rules) command

-- |Expand a command ‘n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules base n = iterate (expandOne rules) base !! n

-- |Move a turtle.
-- * 'F' moves distance 1 in the current direction.
-- * 'L' rotates left according to the given angle.
-- * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move command ((x, y), currRot) rotateAmount
    |command == 'F' = ((newX, newY), currRot)
    |command == 'L' = ((x, y), currRot + rotateAmount)
    |command == 'R' = ((x, y), currRot - rotateAmount)
        where
            newX = x + cos (currRot / 180 * pi)
            newY = y + sin (currRot / 180 * pi)

-- |Trace lines drawn by a turtle using the given colour, following the
-- commands in the string and assuming the given initial angle of rotation.
-- Method 1 (recursive)
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 commands angle colour = fst $ trace1Helper commands angle colour startTS
    where
        startTS = ((0,0), 90)

trace1Helper :: String -> Float -> Colour -> TurtleState -> ([ColouredLine], String)
trace1Helper [] _ _ _ = ([], [])
trace1Helper (c:cs) angle colour turtle
    |c == ']'        = ([], cs)
    |c == '['        = (ts ++ fst (trace1Helper rs angle colour turtle), [])
    |commandIsRotate = (ts2, rs2)
    |otherwise       = ((fst turtle, fst newTurtle,colour):ts2, rs2)
        where
            (ts, rs) = trace1Helper cs angle colour turtle
            (ts2, rs2) = trace1Helper cs angle colour newTurtle
            newTurtle = move c turtle angle
            commandIsRotate = c == 'L' || c == 'R'

-- |Trace lines drawn by a turtle using the given colour, following the
-- commands in the string and assuming the given initial angle of rotation.
-- Method 2 (stack)
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 cmds angle colour = finalCLs
    where
        (finalCLs, _, _) = foldl (trace2Helper colour angle) ([], ((0,0), 90), []) cmds

-- |The helper function for trace2, It takes the next command(Char) and moves
-- the turtle accordingly, and updates the TurtleState and the ColouredLines
-- if needed. It also handles branches using a stack
-- |The naming of the variables should be self-explanatory
trace2Helper :: Colour -> Float -> ([ColouredLine], TurtleState, Stack) -> Char -> ([ColouredLine], TurtleState, Stack)
trace2Helper colour angle (oldCL, turtle, stack) cmd
    |cmd == '['      = (oldCL, turtle, (turtle:stack))
    |cmd == ']'      = (oldCL, head stack, tail stack)
    |commandIsRotate = (oldCL, newTurtle, stack)
    |otherwise       = (newCL, newTurtle, stack)
        where
            commandIsRotate = cmd == 'L' || cmd == 'R'
            newTurtle = move cmd turtle angle
            newCL = (fst turtle, fst newTurtle, colour):oldCL

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Further extensions and random experiments to explore new ideas

-- From this point there is no focus on making the most optimal and readable
-- code, so i apologize if this part will cause headaches
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A variation where there is a "Fade" going on
-- |BaseAlbedo, which is the base color of the tree at the root
-- |TargetAlbedo, which is the color the base color will fade to
-- the further it is from the root
-- |FadeRadius, which is the amount of lines it takes for the fade
-- to complete
type Fade = (Colour, Colour, Float)

-- |The current colour, the amount of radius left and the relative value changes
type FadeInfo = (Colour, Float, Colour)

-- |The stack needed to remember where we were at the begining of the branch
type AdvStack = ([ColouredLine], TurtleState, Stack, [FadeInfo], FadeStack, [Float], Float)
type FadeStack = [[FadeInfo]]

-- A variation where the l-system is probabilistic
-- |These are new types that are defined to accomodate the probability
type ProbRule = (Char, Float, String)

type ProbRules = [ProbRule]

type ProbSystem = (Float, String, ProbRules)

-- |A variation where small flowers, symbols, patterns will be put at the end
-- of each branch. The patterns will be stored as a path, and a helper func
-- will draw it out.
-- |These are the new types that will be needed for this
-- |take a command and a rotation amount
type Command = (Char, Float)

-- |A list of commands which together add up to a symbol
type Path = ([Command], (Colour, Colour))

-- |This holds all the different symbols
type SymbolDataBase = [Path]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- This holds all the different symbols that we can put at the end of each branch
-- All of them are a list of commands
-- 0:long leaves, 1:duplet leaves, 2:circular leaves, 3:flower1, 4:flower2
symbolsDataBase :: SymbolDataBase
symbolsDataBase 
    = [ ([('L',30), ('F',0), ('L',30), ('F',0), ('R',60), ('F',0), ('L',30), ('F',0),
        ('R',60), ('F',0), ('L',30), ('F',0), ('R',60), ('F',0), ('L',30), ('F',0)],
        ((0.039, 0.482, 0.078), (0.705, 0.992, 0.733)))
        , ([('L',45), ('F',0), ('R',90), ('F',0), ('F',0), ('L',90), ('F',0), ('L',90),
        ('F',0), ('L',90), ('F',0), ('F',0), ('R',90), ('F',0)],
        ((0.035, 0.525, 0.078), (0.086, 0.733, 0.145)))
        , ([('L',60), ('F',0), ('R',60), ('F',0), ('R',60), ('F',0), ('R',60), ('F',0),
        ('R',60), ('F',0), ('R',60), ('F',0)],
        ((0.176, 0.360, 0.196), (0.462, 0.878, 0.498)))
        , ([('L',45), ('F',0), ('R',90), ('F',0), ('R',90), ('F',0), ('R',90), ('F',0)],
        ((1, 0.580, 0.929), (0.909, 0.349, 0.388)))
        , ([('L',30), ('F',0), ('L',180), ('F',0), ('L',150), ('L',30), ('F',0),
        ('L',180), ('F',0), ('L',150), ('L',30), ('F',0), ('L',180), ('F',0),
        ('L',150)],
        ((0.654, 0.349, 0.909), (0.909, 0.349, 0.6)))
        ]

-- A pretty random probabilistic system. It is basically the merge of
-- tree and bush. looks like a huge web or bush viewed from the top
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

probTree :: ProbSystem
probTree
    = ( 25
        , "X"
        , [ ('X', 0.5, "X[+MX]M")
          , ('X', 0.5, "X[-MX]M")
          , ('M', 1, "M")
          , ('[', 1, "[")
          , (']', 1, "]")
          , ('+', 1, "+")
          , ('-', 1, "-")
        ]
    )
    

-- |Best used for bush 8
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

-- |Best used for bush 8
pinkWhite :: [Fade]
pinkWhite
    = [ ((1, 0.721, 0.941), (1, 0.4, 0.870), 150)
        , ((1, 0.4, 0.870), (1, 0.341, 0.623), 150)
        , ((1, 0.341, 0.623), (0.996, 0.803, 0.886), 150)
        , ((0.996, 0.803, 0.886), (0.980, 0.258, 0.933), 150)
        , ((0.980, 0.258, 0.933), (1, 0.721, 0.941), 150)
        ]

-- |Best used for bush 8
treeFade :: [Fade]
treeFade
    = [ (brown, brown, 225)
        , (brown, green, 150)
        , (green, green, 75)
        , (green, yellow, 125)
        , (yellow, orange, 125)
        ]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- COLOR FADE
-- I will be extending on the trace2 method as I prefer it way more than trace1
trace2Fade :: [Float] -> String -> Float -> [Fade] -> [(Int, Float, Float)] -> Float -> [ColouredLine]
trace2Fade rands cmds angle fades symbols prob = finalCLs
    where
        (finalCLs, _, _, _, _, _, _) = foldl (trace2FadeHelper angle symbols prob) ([], ((0,0), 90), [], fadeInfo, [], rands, 0) cmds
        fadeInfo = map convertFade fades

-- |Converts a Fade into a FadeInfo for latter functions to handle
convertFade :: Fade -> FadeInfo
convertFade (startC@(bR, bG, bB), (tR, tG, tB), rad) = (startC, rad, colorRelChange)
    where
        colorRelChange = ((tR - bR) / rad, (tG - bG) / rad, (tB - bB) / rad)

scaleFade :: [Fade] -> Float -> [Fade]
scaleFade f n = map (\(a,b,c) -> (a,b,c*n)) f

trace2FadeHelper :: Float -> [(Int, Float, Float)] -> Float -> AdvStack -> Char -> AdvStack
trace2FadeHelper angle symbols prob (oldCLs, prevTS, stack, oldFI, fadeStack, rands@(r:r2:r3:rs), strPoint) cmd
    |cmd == '[' = (oldCLs, prevTS, (prevTS : stack), oldFI, (oldFI : fadeStack), rands, strPoint+1)
    |cmd == ']' = (newCLSymbol, head stack, tail stack, head fadeStack, tail fadeStack, rs, strPoint)
    |cmdIsRotate = (oldCLs, newTS, stack, oldFI, fadeStack, rands, strPoint)
    |otherwise = (newCLs, newTS, stack, updFI, fadeStack, rands, strPoint)
        where
            cmdIsRotate = cmd == 'L' || cmd == 'R'
            newTS = move cmd prevTS angle
            (updFI, newC) = changeColour oldFI
            newCLs = (fst prevTS, fst newTS, newC):oldCLs
            newCLSymbol = (trailSymbol r r2 r3 symbols prob prevTS) ++ oldCLs

-- |This function takes care of handling the color fading
changeColour :: [FadeInfo] -> ([FadeInfo], Colour)
changeColour fades@(f@(c@(cR, cG, cB), r, rel@(rR, rG, rB)):fs) = (finalFIs, newC)
    where
        newC = if r > 0 then (cR + rR, cG + rG, cB + rB) else c
        newFI = (newC, r - 1, rel)
        finalFIs =  if length fades > 1 
                        then if r > 0 then newFI:fs else fs
                        else if r > 0 then [newFI] else [f]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- PROBABILITY
-- |Since we will be using probability, we will need some sort of rng,
-- for this, I'll be using System.Random, which perfectly fulfills
-- the role we want here. No need for cryptographic quality randoms...

-- |Takes in a list of randoms between 0-1, a key and a set of rules, and finds
-- a corresponding definition using the probabilities
lookupCharProb :: ProbRules -> Float -> Char -> String
lookupCharProb probRulesData rand lookupKey = def
    where
        a = [(k, f, s) | (k, f, s) <- probRulesData, k == lookupKey]
        ratios = [prob | (_, prob, _) <- a]
        index = (length (takeWhile (>= 0) (scanl (-) rand ratios))) - 1
        (_, _, def) = a !! index

-- |The probability version of expandOne
expandOneProb :: ProbRules -> [Float] -> String -> String
expandOneProb rulesSet rands command = concat $ map (\(c,r) -> lookupCharProb rulesSet r c) cmds
    where
        cmds = zip command rands

-- |The probability version of expand
expandProb :: ProbRules -> String -> Int -> [Float] -> String
expandProb rules base n rands = iterate (expandOneProb rules rands) base !! n

-- |Returns the rotation angle for the given ProbSystem.
angleProb :: ProbSystem -> Float
angleProb (angle, base, rules) = angle

-- |Returns the base string for the given ProbSystem.
baseProb :: ProbSystem -> String
baseProb (angle, base, rules) = base

-- |Returns the set of rules for the given ProbSystem.
rulesProb :: ProbSystem -> ProbRules
rulesProb (angle, base, rules) = rules

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- SYMBOLS
-- |The function takes a list of indexes and 2 floats, which are the indexes
-- of the symbols, and their probability to appear and their size.
-- There is also a overall probability which is the prob of any
-- symbol "spawning". The size multiplier also enables to enlarge specific
-- type of symbols for visibility
trailSymbol :: Float -> Float -> Float -> [(Int, Float, Float)] -> Float -> TurtleState -> [ColouredLine]
trailSymbol rand1 rand2 rand3 potSyms prob currTS
    |rand1 < prob = trailSymbolBuilder multip clr (symbolsDataBase !! chosen) currTS
    |otherwise    = []
        where
            chosen = trailSymbolChooser rand2 potSyms
            clr = trailColorChooser rand3 (symbolsDataBase !! chosen)
            multip = [m | (i, _, m) <- potSyms, i == fromIntegral chosen] !! 0

-- |This function computes what the symbols's colour will be, which is randomRs
-- between two pre-defined colours
trailColorChooser :: Float -> Path -> Colour
trailColorChooser rand (_, ((bR, bG, bB), (tR, tG, tB))) = (bR + rDiff * rand, bG + gDiff * rand, bB + bDiff * rand)
    where
        rDiff = tR - bR
        gDiff = tG - bG
        bDiff = tB - bB

-- |This function builds out the ColouredLine list from a (Int) size multiplier
-- and a given colour
trailSymbolBuilder :: Float -> Colour -> Path -> TurtleState -> [ColouredLine]
trailSymbolBuilder _ _ ([], (_, _)) _ = []
trailSymbolBuilder multiplier colour ((p : ps), cs) prevTS
    |cmd == 'F' = newCL : (trailSymbolBuilder multiplier colour (ps, cs) newTS)
    |otherwise = trailSymbolBuilder multiplier colour (ps, cs) newTS
        where
            newTS = moveAdvanced cmd prevTS multiplier dgr
            (cmd, dgr) = p
            newCL = (fst prevTS, fst newTS, colour)

-- |This function chooses which one will be spawned using the probability
-- of each given symbol
trailSymbolChooser :: Float -> [(Int, Float, Float)] -> Int
trailSymbolChooser rand potSyms = ind
    where
        (ind, _, _) = potSyms !! index
        index = (length (takeWhile (>= 0) (scanl (-) rand ratios))) - 1
        ratios = [prob | (_, prob, _) <- potSyms]

-- |An advanced version of the move function that handles movement amounts
moveAdvanced :: Char -> TurtleState -> Float -> Float-> TurtleState
moveAdvanced command ((x, y), currRot) moveAmount rotateAmount
    |command == 'F' = ((newX, newY), currRot)
    |command == 'L' = ((x, y), currRot + rotateAmount)
    |command == 'R' = ((x, y), currRot - rotateAmount)
        where
            newX = x + cos (currRot / 180 * pi) * moveAmount
            newY = y + sin (currRot / 180 * pi) * moveAmount

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- GENETIC ALGORITHM
-- This part implements a generic algorithm that tries to emulate the evolution of trees by using a weighted fitness function.
-- The fitness function is a weighted sum of the following:
-- Highest reach: most trees in nature try to be as tall as possible as that enables them to collect the most sunlight possible
-- Symmetry on the vertical axis, or at least a balanced distribution of weight
-- Sunlight absorbing capabilities.
-- Structural strength (minus fitness for potential weak points)

highestPoint :: [ColouredLine] -> Float
highestPoint = foldl (\acc (a,b,c) -> if snd b > acc then snd b else acc) 0

symmetryPoint :: [ColouredLine] -> Float
symmetryPoint xs = foldl (\acc (a,b,c) -> acc + (fst a) + (fst b)) 0 xs

absorbingPoint :: [ColouredLine] -> [ColouredLine] -> Float
absorbingPoint [(a,b,c)] orig = if isLightCollector b orig then 1 else 0
absorbingPoint ((x1,y1,_):(x2,y2,c):xs) orig
    |y1 == x2  = if isLightCollector y1 orig then 1 + point else point
    |otherwise = point
        where
            point = absorbingPoint ((x2,y2,c):xs) orig

isLightCollector :: Vertex -> [ColouredLine] -> Bool
isLightCollector a [] = True
isLightCollector a ((x1,x2,_):xs) = (not sameColumn || (l < snd a)) && (isLightCollector a xs) 
    where
        sameColumn = if fst x1 < fst x2 then (fst x1 < fst a) && (fst a < fst x2) else (fst x2 < fst a) && (fst a < fst x1)
        m = if fst x1 < fst x2  then abs(snd x2 - snd x1) / abs(fst x2 - fst x1) else (snd x1 - snd x2) / (fst x1 - fst x2)
        l = (snd x1) + m * (fst a - fst x1)

testSystem :: System
testSystem = generate dnaGen 32 17

printEvaluate :: System -> Int -> Int -> (Float, Float, Float, Float)
printEvaluate sys n seed = (highest, absorb, symmetry, structure)
    where
        highest = highestPoint cls
        symmetry = symmetryPoint cls
        absorb = absorbingPoint (reverse cls) (reverse cls)
        (cls,structure) = trace2FadeGen randomList (lSystem sys n) (angle sys) rainbow [] 0 
        randomList = help seed

evaluate :: System -> Int -> Int -> Float
evaluate sys n seed = 0.05*highest + 2*absorb - 0.5*symmetry - 0.05*structure
    where
        highest = highestPoint cls
        symmetry = symmetryPoint cls
        absorb = absorbingPoint (reverse cls) (reverse cls)
        (cls,structure) = trace2FadeGen randomList (lSystem sys n) (angle sys) rainbow [] 0 
        randomList = help seed

trace2FadeGen :: [Float] -> String -> Float -> [Fade] -> [(Int, Float, Float)] -> Float -> ([ColouredLine],Float)
trace2FadeGen rands cmds angle fades symbols prob = (finalCLs,strPoint)
    where
        (finalCLs, _, _, _, _, _,strPoint) = foldl (trace2FadeHelper angle symbols prob) ([], ((0,0), 90), [], fadeInfo, [], rands, 0) cmds
        fadeInfo = map convertFade fades

type DNAGen = [(Float, Char)]

dnaGen :: DNAGen 
dnaGen =   [(0.4, 'M'),
            (0.15, '-'),
            (0.15, '+'),
            (0.3, '[')
            ]

generateDNA :: DNAGen -> Int -> [Float] -> System
generateDNA gens n rands = (30, "M", [('M',dna),
                                      ('+',"+"),         
                                      ('-',"-"),
                                      ('[',"["),
                                      (']',"]")
                                      ])
    where 
        dna = generateDNASequence gens n rands

generate :: DNAGen -> Int -> Int -> System
generate gens n seed = generateDNA gens n randomList
    where
        randomList = help seed

generateDNASequence :: DNAGen -> Int -> [Float] -> String
generateDNASequence _ 0 _ = ""
generateDNASequence gens n (r:rs) 
    |next == '[' && n > nextBranchSize + 2 = ('[':nextBranch) ++ (']':(generateDNASequence gens (n-nextBranchSize-2) (drop (nextBranchSize+1) rs)))
    |next == '['                           = (generateDNASequence gens n rs)
    |otherwise                             = next:(generateDNASequence gens (n-1) rs)
        where
            next = generateDNAHelix gens r 
            nextBranchSize = (ceiling ((head rs) * 10))
            nextBranch = generateDNASequence gens nextBranchSize (tail rs)

generateDNAHelix :: DNAGen -> Float -> Char
generateDNAHelix gens rand = snd (gens !! index)
    where
        index = (length (takeWhile (>= 0) (scanl (-) rand ratios))) - 1
        ratios = map fst gens
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Some test systems.

cross
    = ( 90
        , "M-M-M-M"
        , [ ('M', "M-M+M+MM-M-M+M")
        , ('+', "+")
        , ('-', "-")
        ]
        )

triangle
    = ( 90
        , "-M"
        , [ ('M', "M+M-M-M+M")
        , ('+', "+")
        , ('-', "-")
        ]
        )

arrowHead
    = ( 60
        , "N"
        , [ ('M', "N+M+N")
        , ('N', "M-N-M")
        , ('+', "+")
        , ('-', "-")
        ]
        )

peanoGosper
    = ( 60
        , "M"
        , [ ('M', "M+N++N-M--MM-N+")
        , ('N', "-M+NN++N+M--M-N")
        , ('+', "+")
        , ('-', "-")
        ]
        )

dragon
    = ( 45
        , "MX"
        , [ ('M', "A")
        , ('X', "+MX--MY+")
        , ('Y', "-MX++MY-")
        , ('A', "A")
        , ('+', "+")
        , ('-', "-")
        ]
        )

snowflake
    = ( 60
        , "M--M--M"
        , [ ('M', "M+M--M+M")
        , ('+', "+")
        , ('-', "-")
        ]
        )

tree
    = ( 45
        , "M"
        , [ ('M', "N[-M][+M][NM]")
        , ('N', "NN")
        , ('[', "[")
        , (']', "]")
        , ('+', "+")
        , ('-', "-")
        ]
        )

bush
    = ( 22.5
        , "X"
        , [ ('X', "M-[[X]+X]+M[+MX]-X")
        , ('M', "MM")
        , ('[', "[")
        , (']', "]")
        , ('+', "+")
        , ('-', "-")
        ]
        )

mapper :: Rules
mapper
    = [ ('M', "F")
        , ('N', "F")
        , ('X', "")
        , ('Y', "")
        , ('A', "")
        , ('[', "[")
        , (']', "]")
        , ('+', "L")
        , ('-', "R")
        ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n = expandOne mapper (expand rs base n)

-- |The probabilistic version of lSystem
lSystemProb :: ProbSystem -> Int -> Int -> String
lSystemProb (_, base, rs) n seed = expandOne mapper (expandProb rs base n randomList)
    where
        randomList = help seed

-- |Uses the first tracing method
drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour = drawLines (trace1 (lSystem system n) (angle system) colour)

-- |Uses the second tracing method
drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour = drawLines (trace2 (lSystem system n) (angle system) colour)

-- |This is the version with color fading, takes a list of Fade
drawLSystem2Fade :: System -> Int -> [Fade] -> Int -> IO ()
drawLSystem2Fade system n fade seed = drawLines (trace2Fade randomList (lSystem system n) (angle system) fade [] 0)
    where
        randomList = help seed

-- |This is the version with fading and symbols at the end aswell
-- a list of symbols [(Int, Float, Float)], where they are a certain symbols
-- index in symbolsDataBase, their probability to appear, and their size
-- And a final probability which is a 1general one, that's the probability
-- that a branch will be symbolized
drawLSystem2FadeSymbol :: System -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> Int -> IO ()
drawLSystem2FadeSymbol system n fade symbols prob seed = drawLines (trace2Fade randomList (lSystem system n) (angle system) fade symbols prob)
    where
        randomList = help seed

-- |This is the version with fading, symbols and a probabilistic system
drawLSystem2FadeSymbolProb :: ProbSystem -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> Int -> IO ()
drawLSystem2FadeSymbolProb system n fade symbols prob seed = drawLines (trace2Fade randomList (lSystemProb system n seed) (angleProb system) fade symbols prob)
    where
        randomList = help seed

-- |Random number generation
help :: Int -> [Float]
help seed
    = do 
        { let g = mkStdGen seed
        ; let rng = (randomRs (0.0, 1.0) g)
        ; rng
        }

-- || need to create a main function that will iterate through a bunch of
-- pre-defined fancy looking L-Systems once they press enter

-- Notable cases
-- drawLSystem2FadeSymbol bush 8 treeFade [(1,1,1)] 0.3
-- drawLSystem2FadeSymbol bush 8 treeFade [(1,1,2)] 0.15
-- drawLSystem2FadeSymbol bush 8 treeFade [(1,1,2)] 0.05
-- drawLSystem2FadeSymbol bush 8 treeFade [(4,1,5)] 0.3
-- drawLSystem2FadeSymbol bush 8 rainbow [(4,1,5)] 0
-- drawLSystem2 cross 3 red
-- drawLSystem2 triangle 4 red
-- drawLSystem2 arrowHead 4 red
-- drawLSystem2 peanoGosper 4 red
-- drawLSystem2 dragon 10 red
-- drawLSystem2FadeSymbolProb probTree 7 rainbow [(2,1,0.24)] 0.65 66
