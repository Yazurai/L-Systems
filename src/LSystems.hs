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
trace1 commands angle colour = []

--trace1Helper :: String -> Float -> Colour -> ([ColoredLine], String)
--trace1Helper [] _ _ = []
--trace1Helper (c : cs)

-- |Trace lines drawn by a turtle using the given colour, following the
-- commands in the string and assuming the given initial angle of rotation.
-- Method 2 (stack)
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 cmds angle colour = finalCLs
    where
        (finalCLs, _, _) = foldl (trace2Helper colour angle) ([], startTS, []) cmds
        convertedCommands = expandOne mapper cmds
        startPos = (0, 0)
        startRot = 90
        startTS = (startPos, startRot)


-- |The helper function for trace2, It takes the next command(Char) and moves
-- the turtle accordingly, and updates the TurtleState and the coloredLines
-- if needed. It also handles branches using a stack
-- |The naming of the variables should be self-explanatory
trace2Helper :: Colour -> Float -> ([ColouredLine], TurtleState, Stack) -> Char -> ([ColouredLine], TurtleState, Stack)
trace2Helper colour angle (oldCLs, prevTS, stack) command
    |command == '[' = (oldCLs, prevTS, (prevTS : stack))
    |command == ']' = (oldCLs, s, ss)
    |commandIsRotate = (oldCLs, newTS, stack)
    |otherwise = (newCLs, newTS, stack)
        where
            branchStart = '['
            branchEnd = ']'
            commandIsRotate = command == 'L' || command == 'R'
            rotateAmount = angle
            newTS = move command prevTS rotateAmount
            (oldPos, _) = prevTS
            (newPos, _) = newTS
            newCLs = (oldPos, newPos, colour) : oldCLs
            (s:ss) = stack


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
        (finalCLs, _, _, _, _, _) = foldl (trace2FadeHelper angle symbols prob) ([], startTS, [], fadeInfo, [], rands) cmds
        convertedCmds = expandOne mapper cmds
        startPos = (0, 0)
        startRot = 90
        startTS = (startPos, startRot)
        fadeInfo = map convertFade fades

-- |Converts a Fade into a FadeInfo for latter functions to handle
convertFade :: Fade -> FadeInfo
convertFade ((bR, bG, bB), (tR, tG, tB), rad) = fadeInfo
    where
        colorRelChange = ((tR - bR) / rad, (tG - bG) / rad, (tB - bB) / rad)
        startColour = (bR, bG, bB)
        fadeInfo = (startColour, rad, colorRelChange)

trace2FadeHelper :: Float -> [(Int, Float, Float)] -> Float -> ([ColouredLine], TurtleState, Stack, [FadeInfo], FadeStack, [Float]) -> Char -> ([ColouredLine], TurtleState, Stack, [FadeInfo], FadeStack, [Float])
trace2FadeHelper angle symbols prob (oldCLs, prevTS, stack, oldFI, fadeStack, rands) cmd
    |cmd == '[' = (oldCLs, prevTS, (prevTS : stack), oldFI, (oldFI : fadeStack), rands)
    |cmd == ']' = (newCLSymbol, s, ss, f, fs, rs)
    |cmdIsRotate = (oldCLs, newTS, stack, oldFI, fadeStack, rands)
    |otherwise = (newCLs, newTS, stack, updFI, fadeStack, rands)
        where
            cmdIsRotate = cmd == 'L' || cmd == 'R'
            newTS = move cmd prevTS angle
            (oldPos, _) = prevTS
            (newPos, _) = newTS
            (updFI, newC) = changeColour oldFI
            newCLs = (oldPos, newPos, newC):oldCLs
            (r:r2:r3:rs) = rands
            newCLSymbol = (trailSymbol r r2 r3 symbols prob prevTS) ++ oldCLs
            (s:ss) = stack
            (f:fs) = fadeStack

-- |This function takes care of handling the color fading
changeColour :: [FadeInfo] -> ([FadeInfo], Colour)
changeColour [f] = ([newFI], newC)
    where
        (c, r, rel) = f
        (cR, cG, cB) = c
        (rR, rG, rB) = rel
        newC = if r > 0 then (cR + rR, cG + rG, cB + rB) else c
        newFI = if r > 0 then (newC, r - 1, rel) else (newC, r, rel)
changeColour (f : fs) = (finalFIs, newC)
    where
        (c, r, rel) = f
        (cR, cG, cB) = c
        (rR, rG, rB) = rel
        newC = if r > 0 then (cR + rR, cG + rG, cB + rB) else c
        newFI = (newC, r - 1, rel)
        finalFIs = if r > 0 then newFI : fs else fs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- PROBABILITY
-- |Since we will be using probability, we will need some sort of rng,
-- for this, I'll be using System.Random, which is perfectly fulfills
-- the role we want here. No need for cryptographic quality randoms...

-- |Takes in a list of randoms between 0-1, a key and a set of rules, and finds
-- a corresponding definition using the probabilities
lookupCharProb :: ProbRules -> [Float] -> Char -> String
lookupCharProb probRulesData rands lookupKey = def
    where
        a = [(k, f, s) | (k, f, s) <- probRulesData, k == lookupKey]
        ratios = [prob | (_, prob, _) <- a]
        index = (length (takeWhile (>= 0) (scanl (-) rand ratios))) - 1
        (_, _, def) = a !! index
        (rand : _) = rands

-- |The probability version of expandOne
expandOneProb :: ProbRules -> [Float] -> String -> String
expandOneProb rulesSet rands command = concat expanded
    where
        expanded = map (lookupCharProb rulesSet rands) command

-- |The probability version of expand
expandProb :: ProbRules -> String -> Int -> [Float] -> String
expandProb rules base n rands = expandedList !! index
    where
        expandedList = take (n + 1) $ iterate (expandOneProb rules rands) base
        index = length expandedList - 1

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
    |willSpawn = trailSymbolBuilder multip clr (symbolsDataBase !! chosen) currTS
    |not willSpawn = []
        where
            willSpawn = rand1 < prob
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

-- |This function builds out the coloredline list from a (Int) size multiplier
-- an a given colour
trailSymbolBuilder :: Float -> Colour -> Path -> TurtleState -> [ColouredLine]
trailSymbolBuilder _ _ ([], (_, _)) _ = []
trailSymbolBuilder multiplier colour ((p : ps), cs) prevTS
    |cmd == 'F' = newCL : (trailSymbolBuilder multiplier colour (ps, cs) newTS)
    |otherwise = trailSymbolBuilder multiplier colour (ps, cs) newTS
        where
            newTS = moveAdvanced cmd prevTS multiplier dgr
            (cmd, dgr) = p
            (oldPos, _) = prevTS
            (newPos, _) = newTS
            newCL = (oldPos, newPos, colour)

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
lSystemProb :: ProbSystem -> Int -> String
lSystemProb (_, base, rs) n = expandOne mapper (expandProb rs base n randomList)
    where
        randomList = help

-- |Uses the first tracing method
drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour = drawLines (trace1 (lSystem system n) (angle system) colour)

-- |Uses the second tracing method
drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour = drawLines (trace2 (lSystem system n) (angle system) colour)

-- |This is the version with color fading, takes a list of Fade
drawLSystem2Fade :: System -> Int -> [Fade] -> IO ()
drawLSystem2Fade system n fade = drawLines (trace2Fade randomList (lSystem system n) (angle system) fade [] 0)
    where
        randomList = help

-- |This is the version with fading and symbols at the end aswell
-- a list of symbols [(Int, Float, Float)], where they are a certain symbols
-- index in symbolsDataBase, their probability to appear, and their size
-- And a final probability which is a general one, that's the probability
-- that a branch will be symbolized
drawLSystem2FadeSymbol :: System -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> IO ()
drawLSystem2FadeSymbol system n fade symbols prob = drawLines (trace2Fade randomList (lSystem system n) (angle system) fade symbols prob)
    where
        randomList = help

-- |This is the version with fading, symbols and a probabilistic system
drawLSystem2FadeSymbolProb :: ProbSystem -> Int -> [Fade] -> [(Int, Float, Float)] -> Float -> IO ()
drawLSystem2FadeSymbolProb system n fade symbols prob = drawLines (trace2Fade randomList (lSystemProb system n) (angleProb system) fade symbols prob)
    where
        randomList = help

-- |Random number generation
help :: [Float]
help 
    = do
        { let randGen = mkStdGen 34
        ; let rng = randomStuff randGen
        ; rng
        }

randomStuff :: RandomGen g => g -> [Float]
randomStuff g = (randomRs (0.0, 1.0) g)

-- || need to create a main function that will iterate through a bunch of
-- pre-defined fancy looking L-Systems once they press enter