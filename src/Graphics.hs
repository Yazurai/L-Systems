module Graphics
( Colour
, black
, blue
, green
, cyan
, red
, magenta
, yellow
, white
, pink
, purple
, brown
, orange
, brown2
, drawLines
) where

import Control.Monad
import Data.Char
import Data.List
import Graphics.Rendering.OpenGL hiding (Vertex)
import Graphics.UI.GLUT hiding (Vertex)

type Colour = (Float, Float, Float)

black, blue, green, cyan, red, magenta, yellow, white, pink, purple, brown, orange, brown2 :: Colour

black = (0, 0, 0)
blue = (0, 0, 1.0)
green = (0, 1.0, 0)
cyan = (0, 1.0, 1.0)
red = (1.0, 0, 0)
magenta = (1.0, 0, 1.0)
yellow = (1.0, 1.0, 0)
white = (1.0, 1.0, 1.0)
pink = (1, 0.505, 0.752)
purple = (0.494, 0.117, 0.611)
brown = (0.301, 0.164, 0)
orange = (0.976, 0.450, 0.023)
brown2 = (0.678, 0.368, 0)

type Vertex = (Float, Float)

type ColouredLine = (Vertex, Vertex, Colour)

drawLines :: [ColouredLine] -> IO ()
drawLines ls 
    = do
        () <- ls `seq` return ()
        _ <- getArgsAndInitialize
        w <- createWindow "LSystems"
        
        actionOnWindowClose $= ContinueExecution
        displayCallback $= display ls
        keyboardMouseCallback $= Just keyboardMouse
        reshapeCallback $= Just reshape

        mainLoop

display :: [ColouredLine] -> IO ()
display ls 
    = do
        clear [ColorBuffer]

        let verticesOf (from, to, _) = [from, to]
            vertices = concatMap verticesOf ls
            ((minX, minY), (maxX, maxY)) = computeScale vertices
        
        preservingMatrix $ do
            ortho (realToFrac minX) (realToFrac maxX)
                (realToFrac minY) (realToFrac maxY)
                (0) (1)
            renderPrimitive Lines $ mapM_ lineVertices ls
        
        flush

lineVertices :: ColouredLine -> IO ()
lineVertices ((fromX, fromY), (toX, toY), (r, g, b)) 
    = do
        color $ Color3 (realToFrac r :: GLfloat) (realToFrac g) (realToFrac b)
        vertex $ Vertex3 (realToFrac fromX :: GLfloat) (realToFrac fromY) 0
        vertex $ Vertex3 (realToFrac toX :: GLfloat) (realToFrac toY) 0

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse (Char '\ESC') Down _ _ = leaveMainLoop
keyboardMouse (MouseButton _) Down _ p = print p
keyboardMouse _ _ _ _ = return ()

reshape :: Size -> IO ()
reshape s
    = do
        viewport $= (Position 0 0, s)
        postRedisplay Nothing

computeScale :: [Vertex] -> (Vertex, Vertex)
computeScale vs = foldl' f ((infinity, infinity), (-infinity, -infinity)) vs
    where
        f ((minX, minY), (maxX, maxY)) (x, y) = ((min minX x, min minY y), (max maxX x, max maxY y))
        infinity = 10000000