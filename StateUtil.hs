module StateUtil where

import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT

--instance HasSetter IORef where
-- ($=) var val = writeIORef var val

 --instance HasGetter IORef where
 -- get var = readIORef var

new = newIORef
