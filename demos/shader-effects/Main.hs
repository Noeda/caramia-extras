{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}

module Main ( main ) where

import Caramia.Prelude
import Caramia.Events
import Caramia.SDL2Context
import Caramia
import Caramia.Extras.ShaderEffects
import qualified Data.Text.IO as T
import System.Environment
import System.Exit

main :: IO ()
main = do
    lst <- getArgs
    when (length lst < 1) $ do
        putStrLn "Usage:"
        putStrLn "shader-effects [FILENAME] [WIDTH] [HEIGHT]"
        putStrLn ""
        putStrLn "Width and height are optional."
        exitSuccess

    let (w, h) = if length lst >= 3
                    then (read $ lst !! 1, read $ lst !! 2)
                    else (800, 600)
    runSDL2Context simpleContext { sizing = Windowed w h } $ do
        f <- head <$> getArgs
        v <- T.readFile f
        (w, h) <- getDimensions screenFramebuffer
        sh <- newShaderEffects v w h
        forever $ do
            stepShaderEffects sh
            ev <- getEvents
            for_ ev $ \case
                QuitEvent {} -> exitSuccess
                _ -> return ()
            swapBuffers


