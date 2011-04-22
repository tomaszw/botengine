module Waypoints where

import Common
import Math
import Data.Maybe

import System.Directory
import System.FilePath

data Waypoint = Waypoint { waypoint_name :: String
                         , waypoint_p :: Vec3 }
                deriving (Eq, Show)

readWaypoints :: IO [Waypoint]
readWaypoints =
    do home <- getHomeDirectory
       let file = home </> ".bot" </> "waypoints"
       contents <- readFile file
       return $ parseContents contents

saveWaypoints :: [Waypoint] -> IO ()
saveWaypoints ps =
    do home <- getHomeDirectory
       let file = home </> ".bot" </> "waypoints"
       writeFile file $ toString ps

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                ((v,_) : _) -> Just v
                _ -> Nothing
 
parseContents :: String -> [Waypoint]
parseContents = catMaybes . map parseLine . lines where
    parseLine l =
        case words l of
          [name, x, y, z] ->
              do p' <- p x y z
                 return $ Waypoint name p'
          _ ->
              Nothing
    p x y z = do x' <- maybeRead x
                 y' <- maybeRead y
                 z' <- maybeRead z
                 return $ Vec3 x' y' z'

toString :: [Waypoint] -> String
toString = unlines . map render where
    render (Waypoint name (Vec3 x y z)) = name ++ " " ++ show x ++ " " ++ show y ++ " " ++ show z
