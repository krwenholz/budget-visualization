module DataStructureHelp exposing (..)

import Array exposing (..)
import List exposing (..)

removeFromList i xs =
  (List.take i xs) ++ (List.drop (i+1) xs)

removeFromArray i =
  Array.toList >> removeFromList i >> Array.fromList
