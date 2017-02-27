module DataStructureHelp exposing (removeFromList, removeFromArray)

import Array
import List


removeFromList i xs =
    (List.take i xs) ++ (List.drop (i + 1) xs)


removeFromArray i =
    Array.toList >> removeFromList i >> Array.fromList
