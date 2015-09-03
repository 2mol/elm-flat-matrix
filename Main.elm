module Main where

import Text
import Graphics.Element exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.Element as ElementRunner
import ElmTest.Runner.String  as StringRunner
import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import Array
import Matrix


fromList : Test
fromList = suite "From list"
  [ test "equal size" 
      <| assertEqual (2, 2) 
      <| case Matrix.fromList [[1, 1], [1, 1]] of Just v -> v.size,
    test "inequal size" 
      <| assertEqual (3, 2) 
      <| case Matrix.fromList [[1, 1], [1, 1], [3, 3]] of Just v -> v.size,
    test "inequal size" 
      <| assertEqual (2, 3) 
      <| case Matrix.fromList [[1, 1, 1], [1, 1, 1]] of Just v -> v.size,
    test "Non-consistent size" 
      <| assertEqual False 
      <| case Matrix.fromList [[1, 1, 1], [1, 1, 1, 5]] of 
        Just v -> True
        Nothing -> False
  ]

get : Test 
get = suite "Get"
  [ test "get first" 
      <| assertEqual (Just 1) 
      <| Matrix.get 0 0 <| Matrix.repeat 1 1 1,
    test "get invalid range" 
      <| assertEqual (Nothing) 
      <| Matrix.get 1 2 <| Matrix.repeat 1 1 1
  ]

set : Test
set = suite "Set"
  [ test "Set first" 
      <| assertEqual (Just 5) 
      <| Matrix.get 0 0 <| Matrix.set 0 0 5 <| Matrix.repeat 2 2 1,
    test "Set outside of range does nothing"
      <| (\x -> assertEqual (1, 1) x.size)
      <| Matrix.set 5 5 1 <| Matrix.repeat 1 1 1
  ]

update : Test
update = suite "Update"
  [ test "Update first element" 
      <| assertEqual (Just 5) 
      <| Matrix.get 1 1 <| Matrix.update 1 1 (\x -> 5) <| Matrix.repeat 2 2 1
  ]

map : Test
map = suite "Map"
  [ test "increment every value" 
      <| assertEqual (Matrix.repeat 2 2 2) 
      <|  Matrix.map (\x -> x + 1) <| Matrix.repeat 2 2 1,
    test "identity" 
      <| assertEqual (Matrix.repeat 2 2 1) 
      <|  Matrix.map identity <| Matrix.repeat 2 2 1
  ]

indexedMap : Test
indexedMap = suite "IndexedMap"
  [ test "basic index map" 
      <| assertEqual (Matrix.repeat 1 1 0) 
      <|  Matrix.indexedMap (\x y _ -> x + y) <| Matrix.repeat 1 1 1,
    test "(x,y) -> x + y" 
      <| assertEqual (case Matrix.fromList [[0, 1, 1], [2, 2, 3]] of Just v -> v) 
      <|  Matrix.indexedMap (\x y _ -> x + y) <| Matrix.repeat 2 3 1
  ]

filter : Test
filter = suite "Filter"
  [ test "Keep ones" 
      <| assertEqual (Array.repeat 2 1) 
      <|  Matrix.filter (\x -> x == 1) <| case Matrix.fromList [[2, 3], [1, 1]] of Just v -> v
  ]


tests : Test
tests = suite "Tests"
  [ get,
    set,
    update,

    fromList, 
    
    map,
    filter,
    indexedMap
  ]

results : String
results = StringRunner.runDisplay tests

main : Element
main = ElementRunner.runDisplay tests