-- !!! Testing Trex
module TrexTest where

import Hugs.Trex

eg1 = (a = True, b = "Hello", c = 12::Int)
eg2 = (c = 12::Int, a = True, b = "Hello")
average r = (#x r + #y r) / 2

-- interaction with type synonyms

type AddA r = (a :: Int | r)
type AddB r = (b :: Int | r)
type ARow = AddA EmptyRow
type BRow = AddB EmptyRow
type BARow = AddB ARow
type ABRow = AddA BRow

x = (a=2) :: Rec ARow
y = ((b=3 | x) :: Rec ABRow) :: Rec BARow

t20 = y
t21 = y == (a=2 | (b=3))
t22 = (\(a=v|r) -> (v,r)) y
t23 = (\(a=v|r) -> (v,r)) x
