module HierarchyModules.Cube
( volume
, area) where

volume :: Float -> Float
volume a = a ^ 3

area :: Float -> Float
area a = 6 * (a ^ 2)
