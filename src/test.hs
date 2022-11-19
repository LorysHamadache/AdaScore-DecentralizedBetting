

module Test where

data State = Win | Loss
data StatePlus = StatePlus State | NoState

s1::State 
s1 = Win

s2::StatePlus 
s2 = Win

