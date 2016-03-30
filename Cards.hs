module Cards ( Suit(..),
               Pip(..),
               Face(..),
               Card(..),
               Cards,
               suits,
               faceCards,
               pipCards,
               cardDeck,
               shuffle) where

import Control.Applicative
import Control.Exception
import System.Random
import Data.List

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Face = Jack | Queen | King
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Card = PipCard Suit Pip
          | FaceCard Suit Face
            deriving (Eq)

instance Show Card where
    show (PipCard suit pip) = (show pip) ++ " of " ++ show(suit)
    show (FaceCard suit face) = (show face) ++ " of " ++ show(suit)

type Cards = [Card]

suits = [(minBound :: Suit) ..]

faceCards = FaceCard <$> suits <*> [(minBound :: Face) ..]

pipCards = PipCard <$> suits <*> [(minBound :: Pip) ..]

cardDeck = faceCards ++ pipCards

shuffle :: (Eq a) => StdGen -> [a] -> [a]
shuffle _ [element] = [element]
shuffle gen elements =
        let (r, newGen) = random gen :: (Int, StdGen)
            element        = elements !! (mod r $ length elements)
            elements'      = delete element elements 
        in element : shuffle newGen elements'
