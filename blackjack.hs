import Cards
import Data.List
import System.Random

type CardDeck    = Cards
type PlayerCards = Cards
type DealerCards = Cards
type NumCards    = Int
type NumPlayers  = Int
type Score       = Int

main = do
    gen <- getStdGen
    game gen

game gen = do
    let shuffledDeck                                          = shuffle gen cardDeck
        ((playerCards:(dealerCards@(dealerCard:_)):[]), deck) = deal 2 2 shuffledDeck
    putStrLn $ "Dealer card: " ++ show dealerCard
    (playerCards', deck') <- playerGame playerCards deck
    let dealerCards' = dealerGame dealerCards deck'
        outcome      = gameOutcome playerCards' dealerCards'
    putStrLn $ "Dealers' cards: " ++ printCards dealerCards'
    putStrLn outcome

gameOutcome :: PlayerCards -> DealerCards -> String
gameOutcome playerCards dealerCards
    | not playerValidScore || playerScore < dealerScore = "You lose!"
    | standoff                                          = "Standoff!"
    | playerBlackjack                                   = "You've got Blackjack!"
    | playerScore > dealerScore                         = "You win!"
    where playerValidScore = isValidScore playerCards
          playerBlackjack  = isBlackjack playerCards
          dealerBlackjack  = isBlackjack dealerCards
          playerScore      = highestValidScore playerCards
          dealerScore      = highestValidScore dealerCards
          standoff         = (playerBlackjack && dealerBlackjack) ||
                                (not playerBlackjack && not dealerBlackjack &&
                                   playerScore == dealerScore && playerValidScore)

playerGame :: PlayerCards -> CardDeck -> IO (Cards, Cards)
playerGame playerCards deck = do
    putStrLn $ "Your cards: " ++ printCards playerCards
    if isBlackjack playerCards || is21 playerCards then
        return (playerCards, deck)
    else
        if isValidScore playerCards then do
            putStrLn "Hit(h) or Stand(s)?"
            choice <- getLine
            if choice == "Hit" || choice == "h" then
                let (card:[], deck') = deal 1 1 deck
                in playerGame (card ++ playerCards) deck'
            else
                return (playerCards, deck)
        else return (playerCards, deck)

printCards :: Cards -> String
printCards cards = intercalate ", " (map show cards)

dealerGame :: DealerCards -> CardDeck -> DealerCards
dealerGame x [] = error "Not enough cards!"
dealerGame dealerCards (card:cards) =
    let shouldStand = isDealerStand dealerCards
        validScore = isValidScore dealerCards
    in  if shouldStand || not validScore then dealerCards
        else dealerGame (card:dealerCards) cards

highestValidScore :: Cards -> Score
highestValidScore cards = maximum . filter (\x -> x <= 21) $ (0 : score cards)

isValidScore :: Cards -> Bool
isValidScore cards = (==) 1 $ length . find (\x -> x <= 21) $ score cards  

isBlackjack :: Cards -> Bool
isBlackjack cards = length cards == 2 && is21 cards

is21 :: Cards -> Bool
is21 cards = (==) 1 $ length . find (\x -> x == 21) $ score cards

isDealerStand :: DealerCards -> Bool
isDealerStand cards = (==) 1 $ length $ find (\x -> x >= 17) $ filter (\x -> x <= 21 ) $ score cards

score :: Cards -> [Score]
score cards = let cardScores = foldl (\acc card -> (cardScore card):acc) [] cards
              in nub $ sum <$> sequence cardScores

cardScore :: Card -> [Score]
cardScore (PipCard _ value) =
    case value of
        Ace -> [1, 11]
        x   -> [(fromEnum x) + 1]
cardScore (FaceCard _ value) = [10]

deal :: NumCards -> NumPlayers -> CardDeck -> ([PlayerCards], CardDeck)
deal numCards numPlayers cards = deal' numCards (map (\_ -> []) [1..numPlayers], cards)

deal' :: NumCards -> ([PlayerCards], CardDeck) -> ([PlayerCards], CardDeck)
deal' 0 (xxs, cards) = (xxs, cards)
deal' _ ([], cards) = ([], cards)
deal' numCards (xxs, cards) =
    if (length xxs > length cards)
        then error "Not enough cards!"
    else
        let
            (xs, cards') = splitAt (length xxs) cards
            xxs' = zipWith (\x xs -> x:xs) xs xxs
        in
            deal' (numCards-1) (xxs', cards')
