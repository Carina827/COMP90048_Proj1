-- File        : Proj1.hs
-- Author      : Jing Du
-- Student Num : 775074
-- Email       : du2@student.unimelb.edu.au
-- Project     : Project 1 Submission, COMP90048
-- Purpose     : The Game of Musician  
                                                                                 
-- For a Musician game, one player is the composer and the other is the 
-- performer. The composer begins by selecting a three-pitch musical chord,
-- where each pitch comprises a musical note,one of A,B,C,D,E,F,orG,and an 
-- octave,one of 1,2,or3. This chord will be the target for the game. Once
-- the composer has selected the target chord, the performer chooses a 
-- similarly defined chord as a guess and tells it to the composer, who 
-- responds by giving the performer the feedback: 1. the number of correct
-- pitches 2. the number of correct notes 3. the number of correct octaves


module Proj1 (Pitch, toPitch, feedback,
                GameState, initialGuess, nextGuess) where


    -- Import libraries.
    import Data.Maybe
    import Data.List
    import Data.Function


    -- Define Pitch data structure.
    -- Defines Note and Octave as Char.
    data Pitch = Pitch Char Char deriving (Eq,Ord)


    -- Creat instance of Show Class. 
    -- Make sure the new Data Type Pitch can be shown in the correct form.
    instance Show Pitch where
        show (Pitch a b) = [a]++[b]


    -- GameState contains all the pitch lists that may be selected in the 
    -- current state
    type GameState = [[Pitch]]


    -- getNote function returns the note of a Pitch 
    getNote:: Pitch-> Char
    getNote (Pitch node _) = node


    -- getOctave function returns the octave of a Pitch
    getOctave:: Pitch-> Char
    getOctave (Pitch _ octave) = octave


    -- toPitch function turns a string into the Maybe Pitch data type
    -- Pitch Rules: Note:[A,B,C,D,E,F,G] Octave:[1,2,3].
    -- Note: When input is invalid, then the function will return Nothing.
    toPitch:: String-> Maybe Pitch
    toPitch (a) 
        | checkValid = Just (Pitch x y)
        | otherwise = Nothing
        where 
            x = head a
            y = last a
            checkValid = (x `elem` ['A'..'Z']) && (y `elem` ['1'..'3']) 


    -- Function takes a Maybe Pitch type input to a Pitch type data.
    -- Note: When show the Maybe Pitch type, it contains a "Just"
    -- which is different Pitch.
    change :: Maybe Pitch -> Pitch
    change Nothing =  error "error Pitch"
    change (Just pitch) = pitch
  

    -- feedback funtion compute the number of mitch pitchs. First argument 
    -- is the target, second is the guess.
    feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
    feedback target guess = (pitch, note, octave)
        where   
            pitch = length (intersect target guess)
            dnote = deleteFirstsBy (\p1 -> \p2 -> 
                    (getNote p1) == (getNote p2)) guess target
            note = 3 - pitch - length dnote
            doctave = deleteFirstsBy (\p1 -> \p2 -> 
                    (getOctave p1) == (getOctave p2)) guess target
            octave = 3 - pitch - length doctave


    -- initialGuess function does not accept input any parameters.
    -- It returns a pair of set initial guesses by iteratively calculating 
    -- all possible guesses. After that selects the initial guess [A1,B1,C1]
    -- and remove the initial guess from all possible guesses.
    initialGuess :: ([Pitch], GameState)
    initialGuess = 
        let pitchs = [[a]++[b] | a <- ['A'..'G'], b <- ['1'..'3']]
            mpitchs = map toPitch pitchs
            rpitchs = map change mpitchs
            res = [pitch | pitch <- subsequences rpitchs, length pitch == 3]
            mguess = map toPitch ["A1", "B1", "C1"]
            guess = map change mguess
            state = delete guess res
        in (guess,state)


    -- rmdups function remove the duplicates from the list
    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs)   | x `elem` xs   = rmdups xs
                    | otherwise     = x : rmdups xs
    

    -- getnotes function returns all the notes of the Pitchs in a Pitch list
    getnotes :: [Pitch] -> [Char]
    getnotes [] = []
    getnotes (x:xs) = rmdups (getNote x : getnotes xs)


    -- getoctaves function returns all the octaves of the Pitchs in 
    -- a Pitch list
    getoctaves:: [Pitch] -> [Char]
    getoctaves [] = []
    getoctaves (x:xs) = rmdups (getOctave x : getoctaves xs)


    -- getFeedback function need to input currentguess, current gamestate 
    -- and current feedback return the possible guesses that calculate the
    -- feedback with currentguess which result is same as current feedback 
    getFeedback :: ([Pitch], GameState)->(Int,Int,Int)-> GameState
    getFeedback (currentguess,[]) (p,n,o) = []
    getFeedback (currentguess,gamestate) (p,n,o) =
        if (p1,n1,o1) /= (p,n,o)
        then getFeedback (currentguess, tailGst) (p,n,o)
        else headGst : (getFeedback (currentguess, tailGst) (p,n,o))
          where 
            headGst    = head gamestate
            tailGst    = tail gamestate
            (p1,n1,o1) = feedback currentguess headGst


    -- nextGuess function takes as input a pair of the previous guess, 
    -- game state, and the feedback to this guess as a triple of correct
    -- pitches, notes, and octaves, and returns a pair of the next guess 
    -- and game state.
    nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
    nextGuess (guess, state) (correct_P, correct_N, correct_O) = 
      let newState = getFeedback (guess,state) (correct_P,correct_N,correct_O)
          selectedGuess = selectGuess newState
      in (selectedGuess,newState)


    -- selectGuess function implements making good choices from all 
    -- possible guesses
    selectGuess :: GameState -> [Pitch]
    selectGuess pitches = snd $ minimum $ 
                          [(getFreq pitch pitches, pitch)| pitch <- pitches]


    -- getFreq function returns the expected number for each next guess'. 
    -- Group the collection by feedbacks, and apply the equation to calculate
    -- the expected number.
    getFreq :: [Pitch] -> GameState -> Float
    getFreq pitch pitches = 
        sum ([fromIntegral (length x) * fromIntegral (length x)| 
            x <- gFeedback]) / fromIntegral (length pitches)
        where
            gFeedback = group (getFeedbacks pitch pitches)
    

    -- getFeedbacks function calculate the feedback of each possible guess
    -- with given pitch list and returns the feedback list
    getFeedbacks :: [Pitch] -> GameState -> [(Int,Int,Int)]
    getFeedbacks targetguess [] = []
    getFeedbacks targetguess gamestate = 
                 rsp : getFeedbacks targetguess (tail gamestate)
        where 
           rsp = feedback targetguess (head gamestate)

