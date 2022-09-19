import Data.Char ( toLower )
import Data.List ( nub )

valid_chars :: [Char]
valid_chars = "abcdefghijklmnopqrstuvwxyz"

-- check to see if the val is contained in the list
in_list :: Eq a => a -> [a] -> Bool
in_list c [] = False
in_list c (x:[]) = c == x
in_list c (x:xs)
    | c == x    = True
    | otherwise = in_list c xs

-- assuming a list without repeats, remove the given item from it
remove_item :: Eq a => a -> [a] -> [a]
remove_item c xs = remove_item_helper c xs []
    where
        remove_item_helper :: Eq a => a -> [a] -> [a] -> [a]
        remove_item_helper c [] buffer = buffer
        remove_item_helper c (x:[]) buffer
            | c == x    = buffer
            | otherwise = buffer ++ [x]
        remove_item_helper c (x:xs) buffer
            | c == x    = buffer ++ xs
            | otherwise = remove_item_helper c xs (buffer ++ [x])

-- extract the first char of the string if it exists
extract_char :: String -> Maybe Char
extract_char [] = Nothing
extract_char (x:[]) = Just x
extract_char (x:xs) = Just x

-- generates the string that shows how much of the word has been revealed, accounts for valid chars
word_reveal :: String -> [Char] -> String
word_reveal word left = word_reveal_helper word left []
    where
        word_reveal_helper :: String -> [Char] -> [Char] -> [Char]
        word_reveal_helper [] left buffer = buffer
        word_reveal_helper (x:xs) left buffer
            | not (in_list x valid_chars) = word_reveal_helper xs left (buffer ++ [x])--char is not a letter, so display it
            | in_list x left    = word_reveal_helper xs left (buffer ++ "_ ") --letter has not been guessed yet
            | otherwise         = word_reveal_helper xs left (buffer ++ (x : " ")) --letter has been guessed

-- formats the incorrect chars in a readable format
incorrect_chars :: [Char] -> String
incorrect_chars [] = ""
incorrect_chars (x:[]) = [x] --only one item left
incorrect_chars (x:y:xs) = [x] ++ ", " ++ incorrect_chars (y:xs) --at least two items left

-- returns the print sequence given the game information
print_sequence :: String -> Integer -> [Char] -> [Char] -> String
print_sequence word lives incorrect left = "Lives: " ++ show lives ++ "\n" ++ word_reveal word left ++ 
    "\nIncorrect guesses: " ++ incorrect_chars incorrect

-- print the correct message for the last action
message :: Integer -> IO()
message action
    | action == -2  = putStrLn "You're dumb enough to forget inputting that letter already"
    | action == -1  = putStrLn "Bruh, that's not a letter"
    | action == 1   = putStrLn "Incorrect character, you suck balls"
    | action == 2   = putStrLn "Well done, degen"
    | otherwise     = putStrLn ""

-- print out the final things before ending the game, then ask to play again
exit_sequence :: String -> Integer -> [Char] -> Integer -> IO()
exit_sequence word lives incorrect result = do
    putStrLn "\ESC[2J"
    if result == 0
        then putStrLn "mmm alright, you win"
        else putStrLn "ha you suck, trash"
    putStrLn (print_sequence word lives incorrect [])
    putStrLn "Would you wish to play again? y/n"
    input <- getLine
    exit_loop input --after receiving response, give control to the exit_loop
        where
            exit_loop :: String -> IO()
            exit_loop response = do
                putStrLn "\ESC[2J"
                let c = extract_char response
                case c of
                    Nothing -> do --if user gave a trash response
                        putStrLn "Would you wish to play again? y/n"
                        input <- getLine
                        exit_loop input
                    Just c -> do --if user response contains one char
                        case (toLower c) of
                            'y' -> game_initializer --play again
                            'n' -> return () --don't play again
                            otherwise -> do --user gave trash response
                                putStrLn "Would you wish to play again? y/n"
                                input <- getLine
                                exit_loop input


-- place where we interact with the IO
game_loop :: String -> Integer -> [Char] -> [Char] -> [Char] -> Integer -> IO()
game_loop word lives correct incorrect left action = do
    putStrLn "\ESC[2J"
    message action
    putStrLn (print_sequence word lives incorrect left ++ "\nInput your guess: ")
    input <- getLine
    let guess = extract_char input --we receive a Maybe Char
    case guess of
        Nothing -> do --if guess was Nothing, then loop again
            game_loop word lives correct incorrect left (-1)
        Just c_up -> do --otherwise check guess for validity
            let c = toLower c_up
            let valid = in_list c valid_chars
            case valid of
                False -> do --is the guessed character a letter? if not then loop again
                    game_loop word lives correct incorrect left (-1)
                True -> do --otherwise check for repeated guess
                    let repeated = in_list c correct || in_list c incorrect
                    case repeated of
                        True -> do --we've already guessed this letter
                            game_loop word lives correct incorrect left (-2)
                        False -> do --newly guessed character
                            let in_word = in_list c word
                            case in_word of
                                True -> do --guess was correct
                                    let new_correct = c : correct
                                    let new_left = remove_item c left
                                    if length new_left == 0 
                                        then exit_sequence word lives incorrect 0
                                        else game_loop word lives new_correct incorrect new_left 2
                                False -> do --guess was incorrect
                                    let new_incorrect = c : incorrect
                                    let new_lives = lives - 1
                                    if new_lives == 0 
                                        then exit_sequence word new_lives new_incorrect 1
                                        else game_loop word new_lives correct new_incorrect left 1

-- receive the word to be guessed and begin the game loop
game_initializer :: IO()
game_initializer = do
    putStrLn "Input your word: "
    input <- getLine
    let low_input = map toLower input --make input word lowercase
    let pruned = nub low_input --remove repeats from word, for use as a char tracker
    let left = filter (\x -> in_list x valid_chars) pruned --ensure we only track valid characters
    game_loop low_input 6 [] [] left 0 --initiate a new game loop

-- entry point
main :: IO()
main = do
    game_initializer