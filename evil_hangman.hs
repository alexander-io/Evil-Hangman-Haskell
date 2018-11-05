import System.Exit (exitSuccess)

filter_list_items_to_length item_length lst = filter (\e->(item_length==(length e))) lst

-- remove just wrapper from value
eliminate (Just a) = a

-- convert string to character
s_to_c [] = Nothing
s_to_c [string] = Just string
s_to_c s = undefined

-- take a 'match' that also functions as 'guess', also take to work over
-- map each item in lst -> e, map each item in  e -> character, iterate over match in parallel;
-- if each character is found in the match string, keep it in the output, else replace it with 'e'
-- ex ::: input -> "a" ["alex", "bark", "cream"], output -> ["a---", "-a--", "---a-"]
build_list_of_patterns match lst = map (\e-> map (\el-> if el `elem` match then el else '-') e) lst
build_list_of_pattern_tuples match lst = map (\e-> (e, map (\el-> if el `elem` match then el else '-') e)) lst

-- take a list of strings, concat to makea a string
build_string [] = []
build_string (x:xs) = (eliminate (s_to_c x)) : build_string xs

-- remove duplicate items in list, useful for filtering  the list of patterns
remove_duplicates [] = []
remove_duplicates (x:xs) = x : filter (/= x) (remove_duplicates xs)

-- it's useful to structure (wnord, pattern tuples); define functions to extract either word or pattern
get_pattern (word, pattern) = pattern
get_word (word, pattern) = word

-- make a list of families
-- a family has the following structure (pattern, [list of words matching pattern])
-- to accomplish this, take in a unique list of patterns and a list of (word, pattern) tuples
make_families [] _ = []
make_families (unique_pattern:unique_list_of_patterns) list_of_tuples = (unique_pattern, family) : (make_families unique_list_of_patterns list_of_tuples)
    where
      family = filter (\e->e/="xxx") (map (\tuple -> if (get_pattern tuple) == unique_pattern then (get_word tuple) else "xxx") list_of_tuples)

-- family tuple structure : (pattern, [list of matching words]), define a function to extract the list of matching words from the tuple struct
get_family_list_from_family_tuple (family_pattern, family_of_words_list) = family_of_words_list
get_family_pattern_from_tuple (family_pattern, family_of_words_list) = family_pattern

-- recursive funx to determine the largest family  given a list of family tuple groups
determine_largest_family [] curr_largest = curr_largest
determine_largest_family (family_tuple:list_of_families) curr_largest = if (length (get_family_list_from_family_tuple family_tuple)) > (length (get_family_list_from_family_tuple curr_largest)) then (determine_largest_family list_of_families family_tuple) else (determine_largest_family list_of_families curr_largest)

-- print groups, also format
print_groups [] = putStrLn "\n"
print_groups (x:family_groups) = do
  putStrLn ("\t" ++ show (get_family_pattern_from_tuple x) ++ " matches " ++ (show (get_family_list_from_family_tuple x) ))
  print_groups family_groups

-- handle case for winning the game
winner = do
  putStrLn "you won the game"
  exitSuccess

-- recursive function, taking in an updated dictionary and guess
evil_recurse dict_param guess = do
    -- UNIQUE list of patterns matching guess ::: (remove_duplicates (map build_string (build_list_of_patterns guess param)))
    let unique_list_of_patterns = (remove_duplicates (build_list_of_patterns guess dict_param))
    -- patch words with their pattern, ex : ("olive", "o----"); create a list
    let list_of_word_pattern_tuples = (build_list_of_pattern_tuples guess dict_param)
    -- create family groups  of  words, format is (pattern, [list of words matching pattern])
    let family_groups = make_families unique_list_of_patterns list_of_word_pattern_tuples

    -- print family  groups
    print_groups family_groups

    -- determine the next pattern, according the the largest family
    let next_pattern = (get_family_pattern_from_tuple (determine_largest_family family_groups (head family_groups)))

    putStrLn ("using pattern : " ++ (show  next_pattern) ++ " which matches " ++ (show (length (get_family_list_from_family_tuple (determine_largest_family family_groups (head family_groups))))) ++ " word(s)")

    -- did player win?
    if (length (get_family_list_from_family_tuple (determine_largest_family family_groups (head family_groups)))) == 1 then winner else (putStrLn "...")

    -- if no win, continue
    putStrLn ("Letters guessed: " ++ (show guess))

    putStrLn "Guess a letter:"
    character_guess <- getChar

    evil_recurse (get_family_list_from_family_tuple (determine_largest_family family_groups (head family_groups))) (guess ++ [character_guess])
    return "unreachable"

main = do
    putStrLn "Word length?"
    word_length <- getLine
    let word_length_int = read word_length :: Int

    -- filter  the dictionary, store in length_filtered_dict
    let length_filtered_dict = filter_list_items_to_length word_length_int dict

    putStrLn ("Word: " ++ (show  (replicate word_length_int '-')))

    putStrLn "Guess a letter:"
    character_guess <- getChar
    putStrLn "\n"

    evil_recurse length_filtered_dict [character_guess]

dict = ["abbe", "abed", "abet", "able", "abye", "aced", "aces", "ache", "acme", "acne", "acre", "adze", "aeon", "aero", "aery", "aged", "agee", "ager", "ages", "ague", "ahem", "aide", "ajee", "akee", "alae", "alec", "alee", "alef", "ales", "alme", "aloe", "amen", "amie", "anes", "anew", "ante", "aped", "aper", "apes", "apex", "apse", "area", "ares", "arse", "asea", "ates", "aver", "aves", "awed", "awee", "awes", "axed", "axel", "axes", "axle", "ayes", "babe", "bade", "bake", "bale", "bane", "bare", "base", "bate", "bead", "beak", "beam", "bean", "bear", "beat", "beau", "bema", "beta", "blae", "brae", "cade", "cafe", "cage", "cake", "came", "cane", "cape", "care", "case", "cate", "cave", "ceca", "dace", "dale", "dame", "dare", "date", "daze", "dead", "deaf", "deal", "dean", "dear", "deva", "each", "earl", "earn", "ears", "ease", "east", "easy", "eath", "eats", "eaux", "eave", "egad", "egal", "elan", "epha", "eras", "etas", "etna", "exam", "eyas", "eyra", "face", "fade", "fake", "fame", "fane", "fare", "fate", "faze", "feal", "fear", "feat", "feta", "flea", "frae", "gaed", "gaen", "gaes", "gage", "gale", "game", "gane", "gape", "gate", "gave", "gaze", "gear", "geta", "hade", "haed", "haem", "haen", "haes", "haet", "hake", "hale", "hame", "hare", "hate", "have", "haze", "head", "heal", "heap", "hear", "heat", "idea", "ilea", "jade", "jake", "jane", "jape", "jean", "kaes", "kale", "kame", "kane", "keas", "lace", "lade", "lake", "lame", "lane", "lase", "late", "lave", "laze", "lead", "leaf", "leak", "leal", "lean", "leap", "lear", "leas", "leva", "mabe", "mace", "made", "maes", "mage", "make", "male", "mane", "mare", "mate", "maze", "mead", "meal", "mean", "meat", "mesa", "meta", "nabe", "name", "nape", "nave", "neap", "near", "neat", "nema", "odea", "olea", "pace", "page", "pale", "pane", "pare", "pase", "pate", "pave", "peag", "peak", "peal", "pean", "pear", "peas", "peat", "plea", "race", "rage", "rake", "rale", "rape", "rare", "rase", "rate", "rave", "raze", "read", "real", "ream", "reap", "rear", "rhea", "sabe", "sade", "safe", "sage", "sake", "sale", "same", "sane", "sate", "save", "seal", "seam", "sear", "seas", "seat", "sera", "seta", "shea", "spae", "tace", "tael", "take", "tale", "tame", "tape", "tare", "tate", "teak", "teal", "team", "tear", "teas", "teat", "tela", "tepa", "thae", "toea", "twae", "urea", "uvea", "vale", "vane", "vase", "veal", "vela", "vena", "vera", "wade", "waes", "wage", "wake", "wale", "wame", "wane", "ware", "wave", "weak", "weal", "wean", "wear", "weka", "yare", "yeah", "yean", "year", "yeas", "zeal", "zeta", "zoea"]
