module Main

palindrome: Nat -> String -> Bool
palindrome n str = let lowerS = toLower str in
            if length lowerS > n then reverse lowerS == lowerS else False

counts: String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten: Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

over_length: Nat -> List String -> Nat
over_length limit xs = length (filter (\x => length x > limit) xs)

main: IO ()
main = repl "Enter a string: " (\x => show (palindrome 10 x) ++ "\n")
