main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
    -- where wordCount input = show (length (words input)) ++ "\n" ex03
    --where wordCount input = show (length input) ++ "\n" ex04 