--!!! Testing IOError (part 2)

-- These should both raise the same error - not IOErrors!
a1 = ["a" !! 1]
a2 = writeFile "foo" (["a"] !! 1)
