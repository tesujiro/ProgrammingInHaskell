
main = do
    -- pure id <*> x   = x
    print $ pure id <*> [1,2,3]

    -- pure (g x)      = pure g <*> pure x
    print $ pure (head [(+1),(+2),(+3)])        <*> [1,2,3]
    print $ pure head <*> pure [(+1),(+2),(+3)] <*> [1,2,3]

    -- x <*> pure y    = pure (\g -> g y) <*> x
    print $ (pure head) <*> pure [(+1),(+2),(+3)]           <*> [1,2,3]
    print $ pure (\g -> g [(+1),(+2),(+3)]) <*> (pure head) <*> [1,2,3]

    -- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
    print $ pure (+100) <*> (pure (+1) <*> [1,2,3])
    print $ (pure (.) <*> pure (+100) <*> pure (+1)) <*> [1,2,3]
