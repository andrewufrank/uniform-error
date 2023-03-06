0.0.10
    move from the 0.0.9 versions in Workspace8 2019
    change to uniform approach:
        test/Testing.hs
        .ghci 
        cleaned 
0.0.10.1 added (moved) startApp from 
0.1.0   cleaned and removed unnecessary comments and functions

0.1.2 moved to Control.Monad.Exception
0.1.3 some improvements, pushed on hackage
        added stack build for 19.16 (9.0.2)

0.1.3.1 maintenance to fix #1 -- checked with ghc 8.10.7
        import HTF (Test.Framework.HUnitWrapper) unqualified to get the asserts (export in HTF 0.15.0.0 changed see the change log)
0.1.3.2 moved the NoticeLevel to a module in uniform-error
0.1.5.1 for ghc 9.2.5