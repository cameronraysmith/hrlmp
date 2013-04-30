import Criterion.Main

import Corec.COR

main = defaultMain corecBenchmarks 

corecBenchmarks  = 
  [
    bgroup "theFibs head normal form" [ 
        bench "10"   $ nf (\n -> take n theFibs) 10
      , bench "100"  $ nf (\n -> take n theFibs) 100
      , bench "1000" $ nf (\n -> take n theFibs) 1000
    ],
    bgroup "theFibs weak head normal form" [ 
        bench "10"   $ whnf (\n -> take n theFibs) 10
      , bench "100"  $ whnf (\n -> take n theFibs) 100
      , bench "1000" $ whnf (\n -> take n theFibs) 1000
    ]
  ]
