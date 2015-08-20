import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import Graphics.Histogram 

input = take (3600*4) (cycle [1..400]) ++ take  (3600*4) (cycle [1..40]) ++ take  (3600*4) (cycle [200..500])
input2 = take 100 [1..]
 
simple = do
    let hist = histogram binSturges input2
    print (binSturges input)
    plot "simple2.png" hist

advanced = do
    let hist = histogram binSqrt input
    let opts = Opts.title "I'm a histogram!" $ 
               Opts.yLabel "Why?" $ 
               Opts.xLabel "Because!" $ 
               defOpts hist
    plotAdv "advanced.eps" opts hist
