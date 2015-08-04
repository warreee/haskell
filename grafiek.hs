import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import Graphics.Histogram 

input = take (3600*4) (cycle [1..400]) ++ take  (3600*4) (cycle [1..40]) ++ take  (3600*4) (cycle [200..500])
 
simple = do
    let hist = histogram binSturges input
    plot "simple.png" hist

advanced = do
    let hist = histogram binSqrt input
    let opts = Opts.title "I'm a histogram!" $ 
               Opts.yLabel "Why?" $ 
               Opts.xLabel "Because!" $ 
               defOpts hist
    plotAdv "advanced.eps" opts hist
