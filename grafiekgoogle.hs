import Graphics.Google.Chart

main = 
    do putStrLn "URL for your chart:"
       putStrLn $ chartURL $ setSize 1000 100 $ setTitle "My Chart" $ setData (encodeDataSimple [[5,10..2000]]) $ setLegend ["1 to 20"] $ newLineChart
