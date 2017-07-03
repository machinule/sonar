-- Definitions pertaining to the sea grid.

type Elevation = Double

type Row = Int

type Column = Int

type ElevationGrid = Array (Row, Column) Elevation

data Islandness = Island | Sea deriving (Show)

prettyIslandness :: Islandness -> String
prettyIslandness Island = "¦"
prettyIslandness Sea = "~"

type SeaGrid = Array (Row, Column) Islandness

prettyMap :: SeaGrid -> String
prettyMap grid = intercalate
  "\n"
  (map
    (\r -> intercalate "" (map (\(_, v) -> prettyIslandness v) r))
    (groupBy ((==) `on` (\((r,_), _) -> r)) (assocs grid) ))

flood :: Elevation -> Elevation -> Islandness
flood terrain water = if water > terrain then Sea else Island 

mkGrid :: (Int, Int) -> ((Int, Int) -> a) -> Array (Int, Int) a
mkGrid (rows,cols) f = array ((0,0),(rows-1,cols-1)) [(i,f i) | r <- [0..rows-1], c <- [0..cols-1], let i = (r,c)]

maxIslandGrid :: (Int, Int) -> SeaGrid
maxIslandGrid bounds = mkGrid bounds (\(r,c) -> if even $ r + c then Island else Sea) 
