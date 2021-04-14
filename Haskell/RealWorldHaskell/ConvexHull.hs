import Data.List

type Point = (Double,Double)

orderBottomUp :: [Point] -> [Point]
orderBottomUp ps = sortBy (\(x,y) (x',y') -> compare y y') ps

orderLeftToRight :: [Point] -> [Point]
orderLeftToRight = sortBy (\(x,_) (x',_) -> compare x x')

leftMostBottomMost :: [Point] -> Point
leftMostBottomMost ps = 
    head $ orderLeftToRight bottomPs
    where 
        bottomPs = takeWhile (\(_,y) -> y == lowestY) bottomToTop
        (_,lowestY) = head $ bottomToTop
        bottomToTop = orderBottomUp ps

orderFromOrigin :: Point -> [Point] -> [Point]
orderFromOrigin (xo,yo) = sortBy (\p p' -> compare (angleFromOrigin p) (angleFromOrigin p'))
                          where
                              angleFromOrigin (x,y) = atan2 (y-yo) (x-xo)
        

