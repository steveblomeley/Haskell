import Data.List

type Point = (Double,Double)

data Direction = LeftTurn | RightTurn deriving Eq

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

sortFromOrigin :: Point -> [Point] -> [Point]
sortFromOrigin (xo,yo) = sortBy (\p p' -> compare (angleFromOrigin p) (angleFromOrigin p'))
                         where
                             angleFromOrigin (x,y) = atan2 (y-yo) (x-xo)

directionOfTurn :: Point -> Point -> Point -> Direction
directionOfTurn (x1,y1) (x2,y2) (x3,y3) = 
    if zCoordinateOfXProductOfVectors < 0
        then RightTurn
        else LeftTurn
    where
        zCoordinateOfXProductOfVectors = (x2 - x1) * (y3 -y1) - (y2 - y1) * (x3 - x1)               

sortPointsFromOrigin :: [Point] -> (Point,[Point])
sortPointsFromOrigin ps = (pOrigin, psSorted ++ [pOrigin])
                          where
                              pOrigin = leftMostBottomMost ps
                              remainingPs = filter (\p -> p /= pOrigin) ps
                              psSorted = sortFromOrigin pOrigin remainingPs

convexHullInternal :: [Point] -> [Point] -> [Point]
convexHullInternal psToKeep [_] = psToKeep
convexHullInternal (pLast:psToKeep) (pThis:pNext:psRemaining) = 
    if direction == RightTurn
        then convexHullInternal              psToKeep  (pLast:pNext:psRemaining)
        else convexHullInternal (pThis:pLast:psToKeep)       (pNext:psRemaining)
    where
        direction = directionOfTurn pLast pThis pNext

convexHull :: [Point] -> [Point]
convexHull ps
    | length ps < 4 = ps
    | otherwise     = convexHullInternal [pOrigin] sortedPs
                      where 
                          (pOrigin, sortedPs) = sortPointsFromOrigin ps
