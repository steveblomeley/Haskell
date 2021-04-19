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
sortFromOrigin po = sortBy (\p1 p2 -> sorter po p1 p2)

sorter :: Point -> Point -> Point -> Ordering
sorter po p1 p2 = if angle1 /= angle2
                      then compare angle1 angle2                            
                      else compare distance1 distance2
                  where
                      angle1 = angleFromOrigin po p1
                      angle2 = angleFromOrigin po p2
                      distance1 = distanceBetween po p1
                      distance2 = distanceBetween po p2
                      
angleFromOrigin :: Point -> Point -> Double
angleFromOrigin (xo,yo) (x,y) = atan2 (y-yo) (x-xo)

distanceBetween :: Point -> Point -> Double
distanceBetween (xo,yo) (x,y) = sqrt( ((x-xo)^2) + ((y-yo)^2) )

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
