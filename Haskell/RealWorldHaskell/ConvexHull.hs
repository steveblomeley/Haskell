import Data.List

type Point = (Double,Double)

data Direction = LeftTurn | RightTurn deriving Eq

xValue :: Point -> Double
xValue = fst

yValue :: Point -> Double
yValue = snd

xValues :: [Point] -> [Double]
xValues = map xValue

yValues :: [Point] -> [Double]
yValues = map yValue

xEquals :: Double -> Point -> Bool
xEquals n p = (xValue p) == n

yEquals :: Double -> Point -> Bool
yEquals n p = (yValue p) == n

whereXEquals :: [Point] -> Double -> [Point]
whereXEquals ps x = filter (xEquals x) ps

whereYEquals :: [Point] -> Double -> [Point]
whereYEquals ps y = filter (yEquals y) ps

lowest :: Ord a => [a] -> a
lowest = foldr1 min

leftMostBottomMost :: [Point] -> Point
leftMostBottomMost ps = 
    head $ bottomMostPs `whereXEquals` lowestX
    where 
        lowestX = lowest $ xValues bottomMostPs
        bottomMostPs = ps `whereYEquals` lowestY
        lowestY = lowest $ yValues ps

angleFromOrigin :: Point -> Point -> Double
angleFromOrigin (xo,yo) (x,y) = atan2 (y-yo) (x-xo)

distanceBetween :: Point -> Point -> Double
distanceBetween (xo,yo) (x,y) = sqrt( ((x-xo)^2) + ((y-yo)^2) )

sorter :: Point -> Point -> Point -> Ordering
sorter po p1 p2 = if angle1 /= angle2
                      then compare angle1 angle2                            
                      else compare distance1 distance2
                  where
                      angle1 = angleFromOrigin po p1
                      angle2 = angleFromOrigin po p2
                      distance1 = distanceBetween po p1
                      distance2 = distanceBetween po p2
                      
sortPointsFromOrigin :: [Point] -> (Point,[Point])
sortPointsFromOrigin ps = (pOrigin, psSorted ++ [pOrigin])
                          where
                              psSorted = sortBy (sorter pOrigin) remainingPs
                              remainingPs = filter ((/=) pOrigin) ps
                              pOrigin = leftMostBottomMost ps

directionOfTurn :: Point -> Point -> Point -> Direction
directionOfTurn (x1,y1) (x2,y2) (x3,y3) = 
    if zCoordinateOfXProductOfVectors < 0
        then RightTurn
        else LeftTurn
    where
        zCoordinateOfXProductOfVectors = (x2 - x1) * (y3 -y1) - (y2 - y1) * (x3 - x1)               

convexHullInternal :: [Point] -> Point -> [Point] -> [Point]
convexHullInternal psToKeep pOrigin [] = psToKeep
convexHullInternal (pLast:psToKeep) pThis (pNext:psRemaining)
    | direction == RightTurn = convexHullInternal psToKeep pLast (pNext:psRemaining)        -- discard and move back
    | otherwise              = convexHullInternal (pThis:pLast:psToKeep) pNext psRemaining  -- keep and move forward
    where
        direction = directionOfTurn pLast pThis pNext

convexHull :: [Point] -> [Point]
convexHull ps
    | length ps < 4 = ps
    | otherwise     = convexHullInternal [pOrigin] (head sortedPs) (tail sortedPs)
                      where 
                          (pOrigin, sortedPs) = sortPointsFromOrigin ps
