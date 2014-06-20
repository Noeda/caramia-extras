{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Caramia.Prelude hiding ( left, right, subtract )
import Caramia.Extras.Rectangle
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Lens hiding ( children )
import Data.List ( sort, sortBy, permutations )

instance Arbitrary a => Arbitrary (ARectangle a) where
    arbitrary = do
        (l, t, r, b) <- arbitrary
        return $ ltrb l t r b

instance Show (Identity Int) where
    show x = "<runIdentity ==> " ++ show (runIdentity x) ++ ">"

instance Eq (Identity Int) where
    x1 == x2 = runIdentity x1 == runIdentity x2

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Caramia.Extras.Rectangle" [
        testProperty "left-lens" prop_leftlens
      , testProperty "right-lens" prop_rightlens
      , testProperty "top-lens" prop_toplens
      , testProperty "bottom-lens" prop_bottomlens
      , testProperty "ltrb-lens-equivalence" prop_ltrb
      , testProperty "ltwh-lens-equivalence" prop_ltwh
      , testProperty "normalize" prop_normalize
      , testProperty "normalize-isNormal-equivalence" prop_normalize_isnormal_eq
      , testProperty "width-lr-equivalence" prop_width_lr
      , testProperty "height-tb-equivalence" prop_height_tb
      , testProperty "subtract-no-overlap" prop_subtract_no_overlap
      , testProperty "subtract-fullcover" prop_subtract_fullcover
      , testProperty "subtract-complex-case" prop_subtract_complex_case
      , testProperty "overlaps" prop_overlaps
      , testProperty "overlapping-self" prop_overlapping_self
      , testProperty "overlapping" prop_overlapping
      , testProperty "overlap-overlapping-equivalence" prop_overlaps_overlapping
      , testProperty "isInside" prop_isinside
      , testProperty "isInside'-equivalence"
          prop_isinside_curried_and_uncurried_match
      , testProperty "viewArea-squares" prop_viewarea_squares
      , testProperty "byArea" prop_byarea
      , testProperty "byArea-type" prop_byarea_type
      , testProperty "byArea-type-byArea-equivalance" prop_byarea_byarea_type_eq
      , testProperty "maximize" prop_maximize
      , testProperty "cover-fullcover" prop_cover_fullcover
      , testProperty "cover-isinside-positive" prop_cover_point_test_positive
      , testProperty "cover-isinside-negative" prop_cover_point_test_negative
      , testProperty "cover-rectangle-outside" prop_cover_rectangle_outside
      , testProperty "cover-4buildup" prop_cover_4buildup
      ]
    ]

prop_leftlens :: Rectangle -> Int -> Bool
prop_leftlens rect y =
    ( rect^.left+y == after^.left &&
      rect^.right == after^.right &&
      rect^.top == after^.top &&
      rect^.bottom == after^.bottom &&
      rect^.height == after^.height )
  where
    after = rect & (left +~ y)

prop_rightlens :: Rectangle -> Int -> Bool
prop_rightlens rect y =
    ( rect^.left == after^.left &&
      rect^.right+y == after^.right &&
      rect^.top == after^.top &&
      rect^.bottom == after^.bottom &&
      rect^.height == after^.height )
  where
    after = rect & (right +~ y)

prop_toplens :: Rectangle -> Int -> Bool
prop_toplens rect y =
    ( rect^.left == after^.left &&
      rect^.right == after^.right &&
      rect^.top+y == after^.top &&
      rect^.bottom == after^.bottom &&
      rect^.width == after^.width )
  where
    after = rect & (top +~ y)

prop_bottomlens :: Rectangle -> Int -> Bool
prop_bottomlens rect y =
    ( rect^.left == after^.left &&
      rect^.right == after^.right &&
      rect^.top == after^.top &&
      rect^.bottom+y == after^.bottom &&
      rect^.width == after^.width )
  where
    after = rect & (bottom +~ y)

prop_isinside :: Rectangle -> (Int, Int) -> Bool
prop_isinside orig@(normalize -> rect) (x, y) =
    (x >= rect^.left && x <= rect^.right &&
     y >= rect^.top && y <= rect^.bottom) == isInside x y orig

prop_isinside_curried_and_uncurried_match :: Rectangle -> (Int, Int) -> Bool
prop_isinside_curried_and_uncurried_match rect (x, y) =
    isInside x y rect == isInside' (x, y) rect

prop_viewarea_squares :: Int -> Int -> Int -> Property
prop_viewarea_squares x y side = side >= 0 ==>
    viewArea (ltwh x y side side) == side*side

prop_ltrb :: Int -> Int -> Int -> Int -> Bool
prop_ltrb le to ri bo =
    rect^.left == le &&
    rect^.top == to &&
    rect^.right == ri &&
    rect^.bottom == bo &&
    rect^.width == (ri-le+1) &&
    rect^.height == (bo-to+1)
  where
    rect = ltrb le to ri bo

prop_ltwh :: Int -> Int -> Int -> Int -> Bool
prop_ltwh le to wi he =
    rect^.left == le &&
    rect^.top == to &&
    rect^.right == le+wi-1 &&
    rect^.bottom == to+he-1 &&
    rect^.width == wi &&
    rect^.height == he
  where
    rect = ltwh le to wi he

prop_normalize :: Rectangle -> Bool
prop_normalize rect =
    min (rect^.left) (rect^.right) == (normalize rect^.left) &&
    max (rect^.left) (rect^.right) == (normalize rect^.right) &&
    min (rect^.top) (rect^.bottom) == (normalize rect^.top) &&
    max (rect^.top) (rect^.bottom) == (normalize rect^.bottom)

prop_normalize_isnormal_eq :: Rectangle -> Bool
prop_normalize_isnormal_eq rect =
    if isNormal rect
      then normalize rect == rect
      else normalize rect /= rect

prop_width_lr :: Rectangle -> Int -> Property
prop_width_lr (normalize -> rect) x = abs x < (rect^.width) ==>
    rect^.width + x == after^.width
  where
    after = normalize $ rect & right +~ x

prop_height_tb :: Rectangle -> Int -> Property
prop_height_tb (normalize -> rect) x = abs x < (rect^.height) ==>
    rect^.height + x == after^.height
  where
    after = normalize $ rect & bottom +~ x

prop_overlaps :: Rectangle -> Rectangle -> Bool
prop_overlaps rect1 rect2 =
    overlaps rect1 rect2 ==
    not (nrect1^.right < nrect2^.left ||
         nrect1^.bottom < nrect2^.top ||
         nrect1^.left > nrect2^.right ||
         nrect1^.top > nrect2^.bottom)
  where
    nrect1 = normalize rect1
    nrect2 = normalize rect2

prop_byarea :: Rectangle -> Rectangle -> Property
prop_byarea rect1 rect2 = viewArea rect1 /= viewArea rect2 ==>
    (viewArea rect1 < viewArea rect2) == (byArea rect1 rect2 == LT) &&
    (viewArea rect1 > viewArea rect2) == (byArea rect1 rect2 == GT)

prop_byarea_type :: Rectangle -> Rectangle -> Bool
prop_byarea_type rect1 rect2 =
    sort [arect1, arect2] ==
        if | arect1 < arect2 -> [arect1, arect2]
           | arect1 > arect2 -> [arect2, arect1]
           | otherwise -> fmap ByArea (sort [rect1, rect2])
  where
    arect1 = ByArea rect1
    arect2 = ByArea rect2

prop_byarea_byarea_type_eq :: [Rectangle] -> Bool
prop_byarea_byarea_type_eq rects =
    fmap ByArea (sortBy byArea rects) == sort (fmap ByArea rects)

prop_maximize :: Rectangle -> Rectangle -> Bool
prop_maximize (normalize -> rect1) (normalize -> rect2) =
    viewArea mm >= viewArea rect1 &&
    viewArea mm >= viewArea rect2 &&
    overlaps mm rect1 &&
    overlaps mm rect2 &&
    (mm^.left == rect1^.left || mm^.left == rect2^.left) &&
    (mm^.right == rect1^.right || mm^.right == rect2^.right) &&
    (mm^.top == rect1^.top || mm^.top == rect2^.top) &&
    (mm^.bottom == rect1^.bottom || mm^.bottom == rect2^.bottom)
  where
    mm = maximize rect1 rect2

prop_subtract_no_overlap :: Rectangle -> Rectangle -> Property
prop_subtract_no_overlap (normalize -> rect1) (normalize -> rect2) =
    not (rect2 `overlaps` rect1) ==> subtract rect2 rect1 == [rect2]

prop_subtract_fullcover :: Rectangle -> Rectangle -> Property
prop_subtract_fullcover (normalize -> rect1) (normalize -> rect2) =
    fullCover rect2 rect1 ==> subtract rect2 rect1 == []

prop_subtract_complex_case :: Rectangle -> Rectangle -> Property
prop_subtract_complex_case (normalize -> rect1) (normalize -> rect2) =
    rect1 `overlaps` rect2 && not (fullCover rect2 rect1) ==>
        let rects = subtract rect2 rect1
            len = length rects
            area = sum (fmap viewArea rects)
         in len >= 1 &&
            len <= 4 &&
            allPairs rects (\r1 r2 ->
                         r1 == r2 ||
                         not (overlaps r1 r2)) &&
            all isNormal rects &&
            all (flip fullCover rect2) rects &&
            all (overlaps rect2) rects &&
            all (not . overlaps rect1) rects &&
            area < viewArea rect2 &&
            area == viewArea rect2 -
                    viewArea (fromJust $ overlapping rect2 rect1) &&
            (if fullCover rect1 rect2 &&
                rect2^.top /= rect1^.top &&
                rect2^.bottom /= rect1^.bottom &&
                rect2^.left /= rect1^.left &&
                rect2^.right /= rect1^.right
               then len == 4
               else True) &&
            (if fullCover rect1 rect2 &&
                (rect2^.top == rect1^.top ||
                 rect2^.bottom == rect1^.bottom ||
                 rect2^.right == rect1^.right ||
                 rect2^.left == rect1^.left)
              then len < 4
              else True)
  where
    allPairs :: [a] -> (a -> a -> Bool) -> Bool
    allPairs xs fun = rec xs xs
      where
          rec [] _ = True
          rec (x:rest) [] = rec rest xs
          rec xs@(x1:_) (x2:rest) = fun x1 x2 && rec xs rest

prop_overlapping_self :: Rectangle -> Bool
prop_overlapping_self (normalize -> rect) =
    overlapping rect rect == Just rect

prop_overlapping :: Rectangle -> Rectangle -> Property
prop_overlapping (normalize -> r1) (normalize -> r2) = overlaps r1 r2 ==>
    isJust result &&
    isJust result2 &&
    viewArea r1 >= viewArea result' &&
    viewArea r2 >= viewArea result' &&
    result' == result2' &&
    overlapping r1 result' == Just result' &&
    overlapping r2 result' == Just result'
  where
    result' = fromJust result
    result2' = fromJust result2

    result = overlapping r1 r2
    result2 = overlapping r2 r1

prop_overlaps_overlapping :: Rectangle -> Rectangle -> Bool
prop_overlaps_overlapping rect1 rect2
    | rect1 `overlaps` rect2 = isJust $ rect1 `overlapping` rect2
    | otherwise = isNothing $ rect1 `overlapping` rect2

prop_cover_fullcover :: [Rectangle] -> Bool
prop_cover_fullcover rects =
    all (flip fullCover cover) rects
  where
    cover = addManyToCover rects mempty

prop_cover_point_test_positive :: [Rectangle] -> (Int, Int) -> Property
prop_cover_point_test_positive rects coords = any (isInside' coords) rects ==>
    isInside' coords (addManyToCover rects mempty)

prop_cover_point_test_negative :: [Rectangle] -> (Int, Int) -> Property
prop_cover_point_test_negative rects coords =
    all (not . isInside' coords) rects ==>
    not (isInside' coords (addManyToCover rects mempty))

prop_cover_rectangle_outside :: [Rectangle] -> Rectangle -> Property
prop_cover_rectangle_outside rects rect =
    all (not . overlaps rect) rects ==>
    not (fullCover rect (addManyToCover rects mempty))

prop_cover_4buildup :: Rectangle -> Int -> Int -> Property
prop_cover_4buildup (normalize -> rect) x y =
    rect^.height > 4 && rect^.width > 4 &&
    x > 1 && y > 1 &&
    x < (rect^.width - 1) && y < (rect^.height - 1) ==>

    all (\rects ->
            fullCover rect (addManyToCover rects mempty) &&
            all (not . fullCover rect) rects &&
            not (fullCover rect (addManyToCover (tail rects) mempty)))
        (permutations baseRects)
  where
    baseRects =
        [ -- top-left
          ltrb (rect^.left) (rect^.top) (rect^.right-x) (rect^.bottom-y)
          -- top-right
        , ltrb (rect^.right-x+1) (rect^.top) (rect^.right) (rect^.bottom-y)
          -- bottom-left
        , ltrb (rect^.left) (rect^.bottom-y+1) (rect^.right-x) (rect^.bottom)
          -- bottom-right
        , ltrb (rect^.right-x+1) (rect^.bottom-y+1) (rect^.right) (rect^.bottom) ]


