-- | Axis aligned rectangles in two-dimensional space.
--

{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module Caramia.Extras.Rectangle
    (
    -- * Construction
      ltrb
    , ltwh
    -- * Types
    , ARectangle()
    , Rectangle
    , IRectangle
    , FRectangle
    , DRectangle
    , ACover()
    , PointCovering(..)
    , RectangleCovering(..)
    , NextAfter(..)
    , NextAfterDir(..)
    , Halvable(..)
    -- * Transformations
    , normalize
    , maximize
    , subtract
    , overlapping
    -- * Lenses
    , left
    , top
    , right
    , bottom
    , width
    , height
    , lt
    , rb
    -- * Tests
    , overlaps
    , isNormal
    , isInside'
    -- * Views
    , viewArea
    -- * Orderings
    , byArea
    , ByArea(..)
    -- * Rectangle covers
    , addToCover
    , addManyToCover )
    where

import Caramia.Prelude hiding ( left, right, subtract )
import Control.Lens
import Foreign.C.Types

foreign import ccall unsafe "nextafter"
    c_nextafter :: CDouble -> CDouble -> CDouble
foreign import ccall unsafe "nextafterf"
    c_nextafterf :: CFloat -> CFloat -> CFloat

data NextAfterDir = Positive | Negative
                    deriving ( Eq, Ord, Show, Read, Typeable )

-- | Class of values that have a concept of successor.
--
-- If @x@ is input, for integral types, `nextAfter` should return @ x+1 @. For
-- floating point values, it should return the smallest value that is greater
-- than @x@. Instances are provided for common numerical types.
class NextAfter a where
    nextAfter :: NextAfterDir -> a -> a

instance NextAfter CDouble where
    nextAfter Positive x =
        c_nextafter x (1/0)
    nextAfter Negative x =
        c_nextafter x (-(1/0))

instance NextAfter CFloat where
    nextAfter Positive x =
        c_nextafterf x (1/0)
    nextAfter Negative x =
        c_nextafterf x (-(1/0))

instance NextAfter Float where
    nextAfter dir x = unwrap $ nextAfter dir (CFloat x)
      where
        unwrap (CFloat x) = x

instance NextAfter Double where
    nextAfter dir x = unwrap $ nextAfter dir (CDouble x)
      where
        unwrap (CDouble x) = x

instance NextAfter Integer where
    nextAfter Positive x = x+1
    nextAfter Negative x = x-1

instance NextAfter Int    where nextAfter = intNextAfter
instance NextAfter Word8  where nextAfter = intNextAfter
instance NextAfter Word16 where nextAfter = intNextAfter
instance NextAfter Word32 where nextAfter = intNextAfter
instance NextAfter Word64 where nextAfter = intNextAfter
instance NextAfter Int8   where nextAfter = intNextAfter
instance NextAfter Int16  where nextAfter = intNextAfter
instance NextAfter Int32  where nextAfter = intNextAfter
instance NextAfter Int64  where nextAfter = intNextAfter
instance NextAfter CInt   where nextAfter = intNextAfter
instance NextAfter CUInt  where nextAfter = intNextAfter
instance NextAfter CLong  where nextAfter = intNextAfter
instance NextAfter CULong  where nextAfter = intNextAfter
instance NextAfter CShort  where nextAfter = intNextAfter
instance NextAfter CUShort where nextAfter = intNextAfter
instance NextAfter CUChar  where nextAfter = intNextAfter
instance NextAfter CChar   where nextAfter = intNextAfter
instance NextAfter CSize   where nextAfter = intNextAfter

instance Halvable Int where halfCut = intHalfCut
instance Halvable Word8 where halfCut = intHalfCut
instance Halvable Word16 where halfCut = intHalfCut
instance Halvable Word32 where halfCut = intHalfCut
instance Halvable Word64 where halfCut = intHalfCut
instance Halvable Int8 where halfCut = intHalfCut
instance Halvable Int16 where halfCut = intHalfCut
instance Halvable Int32 where halfCut = intHalfCut
instance Halvable Int64 where halfCut = intHalfCut
instance Halvable CInt   where halfCut = intHalfCut
instance Halvable CUInt  where halfCut = intHalfCut
instance Halvable CLong  where halfCut = intHalfCut
instance Halvable CULong  where halfCut = intHalfCut
instance Halvable CShort  where halfCut = intHalfCut
instance Halvable CUShort where halfCut = intHalfCut
instance Halvable CUChar  where halfCut = intHalfCut
instance Halvable CChar   where halfCut = intHalfCut
instance Halvable CSize   where halfCut = intHalfCut

intHalfCut :: Integral a => a -> (a, a)
intHalfCut x = let h = x `div` 2 in ( h, h+1 )

intNextAfter :: (Eq a, Bounded a, Num a) => NextAfterDir -> a -> a
intNextAfter Positive x
    | x == maxBound = error "intNextAfter: integral maximum reached."
    | otherwise = x+1
intNextAfter Negative x
    | x == minBound = error "intNextAfter: integral minimum reached."
    | otherwise = x-1

class NextAfter a => Halvable a where
    halfCut :: a -> (a, a)

instance Halvable CDouble where
    halfCut (CDouble d) = halfCut d & over both CDouble
    {-# INLINE halfCut #-}

instance Halvable CFloat where
    halfCut (CFloat f) = halfCut f & over both CFloat
    {-# INLINE halfCut #-}

instance Halvable Double where
    halfCut d = let h = d/2 in ( h, nextAfter Positive h )
    {-# INLINE halfCut #-}

instance Halvable Float where
    halfCut f = let h = f/2 in ( h, nextAfter Positive h )
    {-# INLINE halfCut #-}

-- | Type of an axis aligned rectangle in two-dimensional space.
data ARectangle a = ARectangle
    { _left :: !a
    , _top :: !a
    , _right :: !a
    , _bottom :: !a }
    deriving ( Eq
             , Ord
             , Show
             , Read
             , Typeable
             , Functor )

-- | Creates a rectangle by specifying left, top, right and bottom.
--
-- @ ltrb = left-top-right-bottom @. All values are inclusive, that is, a
-- a @ ltrb 5 5 5 5 @ is has area of 1 and has a width and height of 1.
ltrb :: a -> a -> a -> a -> ARectangle a
ltrb = ARectangle

-- | Creates a rectangle by specifying left, top, width and height.
--
-- @ ltwh = left-top-width-height @. Left and top are inclusive. Width and
-- height specify the size of the rectangle.
ltwh :: Num a => a -> a -> a -> a -> ARectangle a
ltwh left top width height =
    ARectangle { _left = left
               , _top = top
               , _right = left + width - 1
               , _bottom = top + height - 1 }

-- | `Int` rectangle.
type Rectangle = ARectangle Int
-- | `Integer` rectangle.
type IRectangle = ARectangle Integer
-- | `Float` rectangle.
type FRectangle = ARectangle Float
-- | `Double rectangle.
type DRectangle = ARectangle Double

-- | Lens to left and top of rectangle.
lt :: Lens' (ARectangle a) (a, a)
lt = lens (\old -> (_left old, _top old))
          (\old (new_left, new_top) -> old { _left = new_left
                                           , _top = new_top })

-- | Lens to right and bottom of rectangle.
rb :: Lens' (ARectangle a) (a, a)
rb = lens (\old -> (_right old, _bottom old))
          (\old (new_right, new_bottom) -> old { _right = new_right
                                               , _bottom = new_bottom })

left :: Lens' (ARectangle a) a
left = lens _left (\old new -> old { _left = new })
{-# INLINE left #-}

right :: Lens' (ARectangle a) a
right = lens _right (\old new -> old { _right = new })
{-# INLINE right #-}

top :: Lens' (ARectangle a) a
top = lens _top (\old new -> old { _top = new })
{-# INLINE top #-}

bottom :: Lens' (ARectangle a) a
bottom = lens _bottom (\old new -> old { _bottom = new })
{-# INLINE bottom #-}

width :: Num a => Lens' (ARectangle a) a
width = lens (\x -> _right x - _left x + 1)
             (\old new -> old { _right = _left old + new - 1 })
{-# INLINE width #-}

height :: Num a => Lens' (ARectangle a) a
height = lens (\x -> _bottom x - _top x + 1)
              (\old new -> old { _bottom = _top old + new - 1 })
{-# INLINE height #-}

-- | Returns the area of a rectangle.
viewArea :: (Num a) => ARectangle a -> a
viewArea rect = (rect^.width) * (rect^.height)
{-# INLINE viewArea #-}

-- | Normalizes a rectangle.
--
-- In this context, normalizing means checking that the right side of the
-- rectangle is actually on the right of the left side and bottom side is
-- actually at the bottom. This makes sure `viewArea` returns a positive value
-- and that `right` is guaranteed to be equal or greater than `left` and
-- `bottom` is equal or greater than `top`.
normalize :: Ord a => ARectangle a -> ARectangle a
normalize rect = rect &
    (if _right rect < _left rect
       then (right .~ _left rect) . (left .~ _right rect)
       else id) .
    (if _bottom rect < _top rect
       then (bottom .~ _top rect) . (top .~ _bottom rect)
       else id)
{-# INLINE normalize #-}

-- | Returns `True` if a rectangle is in \"normal\", form, that is, if
-- invoking `normalize` on it would just return the same rectangle.
isNormal :: Ord a => ARectangle a -> Bool
isNormal (ARectangle {..}) = _right >= _left && _bottom >= _top
{-# INLINE isNormal #-}

-- | A function suitable for functions such as `sortBy`. It will order
-- rectangles by their area.
--
-- If areas are equivalent, then the position of the rectangle is considered in
-- an unspecified way.
--
-- Also see `ByArea` data type.
byArea :: (Num a, Ord a) => ARectangle a -> ARectangle a -> Ordering
byArea rect1 rect2 =
    case compare (viewArea rect1) (viewArea rect2) of
        EQ -> rect1 `compare` rect2
        LT -> LT
        GT -> GT
{-# INLINE byArea #-}

-- | A newtype wrapper for `ARectangle` that replaces the `Ord` instance with
-- an implementation equivalent to `byArea`.
newtype ByArea a = ByArea { unwrapByArea :: ARectangle a }
                   deriving ( Eq, Show, Read, Typeable, Functor )

instance (Num a, Ord a, Eq a) => Ord (ByArea a) where
    ByArea x `compare` ByArea y = x `byArea` y

-- | Returns `True` if two given rectangles overlap.
--
-- This function normalizes the rectangles before doing the test.
overlaps :: Ord a => ARectangle a -> ARectangle a -> Bool
overlaps (normalize -> rect1) (normalize -> rect2) = not $
    (_right rect1 < _left rect2) ||
    (_left rect1 > _right rect2) ||
    (_bottom rect1 < _top rect2) ||
    (_top rect1 > _bottom rect2)
{-# INLINE overlaps #-}

-- | Returns the overlapping area of two rectangles, or `Nothing` if the
-- rectangles do not overlap.
--
-- This function normalizes the rectangles before doing the operation.
overlapping :: Ord a => ARectangle a -> ARectangle a -> Maybe (ARectangle a)
overlapping (normalize -> rect1) (normalize -> rect2) =
    if _right result < _left result ||
       _bottom result < _top result
      then Nothing
      else Just result
  where
    result = ARectangle
      { _left = max (_left rect1) (_left rect2)
      , _right = min (_right rect1) (_right rect2)
      , _top = max (_top rect1) (_top rect2)
      , _bottom = min (_bottom rect1) (_bottom rect2) }
{-# INLINE overlapping #-}

-- | This function \"subtracts\" overlapping area from a rectangle.
--
-- Here's an example.
--
-- @
--    Given two rectangles A and B (with the overlapping area marked with
--    dashes):
--
--    AAAAAAAAAAAA
--    AAAAAAAAAAAA
--    AAAAAAAAAAAA
--    AAAAAAA-----BBBBBBBB
--    AAAAAAA-----BBBBBBBB
--           BBBBBBBBBBBBB
--           BBBBBBBBBBBBB
--
--    Results in two rectangles C and D.
--
--    AAAAAAAAAAAA
--    AAAAAAAAAAAA
--    AAAAAAAAAAAA
--    AAAAAAA-----CCCCCCCC
--    AAAAAAA-----CCCCCCCC
--           DDDDDCCCCCCCC
--           DDDDDCCCCCCCC
--  @
--
--  At maximum, there can be 4 returned rectangles which happens when the
--  subtracted area is completely covered by the source rectangle.
subtract :: (Ord a, NextAfter a)
         => ARectangle a   -- ^ Subtract from this rectangle.
         -> ARectangle a
         -> [ARectangle a]   -- ^ Either 0, 1, 2, 3 or 4 rectangles
                             -- , depending on how the given rectangles
                             -- overlap.
subtract (normalize -> rect1) (normalize -> rect2)
    | not $ overlaps rect1 rect2 = [rect1]
    | fullCover rect1 rect2 = []
    | otherwise =
        let rrect8 = ARectangle { _left = max (_left rect2) (_left rect1)
                                , _right = min (_right rect2) (_right rect1)
                                , _top = _top rect1
                                , _bottom = nextAfter Negative $ _top rect2 }
            rrect2 = ARectangle { _left = max (_left rect2) (_left rect1)
                                , _right = min (_right rect2) (_right rect1)
                                , _top = nextAfter Positive $ _bottom rect2
                                , _bottom = _bottom rect1 }
            rrect4 = ARectangle { _left = _left rect1
                               , _right = nextAfter Negative $ _left rect2
                               , _top = _top rect1
                               , _bottom = _bottom rect1 }
            rrect6 = ARectangle { _left = nextAfter Positive $ _right rect2
                               , _right = _right rect1
                               , _top = _top rect1
                               , _bottom = _bottom rect1 }
         in filter isNormal [rrect2, rrect4, rrect6, rrect8]
{-# INLINABLE subtract #-}

-- | Class of things that can cover axis-aligned rectangles in two-dimensional
-- space.
--
-- Minimal implementation: `fullCover`.
class RectangleCovering cov a where
    -- | Returns `True` if the given rectangle is completely inside the given
    -- covering.
    fullCover :: ARectangle a -> cov -> Bool

instance Ord a => RectangleCovering (ARectangle a) a where
    fullCover rect1 rect2 =
        rect1^.left >= rect2^.left &&
        rect1^.right <= rect2^.right &&
        rect1^.top >= rect2^.top &&
        rect1^.bottom <= rect2^.bottom

instance (Ord a, NextAfter a) => RectangleCovering (ACover a) a where
    fullCover rect (_coverings -> covers) =
        folder [rect] covers
      where
        folder [] _ = True
        folder _ [] = False
        folder rects (cover:rest) =
            folder (concatMap (flip subtract cover) rects)
                   rest

-- | Class of things that can cover points in two-dimensional space.
--
-- Minimal implementation: `isInside`.
class PointCovering cov val where
    -- | Tests if a point is inside a rectangle.
    --
    -- This function normalizes the rectangle before doing the test.
    --
    -- The first argument is where the point is on the X axis (left-right) and the
    -- second is on the Y (top-bottom) axis.
    isInside :: val -> val -> cov -> Bool

-- | Same as `isInside` but uncurried to take a tuple.
isInside' :: PointCovering cov a => (a, a) -> cov -> Bool
isInside' (x, y) = isInside x y

instance Ord a => PointCovering (ARectangle a) a where
    isInside x y (normalize -> ARectangle{..}) =
        x >= _left && x <= _right &&
        y >= _top && y <= _bottom
    {-# INLINE isInside #-}

instance Ord a => PointCovering (ACover a) a where
    isInside x y (_coverings -> covers) = any (isInside x y) covers

-- | Given two rectangles, returns a rectangle that encompasses both
-- rectangles.
--
-- This normalizes both rectangles before doing its operation.
maximize :: Ord a => ARectangle a -> ARectangle a -> ARectangle a
maximize (normalize -> rect1) (normalize -> rect2) = ARectangle
    { _left = min (_left rect1) (_left rect2)
    , _top = min (_top rect1) (_top rect2)
    , _right = max (_right rect1) (_right rect2)
    , _bottom = max (_bottom rect1) (_bottom rect2) }

-- | Type of rectangle covers.
--
-- It can be thought as a set of rectangles that together cover an area.
--
-- Be careful of time complexity. Adding a rectangle is O(N) complexity and
-- checking is a point is covered by the cover is also O(N).
--
-- Use `mempty` to create an empty cover.
newtype ACover a = ACover
    { _coverings :: [ARectangle a] }
    deriving ( Eq, Ord, Show, Read, Typeable, Functor )

-- | `mempty` for `ACover` covers nothing and appending two covers together
-- results in a cover that now covers both areas.
instance Monoid (ACover a) where
    mempty = ACover { _coverings = [] }
    (_coverings -> cover1) `mappend` (_coverings -> cover2) =
        ACover { _coverings = cover1 `mappend` cover2 }

-- | This instance follows the same appending rule as the `Monoid` instance.
instance Semigroup (ACover a) where
    cover1 <> cover2 = cover1 `mappend` cover2

coverings :: Lens (ACover a) (ACover b) [ARectangle a] [ARectangle b]
coverings = lens _coverings (\old new -> old { _coverings = new })
{-# INLINE coverings #-}

-- | Adds another rectangle to the cover.
--
-- This is O(N) to the number of rectangles already added to the cover.
--
-- This does not increase the size of the cover if the given rectangle would be
-- fully covered already by the existing rectangles.
addToCover :: (NextAfter a, Ord a) => ARectangle a -> ACover a -> ACover a
addToCover rect cover
    | fullCover rect cover = cover
    | otherwise = cover & coverings %~ (rect:)

-- | Adds many rectangles to a cover.
--
-- Uses `addManyToCover` so time complexity is O(N*M) where M is the number of
-- rectangles to add and N is the number of rectangles already in the cover.
addManyToCover :: (NextAfter a, Ord a) => [ARectangle a] -> ACover a -> ACover a
addManyToCover [] cover = cover
addManyToCover xs cover = foldl' (flip addToCover) cover xs

