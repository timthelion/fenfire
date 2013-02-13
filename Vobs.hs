{-# LINE 1 "Vobs.fhs" #-}
{-# OPTIONS_GHC -fth #-} {-# LANGUAGE OverlappingInstances, DoRec #-}
                         module Vobs where
{-# LINE 1 "Vobs.fhs" #-}
import qualified FunctorSugar
{-# LINE 22 "Vobs.fhs" #-}
import Utils
{-# LINE 24 "Vobs.fhs" #-}
import Cairo
{-# LINE 26 "Vobs.fhs" #-}
import Data.IORef
{-# LINE 27 "Vobs.fhs" #-}
import System.IO.Unsafe (unsafePerformIO)
{-# LINE 28 "Vobs.fhs" #-}
import qualified System.Time
{-# LINE 30 "Vobs.fhs" #-}
import Control.Applicative
{-# LINE 31 "Vobs.fhs" #-}
import Control.Monad.Reader
{-# LINE 32 "Vobs.fhs" #-}
import Control.Monad.Trans (liftIO, MonadIO)
{-# LINE 34 "Vobs.fhs" #-}
import Graphics.UI.Gtk hiding (Point, Size, Layout, Color, get)
{-# LINE 35 "Vobs.fhs" #-}
import qualified Graphics.Rendering.Cairo as C
{-# LINE 36 "Vobs.fhs" #-}
import Graphics.Rendering.Cairo.Matrix (Matrix(Matrix))
{-# LINE 37 "Vobs.fhs" #-}
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
{-# LINE 38 "Vobs.fhs" #-}
import Graphics.UI.Gtk.Cairo
{-# LINE 39 "Vobs.fhs" #-}
import Graphics.UI.Gtk.Gdk.EventM
{-# LINE 41 "Vobs.fhs" #-}
import Data.List (intersect)
{-# LINE 42 "Vobs.fhs" #-}
import Data.Map (Map, keys, fromList, toList, insert, empty)
{-# LINE 43 "Vobs.fhs" #-}
import qualified Data.Map as Map
{-# LINE 44 "Vobs.fhs" #-}
import Data.Maybe (fromMaybe, isJust)
{-# LINE 45 "Vobs.fhs" #-}
import Data.Monoid (Monoid(mempty, mappend))
{-# LINE 47 "Vobs.fhs" #-}
import Control.Monad (when)
{-# LINE 48 "Vobs.fhs" #-}
import Control.Monad.State
{-# LINE 49 "Vobs.fhs" #-}
import Control.Monad.Reader
 
{-# LINE 52 "Vobs.fhs" #-}
type Scene k = Map k (Maybe (Matrix, Size))
 
{-# LINE 53 "Vobs.fhs" #-}
data Vob k = Vob{defaultSize :: Size,
                 vobScene :: RenderContext k -> Scene k,
                 renderVob :: RenderContext k -> Render ()}
 
{-# LINE 57 "Vobs.fhs" #-}
type Cx k = MaybeT (Reader (RenderContext k))
 
{-# LINE 59 "Vobs.fhs" #-}
runCx :: RenderContext k -> Cx k a -> Maybe a
{-# LINE 60 "Vobs.fhs" #-}
runCx cx m = runReader (runMaybeT m) cx
 
{-# LINE 62 "Vobs.fhs" #-}
data RenderContext k = RenderContext{rcRect :: Rect,
                                     rcScene :: Scene k, rcFade :: Double, rcFgColor :: Color,
                                     rcBgColor :: Color, rcFadeColor :: Color}
{-# LINE 66 "Vobs.fhs" #-}
rcMatrix = fst . rcRect
{-# LINE 66 "Vobs.fhs" #-}
rcSize = snd . rcRect
 
{-# LINE 68 "Vobs.fhs" #-}
type View s k = s -> Vob k
 
{-# LINE 69 "Vobs.fhs" #-}
type Handler e s = e -> HandlerAction s
 
{-# LINE 71 "Vobs.fhs" #-}
type HandlerAction s = StateT s (StateT (Bool, Bool) IO) ()
 
{-# LINE 74 "Vobs.fhs" #-}
instance (Ord k) => Monoid (Vob k) where
        {-# LINE 75 "Vobs.fhs" #-}
        mempty = Vob (0, 0) (const Map.empty) (const $ return ())
        {-# LINE 76 "Vobs.fhs" #-}
        mappend (Vob (w1, h1) sc1 r1) (Vob (w2, h2) sc2 r2)
          = Vob (w, h) sc r
          where {-# LINE 77 "Vobs.fhs" #-}
                (w, h) = (max w1 w2, max h1 h2)
                {-# LINE 78 "Vobs.fhs" #-}
                sc cx = Map.union (sc1 cx) (sc2 cx)
                {-# LINE 79 "Vobs.fhs" #-}
                r cx = r1 cx >> r2 cx
 
{-# LINE 81 "Vobs.fhs" #-}
instance Functor (Cx k) where
        {-# LINE 81 "Vobs.fhs" #-}
        fmap = liftM
 
{-# LINE 82 "Vobs.fhs" #-}
instance Applicative (Cx k) where
        {-# LINE 83 "Vobs.fhs" #-}
        pure = return
        {-# LINE 84 "Vobs.fhs" #-}
        (<*>) = ap
 
{-# LINE 86 "Vobs.fhs" #-}
instance (Ord k) => Cairo (Cx k) (Vob k) where
        {-# LINE 87 "Vobs.fhs" #-}
        cxAsk = asks rcRect
        {-# LINE 89 "Vobs.fhs" #-}
        cxLocal rect m
          = do rect' <- rect
               local (\ cx -> cx{rcRect = rect'}) m
        {-# LINE 91 "Vobs.fhs" #-}
        cxWrap f (Vob size sc ren)
          = Vob size sc $ \ cx -> maybeDo (runCx cx $ f $ ren cx) id
        {-# LINE 94 "Vobs.fhs" #-}
        cxLocalR rect (Vob size sc ren)
          = Vob size
              (\ cx ->
                 let {-# LINE 95 "Vobs.fhs" #-}
                     msc = liftM sc (upd cx)
                   in Map.mapWithKey (\ k _ -> msc >>= (Map.! k)) (sc cx))
              (\ cx -> maybe (return ()) ren (upd cx))
          where {-# LINE 98 "Vobs.fhs" #-}
                upd cx
                  = do rect' <- runCx cx rect
                       return $ cx{rcRect = rect'}
{-# LINE 102 "Vobs.fhs" #-}
defaultWidth (Vob (w, _) _ _) = w
{-# LINE 103 "Vobs.fhs" #-}
defaultHeight (Vob (_, h) _ _) = h
 
{-# LINE 106 "Vobs.fhs" #-}
setInterp :: Bool -> HandlerAction s
{-# LINE 107 "Vobs.fhs" #-}
setInterp interp
  = lift $ modify $ \ (_, handled) -> (interp, handled)
 
{-# LINE 109 "Vobs.fhs" #-}
unhandledEvent :: HandlerAction s
{-# LINE 110 "Vobs.fhs" #-}
unhandledEvent = lift $ modify $ \ (interp, _) -> (interp, False)
{-# LINE 112 "Vobs.fhs" #-}
runHandler handleEvent state event
  = do (((), state'), (interpolate', handled)) <- runStateT
                                                    (runStateT (handleEvent event) state)
                                                    (False, True)
       return (state', interpolate', handled)
 
{-# LINE 118 "Vobs.fhs" #-}
(@@) :: (Ord k) => Cx k a -> k -> Cx k a
{-# LINE 119 "Vobs.fhs" #-}
x @@ key
  = do cx <- ask
       rect <- maybeReturn =<< Map.lookup key (rcScene cx)
       local (\ _ -> cx{rcRect = rect}) x
 
{-# LINE 124 "Vobs.fhs" #-}
changeSize :: (Ord k) => Endo Size -> Endo (Vob k)
{-# LINE 125 "Vobs.fhs" #-}
changeSize f vob = vob{defaultSize = f $ defaultSize vob}
 
{-# LINE 127 "Vobs.fhs" #-}
changeContext :: (Ord k) => Endo (RenderContext k) -> Endo (Vob k)
{-# LINE 128 "Vobs.fhs" #-}
changeContext f (Vob s sc r) = Vob s (sc . f) (r . f)
 
{-# LINE 130 "Vobs.fhs" #-}
changeRect :: (Ord k) => Endo Rect -> Endo (Vob k)
{-# LINE 131 "Vobs.fhs" #-}
changeRect f = changeContext (\ cx -> cx{rcRect = f $ rcRect cx})
 
{-# LINE 133 "Vobs.fhs" #-}
ownSize :: (Ord k) => Endo (Vob k)
{-# LINE 134 "Vobs.fhs" #-}
ownSize vob = changeRect (\ (m, _) -> (m, defaultSize vob)) vob
 
{-# LINE 136 "Vobs.fhs" #-}
invisibleVob :: (Ord k) => Endo (Vob k)
{-# LINE 137 "Vobs.fhs" #-}
invisibleVob = cxWrap (const mempty)
 
{-# LINE 140 "Vobs.fhs" #-}
comb :: Size -> (RenderContext k -> Vob k) -> Vob k
{-# LINE 141 "Vobs.fhs" #-}
comb size f
  = Vob size (\ cx -> vobScene (f cx) cx)
      (\ cx -> renderVob (f cx) cx)
 
{-# LINE 144 "Vobs.fhs" #-}
renderable :: (Ord k) => Size -> Render () -> Vob k
{-# LINE 145 "Vobs.fhs" #-}
renderable size ren
  = Vob size (const Map.empty) $
      \ cx ->
        do do C.save
              C.transform (rcMatrix cx)
              ren
              C.restore
 
{-# LINE 149 "Vobs.fhs" #-}
keyVob :: (Ord k) => k -> Endo (Vob k)
{-# LINE 150 "Vobs.fhs" #-}
keyVob key vob
  = vob{vobScene =
          \ cx -> Map.insert key (Just $ rcRect cx) (vobScene vob cx),
        renderVob =
          \ cx ->
            maybeDo (maybeReturn =<< (Map.lookup key $ rcScene cx)) $
              \ rect -> renderVob vob $ cx{rcRect = rect}}
 
{-# LINE 157 "Vobs.fhs" #-}
rectBox :: (Ord k) => Endo (Vob k)
{-# LINE 158 "Vobs.fhs" #-}
rectBox vob
  = useBgColor (C.fill extents) & clip extents vob &
      useFgColor (stroke extents)
 
{-# LINE 162 "Vobs.fhs" #-}
pangoContext :: PangoContext
{-# LINE 163 "Vobs.fhs" #-}
pangoContext
  = unsafePerformIO $
      do context <- cairoCreateContext Nothing
         desc <- contextGetFontDescription context
         fontDescriptionSetFamily desc "Sans"
         fontDescriptionSetSize desc (fromInteger 10)
         contextSetFontDescription context desc
         return context
 
{-# LINE 172 "Vobs.fhs" #-}
label :: (Ord k) => String -> Vob k
{-# LINE 173 "Vobs.fhs" #-}
label s
  = unsafePerformIO $
      do layout <- layoutText pangoContext s
         (PangoRectangle _ _ w1 h1,
          PangoRectangle _ _ w2 h2) <- layoutGetExtents layout
         let {-# LINE 177 "Vobs.fhs" #-}
             w = max w1 w2
             {-# LINE 177 "Vobs.fhs" #-}
             h = max h1 h2
         return $ renderable (realToFrac w, realToFrac h) $
           showLayout layout
 
{-# LINE 180 "Vobs.fhs" #-}
multiline :: (Ord k) => Bool -> Int -> String -> Vob k
{-# LINE 181 "Vobs.fhs" #-}
multiline useTextWidth widthInChars s
  = unsafePerformIO $
      do layout <- layoutText pangoContext s
         layoutSetWrap layout WrapPartialWords
         desc <- contextGetFontDescription pangoContext
         lang <- languageFromString s
         (FontMetrics{approximateCharWidth = cw, ascent = ascent',
                      descent = descent'}) <- contextGetMetrics pangoContext desc lang
         let {-# LINE 188 "Vobs.fhs" #-}
             w1 = fromIntegral widthInChars * cw
             {-# LINE 189 "Vobs.fhs" #-}
             h1 = ascent' + descent'
         layoutSetWidth layout (Just w1)
         (PangoRectangle _ _ w2 h2,
          PangoRectangle _ _ w3 h3) <- layoutGetExtents layout
         let {-# LINE 193 "Vobs.fhs" #-}
             w = if useTextWidth then max w2 w3 else w1
             {-# LINE 194 "Vobs.fhs" #-}
             h = maximum [h1, h2, h3]
         return $ renderable (realToFrac w, realToFrac h) $
           showLayout layout
 
{-# LINE 198 "Vobs.fhs" #-}
fadedColor :: (Ord k) => Endo (Cx k Color)
{-# LINE 199 "Vobs.fhs" #-}
fadedColor c
  = liftM3 interpolate (asks rcFade) (asks rcFadeColor) c
 
{-# LINE 201 "Vobs.fhs" #-}
setFgColor :: (Ord k) => Color -> Endo (Vob k)
{-# LINE 202 "Vobs.fhs" #-}
setFgColor c = changeContext $ \ cx -> cx{rcFgColor = c}
 
{-# LINE 204 "Vobs.fhs" #-}
setBgColor :: (Ord k) => Color -> Endo (Vob k)
{-# LINE 205 "Vobs.fhs" #-}
setBgColor c = changeContext $ \ cx -> cx{rcBgColor = c}
 
{-# LINE 207 "Vobs.fhs" #-}
useFgColor :: (Ord k) => Endo (Vob k)
{-# LINE 208 "Vobs.fhs" #-}
useFgColor = withColor (fadedColor $ asks rcFgColor)
 
{-# LINE 210 "Vobs.fhs" #-}
useBgColor :: (Ord k) => Endo (Vob k)
{-# LINE 211 "Vobs.fhs" #-}
useBgColor = withColor (fadedColor $ asks rcBgColor)
 
{-# LINE 213 "Vobs.fhs" #-}
useFadeColor :: (Ord k) => Endo (Vob k)
{-# LINE 214 "Vobs.fhs" #-}
useFadeColor = withColor (asks rcFadeColor)
 
{-# LINE 216 "Vobs.fhs" #-}
fade :: (Ord k) => Double -> Endo (Vob k)
{-# LINE 217 "Vobs.fhs" #-}
fade a = changeContext $ \ cx -> cx{rcFade = rcFade cx * a}
 
{-# LINE 220 "Vobs.fhs" #-}
centerVob :: (Ord k) => Endo (Vob k)
{-# LINE 221 "Vobs.fhs" #-}
centerVob vob = translate (pure (- w / 2)) (pure (- h / 2)) vob
  where {-# LINE 222 "Vobs.fhs" #-}
        (w, h) = defaultSize vob
 
{-# LINE 225 "Vobs.fhs" #-}
pad4 ::
       (Ord k) => Double -> Double -> Double -> Double -> Endo (Vob k)
{-# LINE 226 "Vobs.fhs" #-}
pad4 x1 x2 y1 y2 vob
  = changeSize (const (x1 + w + x2, y1 + h + y2)) $
      changeRect (\ (m, (w', h')) -> (f m, (w' - x1 - x2, h' - y1 - y2)))
        vob
  where {-# LINE 229 "Vobs.fhs" #-}
        (w, h) = defaultSize vob
        {-# LINE 229 "Vobs.fhs" #-}
        f = Matrix.translate x1 y1
 
{-# LINE 231 "Vobs.fhs" #-}
pad2 :: (Ord k) => Double -> Double -> Endo (Vob k)
{-# LINE 232 "Vobs.fhs" #-}
pad2 x y = pad4 x x y y
 
{-# LINE 234 "Vobs.fhs" #-}
pad :: (Ord k) => Double -> Endo (Vob k)
{-# LINE 235 "Vobs.fhs" #-}
pad pixels = pad2 pixels pixels
 
{-# LINE 238 "Vobs.fhs" #-}
class Interpolate a where
         
        {-# LINE 239 "Vobs.fhs" #-}
        interpolate :: Double -> Op a
 
{-# LINE 241 "Vobs.fhs" #-}
instance Interpolate Double where
        {-# LINE 242 "Vobs.fhs" #-}
        interpolate fract x y = (1 - fract) * x + fract * y
 
{-# LINE 244 "Vobs.fhs" #-}
instance Interpolate Color where
        {-# LINE 245 "Vobs.fhs" #-}
        interpolate fract (Color r g b a) (Color r' g' b' a')
          = Color (i r r') (i g g') (i b b') (i a a')
          where {-# LINE 247 "Vobs.fhs" #-}
                i = interpolate fract
 
{-# LINE 249 "Vobs.fhs" #-}
instance Interpolate Matrix where
        {-# LINE 250 "Vobs.fhs" #-}
        interpolate fract (Matrix u v w x y z) (Matrix u' v' w' x' y' z')
          = Matrix (i u u') (i v v') (i w w') (i x x') (i y y') (i z z')
          where {-# LINE 252 "Vobs.fhs" #-}
                i = interpolate fract
 
{-# LINE 254 "Vobs.fhs" #-}
interpolateScene :: (Ord k) => Double -> Op (Scene k)
{-# LINE 255 "Vobs.fhs" #-}
interpolateScene fract sc1 sc2
  = fromList
      [(key, liftM2 f (sc1 Map.! key) (sc2 Map.! key)) |
       key <- interpKeys]
  where {-# LINE 258 "Vobs.fhs" #-}
        interpKeys = intersect (keys sc1) (keys sc2)
        {-# LINE 259 "Vobs.fhs" #-}
        f (m1, (w1, h1)) (m2, (w2, h2)) = (i m1 m2, (i w1 w2, i h1 h2))
        {-# LINE 260 "Vobs.fhs" #-}
        i x y = interpolate fract x y
 
{-# LINE 263 "Vobs.fhs" #-}
isInterpUseful :: (Ord k) => Scene k -> Scene k -> Bool
{-# LINE 264 "Vobs.fhs" #-}
isInterpUseful sc1 sc2
  = not $
      all same [(sc1 Map.! key, sc2 Map.! key) | key <- interpKeys]
  where {-# LINE 266 "Vobs.fhs" #-}
        same (a, b)
          = all (\ d -> abs d < 5) $ zipWith (-) (values a) (values b)
        {-# LINE 267 "Vobs.fhs" #-}
        values (Just (Matrix a b c d e f, (w, h)))
          = [a, b, c, d, e, f, w, h]
        {-# LINE 268 "Vobs.fhs" #-}
        values (Nothing) = error "shouldn't happen"
        {-# LINE 269 "Vobs.fhs" #-}
        interpKeys = intersect (getKeys sc1) (getKeys sc2)
        {-# LINE 270 "Vobs.fhs" #-}
        getKeys sc = [k | k <- keys sc, isJust (sc Map.! k)]
 
{-# LINE 273 "Vobs.fhs" #-}
timeDbg :: (MonadIO m) => String -> Endo (m ())
{-# LINE 274 "Vobs.fhs" #-}
timeDbg s act
  | False =
    do out s
       act
       out s
  | otherwise = act
  where {-# LINE 276 "Vobs.fhs" #-}
        out t
          = liftIO $
              do time <- System.Time.getClockTime
                 putStrLn $ s ++ " " ++ t ++ "\t" ++ show time
 
{-# LINE 280 "Vobs.fhs" #-}
linearFract :: Double -> (Double, Bool)
{-# LINE 281 "Vobs.fhs" #-}
linearFract x = if (x < 1) then (x, True) else (1, False)
 
{-# LINE 283 "Vobs.fhs" #-}
bounceFract :: Double -> (Double, Bool)
{-# LINE 284 "Vobs.fhs" #-}
bounceFract x = (y, cont)
  where {-# LINE 285 "Vobs.fhs" #-}
        x' = x + x * x
        {-# LINE 286 "Vobs.fhs" #-}
        y = 1 - cos (2 * pi * n * x') * exp (- x' * r)
        {-# LINE 287 "Vobs.fhs" #-}
        cont = - (x + x * x) * r >= log 2.0e-2
        {-# LINE 288 "Vobs.fhs" #-}
        (n, r) = (0.4, 2)
 
{-# LINE 292 "Vobs.fhs" #-}
type Anim a = Time -> (Scene a, Bool)
 
{-# LINE 294 "Vobs.fhs" #-}
interpAnim ::
             (Ord a) => Time -> TimeDiff -> Scene a -> Scene a -> Anim a
{-# LINE 295 "Vobs.fhs" #-}
interpAnim startTime interpDuration sc1 sc2 time
  = if continue then (interpolateScene fract sc1 sc2, True) else
      (sc2, False)
  where {-# LINE 297 "Vobs.fhs" #-}
        (fract, continue)
          = bounceFract ((time - startTime) / interpDuration)
{-# LINE 299 "Vobs.fhs" #-}
noAnim scene = const (scene, False)
 
{-# LINE 302 "Vobs.fhs" #-}
vobCanvas ::
            (Ord b) =>
            IORef a ->
              View a b ->
                Handler EventM a ->
                  Handler c a ->
                    (a -> IO ()) ->
                      Color -> TimeDiff -> IO (DrawingArea, Bool -> IO (), c -> IO Bool)
{-# LINE 305 "Vobs.fhs" #-}
vobCanvas stateRef view eventHandler actionHandler stateChanged
  bgColor animTime
  = do canvas <- drawingAreaNew
       widgetSetCanFocus canvas True
       animRef <- newIORef (mempty, Map.empty, noAnim Map.empty)
       let {-# LINE 313 "Vobs.fhs" #-}
           getWH
             = do (cw, ch) <- drawingAreaGetSize canvas
                  return (fromIntegral cw, fromIntegral ch)
           {-# LINE 316 "Vobs.fhs" #-}
           getVob
             = do state <- readIORef stateRef
                  return $ useFadeColor paint & view state
           {-# LINE 319 "Vobs.fhs" #-}
           getRenderContext sc
             = do size <- getWH
                  return $
                    RenderContext{rcScene = sc, rcRect = (Matrix.identity, size),
                                  rcFade = 1, rcFgColor = black, rcBgColor = white,
                                  rcFadeColor = bgColor}
           {-# LINE 324 "Vobs.fhs" #-}
           updateAnim interpolate'
             = do rec
                     (vob, scene, _) <- readIORef animRef
                     vob' <- getVob
                     rc' <- getRenderContext scene'
                     let {-# LINE 328 "Vobs.fhs" #-}
                         scene' = vobScene vob' rc'
                     time <- scene' `seq` getTime
                     let {-# LINE 330 "Vobs.fhs" #-}
                         anim'
                           = if interpolate' && isInterpUseful scene scene' then
                               interpAnim time animTime scene scene' else noAnim scene'
                     writeIORef animRef (vob', scene', anim')
                  widgetQueueDraw canvas
           {-# LINE 338 "Vobs.fhs" #-}
           handle handler event
             = do state <- readIORef stateRef
                  (state', interpolate', handled) <- runHandler handler state event
                  when handled $
                    do writeIORef stateRef state'
                       stateChanged state'
                       updateAnim interpolate'
                  return handled
           {-# LINE 350 "Vobs.fhs" #-}
           handleEvent = handle eventHandler
           {-# LINE 352 "Vobs.fhs" #-}
           handleAction = handle actionHandler
       onRealize canvas $
         do rec
               vob <- getVob
               rc <- getRenderContext scene
               let {-# LINE 357 "Vobs.fhs" #-}
                   scene = vobScene vob rc
               writeIORef animRef (vob, scene, noAnim scene)
            return ()
       onConfigure canvas $
         \ _event ->
           do updateAnim False
              return True
       onKeyPress canvas $
         \ event ->
           do let {-# LINE 364 "Vobs.fhs" #-}
                  Key{eventModifier = mods, eventKeyName = key, eventKeyChar = char}
                    = event
              putStrLn $ show mods ++ " " ++ key ++ " (" ++ show char ++ ")"
              handleEvent event
       onButtonPress canvas $
         \ (Button{}) ->
           do widgetGrabFocus canvas
              return True
       onExpose canvas $
         \ (Expose{}) ->
           do drawable <- drawingAreaGetDrawWindow canvas
              (vob, _, anim) <- readIORef animRef
              time <- getTime
              let {-# LINE 377 "Vobs.fhs" #-}
                  (scene, rerender) = anim time
              rc <- getRenderContext scene
              renderWithDrawable drawable $ timeDbg "redraw" $ renderVob vob rc
              if rerender then widgetQueueDraw canvas else return ()
              return True
       return (canvas, updateAnim, handleAction)