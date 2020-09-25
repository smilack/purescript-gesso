module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Array (range)
import Data.Enum (fromEnum)
import Data.Foldable (traverse_, sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time as Time
import Debug.Trace (trace, traceM, spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Now (nowTime)
import Effect.Ref as Ref
import Gesso.Application as App
import Gesso.AppM (AppM, runAppM, getState, putState, class ManageState)
import Gesso.AspectRatio as AR
import Gesso.Canvas as GC
import Gesso.Dimensions as Dims
import Gesso.Env (Env)
import Gesso.Time as T
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
-- import Halogen.Aff (runHalogenAff, awaitLoad, selectElement)
import Halogen.HTML as HH
import Halogen.Query.HalogenM as HalM
import Halogen.VDom.Driver (runUI)
import Math (cos, sin)

-- import Web.DOM.ParentNode (QuerySelector(..))
-- runUI :: 
--   forall query input output. 
--   Component HTML query input output Aff -> 
--   input -> 
--   HTMLElement -> 
--   Aff (HalogenIO query output Aff)
-- hoist ::
--   forall surface query input output m Aff.
--   Bifunctor surface => Functor Aff =>
--   (m ~> Aff) ->
--   Component surface query input output m -> 
--   Component surface query input output Aff
main :: Effect Unit
main =
  runHalogenAff do
    -- awaitLoad
    -- mdiv <- selectElement $ QuerySelector "#app"
    body <- awaitBody
    appState <- H.liftEffect $ Ref.new initialState
    let
      environment :: Env AppState
      environment = { appState }

      rootComponent :: H.Component HH.HTML (GC.Query AppState) (GC.Input AppState) (GC.Output AppState) Aff
      rootComponent = H.hoist (runAppM environment) GC.component
    -- mio <- traverse (runUI rootComponent init) mdiv
    io <- runUI rootComponent init body
    -- ( (runAppM environment :: AppM AppState ~> Aff)
    -- $ 
    (io.subscribe :: CR.Consumer (GC.Output AppState) Aff Unit -> Aff Unit)
      $ (CR.consumer :: (GC.Output AppState -> Aff (Maybe Unit)) -> CR.Consumer (GC.Output AppState) Aff Unit)
      $ (subCallback (runAppM environment) io.query :: GC.Output AppState -> Aff (Maybe Unit))
    pure unit
  where
  --
  -- subscribe :: Control.Coroutine.Consumer (GC.Output AppState) (AppM AppState) Unit -> AppM AppState Unit
  -- consumer :: forall r. (GC.Output AppState -> AppM AppState (Maybe r)) -> Consumer (GC.Output AppState) (AppM AppState) r
  -- consumer :: H.HalogenIO (GC.Query AppState) (GC.Output AppState) (AppM AppState) -> CR.Consumer (GC.Output AppState) (AppM AppState) Unit
  -- consumer = CR.consumer <<< subCallback
  subCallback :: (AppM AppState ~> Aff) -> (GC.Query AppState Unit -> Aff (Maybe Unit)) -> GC.Output AppState -> Aff (Maybe Unit)
  subCallback nat query out =
    nat
      $ case out of
          GC.StateUpdated appState' -> do
            (putState appState' :: AppM AppState Unit)
            pure Nothing
          GC.MouseMove p -> do
            let
              x = Dims.getX p

              y = Dims.getY p
            appState <- (getState :: AppM AppState AppState)
            let
              appState' = appState { mouse = Just { x, y } }
            (putState appState' :: AppM AppState Unit)
            _ <-
              (liftAff :: Aff (Maybe Unit) -> AppM AppState (Maybe Unit))
                $ (query :: GC.Query AppState Unit -> Aff (Maybe Unit))
                $ (H.tell :: (Unit -> GC.Query AppState Unit) -> GC.Query AppState Unit)
                $ (GC.UpdateAppState :: AppState -> Unit -> GC.Query AppState Unit)
                $ (appState' :: AppState)
            pure Nothing

type AppState
  = { color :: String
    , mouse :: Maybe { x :: Number, y :: Number }
    }

initialState :: AppState
initialState = { color: "blue", mouse: Nothing }

init :: GC.Input AppState
init =
  GC.Input
    { name: "test-app"
    , appState: initialState
    , app:
        App.mkApplication
          $ App.defaultApp
              -- { window = App.stretch 
              { window = App.fullscreen
              -- { window = App.fixed (Dims.fromWidthAndHeight { width: 100.0, height: 100.0 })
              , render = Just renderFn
              }
    , viewBox:
        Dims.fromPointAndSize
          Dims.origin
          (Dims.fromHeightAndRatio { height: 360.0, aspectRatio: AR.w16h9 })
    }

renderFn :: App.RenderStyle AppState
renderFn = App.continuous render
  where
  render :: AppState -> T.Delta -> Dims.Scaler -> Canvas.Context2D -> Effect Unit
  render { color, mouse } { now } { x_, y_, w_, h_, screen, toVb } context = do
    Canvas.setLineWidth context 3.0
    Canvas.setFillStyle context "#FFDDDD"
    Canvas.setStrokeStyle context "#00FF00"
    Canvas.fillRect context { x: screen.x, y: screen.y, width: screen.width, height: screen.height }
    Canvas.strokeRect context { x: screen.x, y: screen.y, width: screen.width, height: screen.height }
    Canvas.setFillStyle context "#DDFFDD"
    Canvas.setStrokeStyle context "#FF0000"
    Canvas.fillRect context { x: x_ 0.0, y: y_ 0.0, width: w_ 640.0, height: h_ 360.0 }
    Canvas.strokeRect context { x: x_ 0.0, y: y_ 0.0, width: w_ 640.0, height: h_ 360.0 }
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: x_ 100.0, y: y_ 50.0, width: w_ 200.0, height: h_ 25.0 }
    Canvas.fillRect context
      { x: x_ $ 150.0 + 50.0 * cos t
      , y: y_ $ 150.0 + 50.0 * sin t
      , width: w_ 10.0
      , height: h_ 10.0
      }
    traverse_ (Canvas.fillRect context) (mouseRect <$> mouse)
    time <- nowTime
    let
      clock = { x: x_ 300.0, y: y_ 175.0 }

      hour = toNumber $ (_ `mod` 12) $ (_ + 7) $ fromEnum $ Time.hour time

      hrAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 12.0) $ hour

      hr = { x: clock.x + w_ (cos hrAng * 40.0), y: clock.y + h_ (sin hrAng * 40.0) }

      minute = toNumber $ fromEnum $ Time.minute time

      mnAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 60.0) $ minute

      min = { x: clock.x + w_ (cos mnAng * 60.0), y: clock.y + h_ (sin mnAng * 60.0) }

      second = toNumber $ fromEnum $ Time.second time

      scAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 60.0) $ second

      sec = { x: clock.x + w_ (cos scAng * 60.0), y: clock.y + h_ (sin scAng * 60.0) }
    Canvas.setStrokeStyle context "black"
    Canvas.fillText context (show hour) (x_ 30.0) (y_ 30.0)
    Canvas.fillText context (show minute) (x_ 30.0) (y_ 40.0)
    Canvas.fillText context (show second) (x_ 30.0) (y_ 50.0)
    Canvas.strokePath context do
      Canvas.arc context { x: clock.x, y: clock.y, start: 0.0, end: 6.283, radius: w_ 75.0 }
    Canvas.strokePath context do
      Canvas.moveTo context clock.x clock.y
      Canvas.lineTo context hr.x hr.y
    Canvas.strokePath context do
      Canvas.moveTo context clock.x clock.y
      Canvas.lineTo context min.x min.y
    Canvas.setStrokeStyle context "red"
    Canvas.strokePath context do
      Canvas.moveTo context clock.x clock.y
      Canvas.lineTo context sec.x sec.y
    sequence_ $ map drawNum $ range 1 12
    where
    mouseRect { x, y } = { x: toVb.x_ x, y: toVb.y_ y, width: w_ 30.0, height: h_ 30.0 }

    drawNum i = do
      let
        ang = (_ - 1.571) $ (_ * 6.283) $ (_ / 12.0) $ toNumber $ (_ `mod` 12) $ i
      Canvas.fillText context (show i) (x_ $ 299.0 + 70.0 * cos ang) (y_ $ 177.0 + 70.0 * sin ang)

    t = (unwrap now) * 6.28 / 1000.0 / 2.0
