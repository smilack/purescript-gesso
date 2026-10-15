module Gesso.Canvas.Context.Context2D
  ( Context2DAttributes
  , Context2DColorSpace
  , Context2DColorType
  , sRGB
  , displayP3
  , unorm8
  , float16
  , defaultContext2DAttributes
  ) where

-- https://html.spec.whatwg.org/multipage/canvas.html#canvasrenderingcontext2dsettings
defaultContext2DAttributes :: Context2DAttributes
defaultContext2DAttributes =
  { alpha: true
  , colorSpace: sRGB
  , colorType: unorm8
  , desynchronized: false
  , willReadFrequently: false
  }

type Context2DAttributes =
  { alpha :: Boolean
  , colorSpace :: Context2DColorSpace
  , colorType :: Context2DColorType
  , desynchronized :: Boolean
  , willReadFrequently :: Boolean
  }

newtype Context2DColorSpace = Context2DColorSpace String

sRGB :: Context2DColorSpace
sRGB = Context2DColorSpace "srgb"

displayP3 :: Context2DColorSpace
displayP3 = Context2DColorSpace "display-p3"

newtype Context2DColorType = Context2DColorType String

unorm8 :: Context2DColorType
unorm8 = Context2DColorType "unorm8"

float16 :: Context2DColorType
float16 = Context2DColorType "float16"
