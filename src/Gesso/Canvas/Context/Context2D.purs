-- | Bindings for `Context2D` options not provided by `Graphics.Canvas`.
module Gesso.Canvas.Context.Context2D
  ( CanvasRenderingContext2DSettings
  , PredefinedColorSpace
  , CanvasColorType
  , sRGB
  , displayP3
  , unorm8
  , float16
  , defaultContext2DSettings
  ) where

-- | Optional parameters for the `HTMLCanvasElement.getContext()` method when
-- | getting a `Context2D`:
-- |
-- | - `alpha`: Whether the canvas can be transparent
-- | - `colorSpace`: sRGB or display-p3 color space
-- | - `colorType`: Unsigned 8-bit integer or 16-bit float color values
-- | - `desynchronized`: Whether the canvas rendering should be synchronized
-- |   with the event loop
-- | - `willReadFrequently`: Force software rendering if the canvas image data
-- |   will be read frequently
-- |
-- | See
-- | [HTMLCanvasElement: getContext() methodi (MDN)](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext)
-- | and
-- | [HTML Living Standard § 4.12.5.1.2 The canvas settings (WHATWG)](https://html.spec.whatwg.org/multipage/canvas.html#the-canvas-settings)
type CanvasRenderingContext2DSettings =
  { alpha :: Boolean
  , colorSpace :: PredefinedColorSpace
  , colorType :: CanvasColorType
  , desynchronized :: Boolean
  , willReadFrequently :: Boolean
  }

-- | The default Context2D settings:
-- |
-- | ```purescript
-- | { alpha: true
-- | , colorSpace: sRGB
-- | , colorType: unorm8
-- | , desynchronized: false
-- | , willReadFrequently: false
-- | }
-- | ```
defaultContext2DSettings :: CanvasRenderingContext2DSettings
defaultContext2DSettings =
  { alpha: true
  , colorSpace: sRGB
  , colorType: unorm8
  , desynchronized: false
  , willReadFrequently: false
  }

-- | `PredefinedColorSpace` enum from
-- | [HTML Living Standard § 4.12.5.4 Color spaces and color space conversion (WHATWG)](https://html.spec.whatwg.org/multipage/canvas.html#predefinedcolorspace)
newtype PredefinedColorSpace = PredefinedColorSpace String

-- | `"srgb"`
sRGB :: PredefinedColorSpace
sRGB = PredefinedColorSpace "srgb"

-- | `"display-p3"`
displayP3 :: PredefinedColorSpace
displayP3 = PredefinedColorSpace "display-p3"

-- | `CanvasColorType` enum from
-- | [HTML Living Standard § 4.12.5.1 The 2D rendering context (WHATWG)](https://html.spec.whatwg.org/multipage/canvas.html#canvascolortype)
newtype CanvasColorType = CanvasColorType String

-- | `"unorm8"`
unorm8 :: CanvasColorType
unorm8 = CanvasColorType "unorm8"

-- | `"float16"`
float16 :: CanvasColorType
float16 = CanvasColorType "float16"
