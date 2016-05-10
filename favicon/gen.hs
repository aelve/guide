{-# LANGUAGE
FlexibleContexts,
GADTs
  #-}

-- run as: ./gen -o favicon.png -w 32 -h 32

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

main = defaultMain icon

icon :: Diagram B
icon = lines_ `atop` canvas_
  where
    line_  = rect 14 3 # fc white . lw none
    dot_   = circle 2 # fc white . lw none
    line1  = alignL $ hsep 2 [line_, dot_]
    line2  = alignL $ hsep 2 [dot_, line_]
    lines_ = centerXY $ vsep 3 [line1, line2, line1]
    canvas_ = roundedRect 32 32 4 # fc (sRGB24read "7868FF") . lw none
