% Pplmonad | Output.Window
%
% October 11, 2018

We define a module for drawing the GUI-style windows that frame things like menus and dialogue text. It extends the tile-drawing facilities in the `Output` module.

\begin{code}
module Output.Window where

import Output
import TileName
\end{code}

This module defines a single function `drawWindow`. It applies `drawTiledBg` to the given window position and dimensions (in numbers of tiles), as well as a list of tiles that together form the background and border of the window.

\begin{code}
-- | Draw the background and border of a window with the given position and dimensions (in numbers of tiles)
drawWindow p (w, h) = drawTiledBg p (w, h) [(t, Original) | t <- tiles]
  where
    tiles  = rowT ++ concat (replicate (h - 2) rowM) ++ rowB
    rowT   = TextBoxTopLeft     : (replicate (w - 2) TextBoxTopMiddle)     ++ [TextBoxTopRight]
    rowM   = TextBoxLeft        : (replicate (w - 2) SolidWhite)           ++ [TextBoxRight]
    rowB   = TextBoxBottomLeft  : (replicate (w - 2) TextBoxBottomMiddle)  ++ [TextBoxBottomRight]
\end{code}

The list of tiles comprises a top row and bottom row separated by a concatenated list of middle rows. Each of these rows comprise a left tile and a right tile separated by a list of middle tiles. Every tile appears in its `Original` orientation.
