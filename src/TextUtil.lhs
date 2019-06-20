% Pplmonad | TextUtil
% 
% October 10, 2018

We define a module of simple text utility functions. They manipulate almost nothing but `Text` values, so for brevity we mask some Prelude terms with their counterparts in Data.Text.

\begin{code}
module TextUtil (padInt, padIntWith, wrap) where

import Prelude hiding (length, replicate, unwords, words)
import Data.Text hiding (map)
\end{code}

* * *

PAD INT
======

`padInt` `n` produces a text representation of an integer using no fewer than `n` decimal digits. It pads extra space with zeroes to the left of the other digits.

\begin{code}
-- | Produce a the decimal representation of an integer padded with zeroes
padInt :: Int -> Int -> Text
padInt = padIntWith '0'
\end{code}

* * *

PAD INT WITH
======

`padIntWith` `p` `n` produces a text representation of an integer using no fewer than `n` characters. It pads extra space with copies of `p` to the left of the other digits.

\begin{code}
-- | Produce a the decimal representation of an integer padded with copies of a character
padIntWith :: Char -> Int -> Int -> Text
padIntWith p n x = append zeroes rep
    where  zeroes  = replicate (n - length rep) (singleton p)
           rep     = pack (show x)
\end{code}


* * *

WRAP 
====

`wrap` takes a single text and produces a list of texts, each no longer than a given length. These represent text broken at word boundaries into rows that fit in a printed column.

\begin{code}
-- | Break text at word boundaries into rows no longer than a given length
wrap :: Int -> Text -> [Text]
wrap len = map unwords . rows len . words
\end{code}

Concatenating these texts with spaces between them will reproduce the original text, so $\forall$ `t` $\in$ `Text`, $\forall$ `n` $\in$ `Int`,

    t = unwords (wrap n t)

`wrap` mostly just transforms text to words and back. It does interesting work only by applying `rows`, a function that takes a list of words and recursively builds a list of lists of words. Each of those lists represents an output row.

\begin{code}
rows :: Int -> [Text] -> [[Text]]
rows  _    []     =  []
rows  len  words  =  l : rows len rest
    where (l, rest) = row len [] words
\end{code}

`rows` computes each output row by applying `row`. That function iteratively builds a list of words as a candidate to represent the row. In each iteration it checks whether adding the next remaining word would make the candidate too long. If so, it stops and produces the current candidate as well as the remaining words that must go in other rows. Otherwise it repeats with new data. Its new candidate extends the current one by the next word, and its new definition of "too long" accounts for the lengths of both the word and a preceding space.

\begin{code}
row :: Int -> [Text] -> [Text] -> ([Text], [Text])
row  _    current  []    = (current, [])
row  len  current  rem@(word:words)
    | length word > len  = (current, rem)
    | otherwise          = row (len - length word - 1) (current ++ [word]) words
\end{code}
