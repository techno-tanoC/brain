module Brain
  (
    parse
  , module Brain.Type
  , module Brain.Parser
  , module Brain.Eval
  ) where

import Text.Parsec (parse)

import Brain.Type
import Brain.Parser
import Brain.Eval

