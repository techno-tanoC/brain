module Brain
  (
    parse
  , module Brain.Type
  , module Brain.Parser
  , module Brain.Eval
  , module Brain.Gochiusa
  ) where

import Text.Parsec (parse)

import Brain.Type
import Brain.Parser
import Brain.Eval
import Brain.Gochiusa
