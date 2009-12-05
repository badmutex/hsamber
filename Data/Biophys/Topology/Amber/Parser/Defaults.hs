module Data.Biophys.Topology.Amber.Parser.Default (defaults) where

import Data.Biophys.Topology.Amber.Types
import Data.Biophys.Topology.Amber.Data.Defaults
import Data.Biophys.Topology.Amber.Parser.Util

import Text.ParserCombinators.Parsec hiding (newline)


header :: Parser ()
header = do char '['          ; spaces
            string "defaults" ; spaces
            char ']'
            space `manyTill` eol
            return ()

gen_pairs "yes" = True
gen_pairs "no"  = False

values :: Parser Defaults
values = do nbfunc   <- positiveInt                                           ; spaces
            combrule <- positiveInt                                           ; spaces
            genpairs <- gen_pairs `fmap` (try (string "yes") <|> string "no") ; spaces
            fLJ      <- decimal                                               ; spaces
            fQQ      <- decimal
            space `manyTill` (try comment <|> eol)

            return Defaults { non_bonded_function_type = nbfunc
                            , combination_rule         = combrule
                            , generate_pairs           = genpairs
                            , fudgeLJ                  = fLJ
                            , fudgeQQ                  = fQQ
                            }

defaults :: Parser Defaults
defaults = do header
              try comment
              values
