module Data.Biophys.Topology.Amber.Parser.Default where

import Data.Biophys.Topology.Amber.Types
import Data.Biophys.Topology.Amber.Data.Defaults
import Data.Biophys.Topology.Amber.Parser.Util

import Text.ParserCombinators.Parsec hiding (newline)


defaults' :: Parser ()
defaults' = do char '['          ; spaces
               string "defaults" ; spaces
               char ']'
               space `manyTill` eol
               return ()

gen_pairs "yes" = True
gen_pairs "no"  = False

defaults'' :: Parser Defaults
defaults'' = do nbfunc   <- positiveInt                                           ; spaces
                combrule <- positiveInt                                           ; spaces
                genpairs <- gen_pairs `fmap` (try (string "yes") <|> string "no") ; spaces
                fLJ      <- decimal                                               ; spaces
                fQQ      <- decimal
                space `manyTill` eol

                return Defaults { non_bonded_function_type = nbfunc
                                , combination_rule         = combrule
                                , generate_pairs           = genpairs
                                , fudgeLJ                  = fLJ
                                , fudgeQQ                  = fQQ
                                }

defaults :: Parser Defaults
defaults = do defaults'
              defaults''

t = "[ defaults ]\n1               2               yes             0.5     0.8333\n"

p = parse defaults [] t