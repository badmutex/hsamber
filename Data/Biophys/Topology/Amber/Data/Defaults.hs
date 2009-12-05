module Data.Biophys.Topology.Amber.Data.Defaults where

import Data.Biophys.Topology.Amber.Types

data Defaults = Defaults { non_bonded_function_type :: Int
                         , combination_rule         :: Int
                         , generate_pairs           :: Bool
                         , fudgeLJ                  :: Double
                         , fudgeQQ                  :: Double
                         } deriving (Eq, Ord, Show)
