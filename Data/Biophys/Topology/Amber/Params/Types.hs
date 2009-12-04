{-# LANGUAGE
  EmptyDataDecls
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , RankNTypes
  #-}

module Data.Biophys.Topology.Amber.Params.Types where

data Default = Default { non_bonded_function_type :: Int
                       , combination_rule         :: Int
                       , generate_pairs           :: Bool
                       , fudgeLJ                  :: Double
                       , fudgeQQ                  :: Double
                       } deriving (Eq, Ord, Show)


-- | i.e. amber99_17 is H bonded to nitrogen atoms
newtype AtomType = AtomType String deriving (Eq, Ord, Show)
-- | i.e. LEU
newtype ResidueName = ResidueName String deriving (Eq, Ord, Show)
-- | i.e. N, H1, CE, etc
newtype AtomName = AtomName String deriving (Eq, Ord, Show)

-- -------------------- Units -------------------- --
class TShow t where tshow :: t a -> String

data AMU
data Electron
data KiloJoulesPerMol
data NanoMeters

tshow' s t = tshow t ++ " " ++ s
instance TShow t => Show (t AMU)              where show = tshow' "amu"
instance TShow t => Show (t Electron)         where show = tshow' "e"
instance TShow t => Show (t KiloJoulesPerMol) where show = tshow' "kJ/mol"
instance TShow t => Show (t NanoMeters)       where show = tshow' "nm"

-- -------------------- Dimensions -------------------- --
data Mass a = Mass Double
data Charge a = Charge Double
data Epsilon a = Epsilon Double
data Sigma a = Sigma Double

instance TShow Mass    where tshow (Mass m)    = "Mass "    ++ show m
instance TShow Charge  where tshow (Charge c)  = "Charge "  ++ show c
instance TShow Epsilon where tshow (Epsilon e) = "Epsilon " ++ show e
instance TShow Sigma   where tshow (Sigma s)   = "Sigma "   ++ show s
-- -------------------- -------------------- --


-- -------------------- molecule definitions -------------------- --
data Atoms = Atoms { atom_number              :: Integer
                   , atom_type                :: AtomType
                   , atom_resno               :: Int
                   , atom_resname             :: ResidueName
                   , atom_name                :: AtomName
                   , atom_charge_group_number :: Int
                   , atom_charge              :: Charge Electron
                   , atom_mass                :: Mass AMU
                   } deriving Show





