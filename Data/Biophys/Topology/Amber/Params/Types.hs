{-# LANGUAGE
  EmptyDataDecls
  #-}

module Data.Biophys.Topology.Amber.Params.Types where

data Default = Default { non_bonded_function_type :: Int
                       , combination_rule         :: Int
                       , generate_pairs           :: Bool
                       , fudgeLJ                  :: Double
                       , fudgeQQ                  :: Double
                       } deriving (Eq, Ord, Show)


newtype AtomType = AtomType String -- ^ i.e. amber99_17 is H bonded to nitrogen atoms
newtype ResidueName = ResidueName String -- ^ i.e. LEU
newtype AtomName = AtomName String -- ^ i.e. N, H1, CE, etc

-- -------------------- Units -------------------- --
data AMU
data Electron
data KiloJoulesPerMol
data NanoMeters

data Mass a = Mass Double
data Charge a = Charge Double
data Epsilon a = Epsilon Double
data Sigma a = Sigma Double
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
                   }





