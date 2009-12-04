module Data.Biophys.Topology.Amber.Data.Atoms where

import Data.Biophys.Topology.Amber.Types


data Atoms = Atoms { atom_number              :: Integer
                   , atom_type                :: AtomType
                   , atom_resno               :: Int
                   , atom_resname             :: ResidueName
                   , atom_name                :: AtomName
                   , atom_charge_group_number :: Int
                   , atom_charge              :: Charge Electron
                   , atom_mass                :: Mass AMU
                   } deriving Show
