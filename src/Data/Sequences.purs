module Data.Sequences where

import Prelude
import Data.String.Read
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)

data SequenceType
  = Protein
  | DNA
  | RNA

instance readSequenceType :: Read SequenceType where
  read s = case s of
    "DNA" -> pure DNA
    "RNA" -> pure RNA
    "Protein" -> pure Protein
    _ -> Nothing

instance zeroSequenceType :: Zero SequenceType where
    zero = DNA

derive instance genericSequenceType :: Generic SequenceType _

allSequenceTypes :: Array SequenceType
allSequenceTypes = [ DNA, RNA, Protein ]

derive instance eqSequenceType :: Eq SequenceType

instance showSequeneType :: Show SequenceType where
  show = genericShow

type SequenceRec
  = { sequence :: String
    , "type" :: SequenceType
    }

mkSequence :: SequenceType -> String -> SequenceRec
mkSequence t seq = { sequence: seq, "type": t }

type SequencePair
  = { firstSequence :: SequenceRec
    , secondSequence :: SequenceRec
    }

sequencePair :: SequenceRec -> SequenceRec -> SequencePair
sequencePair firstSequence secondSequence = { firstSequence, secondSequence }

exampleSequencePair :: SequencePair
exampleSequencePair = sequencePair (mkSequence DNA "ATCG") (mkSequence DNA "ATCG")
