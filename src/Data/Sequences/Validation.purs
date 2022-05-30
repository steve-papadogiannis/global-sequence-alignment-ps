module Data.Sequences.Validation where

import Prelude
import Data.Either (Either)
import Data.String (length)
import Data.Traversable (traverse)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (andThen, V, invalid, toEither)
import Data.Sequences (SequenceType, SequenceType(..), SequencePair, SequenceRec, sequencePair, mkSequence)

newtype ValidationError
  = ValidationError String

type Errors
  = Array ValidationError

nonEmpty :: String -> String -> V Errors String
nonEmpty field "" = invalid [ ValidationError $ "Field '" <> field <> "' cannot be empty" ]

nonEmpty _ value = pure value

dNARegex :: Regex
dNARegex = unsafeRegex "^[ATCG]+$" noFlags

rNARegex :: Regex
rNARegex = unsafeRegex "^[AUCG]+$" noFlags

proteinRegex :: Regex
proteinRegex = unsafeRegex "^[ARNDCQEGHILKMFPSTWYV]+$" noFlags

matches :: String -> Regex -> String -> V Errors String
matches _ regex value
  | test regex value = pure value

matches sequenceType _ _ = invalid [ ValidationError $ "Sequence did not match the required format for type: " <> sequenceType ]

validateSequenceRec :: String -> SequenceRec -> V Errors SequenceRec
validateSequenceRec field sr =
  mkSequence <$> pure sr."type"
    <*> let
        regex = case sr."type" of
          Protein -> proteinRegex
          DNA -> dNARegex
          RNA -> rNARegex
      in
        nonEmpty field sr.sequence `andThen` matches (show sr."type") regex

validateSecondSequence :: SequencePair -> V Errors SequencePair
validateSecondSequence sp =
  sequencePair <$> pure sp.firstSequence
    <*> validateSequenceRec "Second Sequence" sp.secondSequence

validateFirstSequence :: SequencePair -> V Errors SequencePair
validateFirstSequence sp =
  sequencePair <$> validateSequenceRec "First Sequence" sp.firstSequence
    <*> pure sp.secondSequence

validateFirstSequence' :: SequencePair -> Either Errors SequencePair
validateFirstSequence' sp = toEither $ validateFirstSequence sp

validateSecondSequence' :: SequencePair -> Either Errors SequencePair
validateSecondSequence' sp = toEither $ validateSecondSequence sp
