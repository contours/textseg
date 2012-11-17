{-# LANGUAGE TemplateHaskell #-}
import NLP.Tokenizer
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Text as T
import           Data.Text (Text)
import Control.Applicative

prop_placeholder () = True

main = $quickCheckAll

newtype Word = Word { unWord :: Text }
    deriving (Show)
newtype Sentence = Sentence { unSentence :: Text }
    deriving (Show)
newtype Document = Document { unDocument :: Text }
    deriving (Show)

instance Arbitrary Word where
    -- TODO: use a bigger dictionary
    arbitrary = Word <$> do
        elements $ map T.pack $ ["foo", "bar", "baz"]
    
instance Arbitrary Sentence where
    arbitrary = Sentence <$> do
        words <- map unWord <$> arbitrary :: Gen [Text]
        -- TODO: capitalize the first one, sprinkle in some punctuation.
        return $ T.intercalate (T.pack " ") words

instance Arbitrary Document where
    arbitrary = Document <$> do
        sentences <- map unSentence <$> arbitrary
        -- TODO: make paragraphs
        return $ T.intercalate (T.pack " ") sentences

