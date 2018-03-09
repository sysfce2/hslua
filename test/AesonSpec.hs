{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   :  © 2017–2018 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (*>))
#endif
import Control.Monad (forM_, when)
import Data.AEq ((~==))
import Data.HashMap.Lazy (HashMap)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Vector (Vector)
import Foreign.Lua
import Foreign.Lua.Aeson (registerNull)
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "Value component" $ do
    describe "Scientific" $ do
      it "can be converted to a lua number" $ property $
        \x -> assert =<< luaTest "type(x) == 'number'" [("x", x::Scientific)]
      it "can be round-tripped through the stack with numbers of double precision" $
        property $ \x -> assertRoundtripEqual (luaNumberToScientific (LuaNumber x))
      it "can be round-tripped through the stack and stays approximately equal" $
        property $ \x -> assertRoundtripApprox (x :: Scientific)
    describe "Text" $ do
      it "can be converted to a lua string" $ property $
        \x -> assert =<< luaTest "type(x) == 'string'" [("x", x::Text)]
      it "can be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Text)
    describe "Vector" $ do
      it "is converted to a lua table" $ property $
        \x -> assert =<< luaTest "type(x) == 'table'" [("x", x::Vector Bool)]
      it "can contain Bools and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Vector Bool)
      it "can contain Texts and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Vector Text)
      it "can contain Vector of Bools and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::(Vector (Vector Bool)))
    describe "HashMap" $ do
      it "is converted to a lua table" $ property $
        \x -> assert =<< luaTest "type(x) == 'table'" [("x", x::HashMap Text Bool)]
      it "can be round-tripped through the stack with Text keys and Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text Bool)
      it "can be round-tripped through the stack with Text keys and Vector Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text (Vector Bool))
    describe "Value" $ do
      it "can be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Aeson.Value)

assertRoundtripApprox :: Scientific -> IO ()
assertRoundtripApprox x = do
  y <- roundtrip x
  let xdouble = toRealFloat x :: Double
  let ydouble = toRealFloat y :: Double
  assert (xdouble ~== ydouble)

assertRoundtripEqual :: (Show a, Eq a, ToLuaStack a, FromLuaStack a) => a -> IO ()
assertRoundtripEqual x = do
  y <- roundtrip x
  assert (x == y)

roundtrip :: (ToLuaStack a, FromLuaStack a) => a -> IO a
roundtrip x = runLua $ do
  registerNull
  push x
  size <- gettop
  when (size /= 1) $
    error ("not the right amount of elements on the stack: " ++ show size)
  peek (-1)

luaTest :: ToLuaStack a => String -> [(String, a)] -> IO Bool
luaTest luaTestCode xs = runLua $ do
  openlibs
  registerNull
  forM_ xs $ \(var, value) ->
    push value *> setglobal var
  let luaScript = "function run() return (" ++ luaTestCode ++ ") end"
  _ <- loadstring luaScript
  call 0 0
  callFunc "run"

luaNumberToScientific :: LuaNumber -> Scientific
luaNumberToScientific = fromFloatDigits . (realToFrac :: LuaNumber -> Double)

instance Arbitrary Aeson.Value where
  arbitrary = arbitraryValue 7

arbitraryValue :: Int -> Gen Aeson.Value
arbitraryValue size = frequency $
    [ (1, return Aeson.Null)
    , (4, Aeson.Bool <$> arbitrary)
    -- FIXME: this is cheating: we don't draw numbers from the whole possible
    -- range, but only fro the range of nubers that can pass through the lua
    -- stack without rounding errors. Good enough for now, but we still might
    -- want to do better in the future.
    , (4, Aeson.Number . luaNumberToScientific . LuaNumber <$> arbitrary)
    , (4, Aeson.String <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Array <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Object <$> arbitrary)
    ]
