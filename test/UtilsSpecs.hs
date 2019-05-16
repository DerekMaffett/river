module UtilsSpecs where

import           Test.Hspec
import           Data.GraphQL.AST
import           Data.GraphQL.Encoder
import qualified Utils
import           Data.Aeson

shouldBeObject object (Object expectedObject) = shouldBe object expectedObject
testSetAtPath pathSegments (Object object) content =
    Utils.setAtPath pathSegments object content

spec :: Spec
spec = do
    describe "Utils" $ do
        describe "setAtPath" $ do
            it "should set a single field on an empty object" $ do
                shouldBeObject
                    (testSetAtPath ["field"]
                                   (object [])
                                   (Utils.MkWriteable (1 :: Int))
                    )
                    (object ["field" .= (1 :: Int)])
            it "should set additional fields on a populated object" $ do
                shouldBeObject
                    (testSetAtPath ["field2"]
                                   (object ["field1" .= (1 :: Int)])
                                   (Utils.MkWriteable (2 :: Int))
                    )
                    (object ["field1" .= (1 :: Int), "field2" .= (2 :: Int)])
            it "should set nested fields on an empty object" $ do
                shouldBeObject
                    (testSetAtPath ["settings", "id"]
                                   (object [])
                                   (Utils.MkWriteable (2 :: Int))
                    )
                    (object ["settings" .= object ["id" .= (2 :: Int)]])
            it "should set nested fields on a populated object" $ do
                shouldBeObject
                    (testSetAtPath
                        ["settings", "id"]
                        (object
                            ["settings" .= object ["name" .= ("Tom" :: String)]]
                        )
                        (Utils.MkWriteable (2 :: Int))
                    )
                    (object
                        [ "settings" .= object
                              ["id" .= (2 :: Int), "name" .= ("Tom" :: String)]
                        ]
                    )
            it "should create extra nesting as necessary without other changes"
                $ do
                      shouldBeObject
                          (testSetAtPath
                              ["settings", "country", "name"]
                              (object
                                  [ "settings"
                                        .= object ["name" .= ("Tom" :: String)]
                                  ]
                              )
                              (Utils.MkWriteable ("Sverige" :: String))
                          )
                          (object
                              [ "settings" .= object
                                    [ "country" .= object
                                        ["name" .= ("Sverige" :: String)]
                                    , "name" .= ("Tom" :: String)
                                    ]
                              ]
                          )
