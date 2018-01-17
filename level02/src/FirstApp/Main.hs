{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           rawPathInfo, strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404,
                                           StdMethod(POST,GET))

import           Network.HTTP.Types.Method (parseMethod)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.List                (intersperse)

import qualified Data.Text                as T
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           FirstApp.Types           (ContentType(PlainText),
                                           Error(EmptyTopic, EmptyCommentText, InvalidRequest),
                                           RqType(AddRq, ListRq, ViewRq),
                                           getCommentText, getTopic,
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType =
  responseLBS status [("ContentType", renderContentType contentType)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  AddRq <$> mkTopic topic <*> (mkCommentText . decodeUtf8 . LBS.toStrict $ comment)

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t =
  ViewRq <$> mkTopic t

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic       = resp400 PlainText "Empty topic"
mkErrorResponse EmptyCommentText = resp400 PlainText "Empty comment text"
mkErrorResponse (InvalidRequest method path) = resp400 PlainText
    (LBS.fromStrict . mconcat . intersperse "\n" $ ["Invalid request", method, path])

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (parseMethod(requestMethod r), pathInfo r) of
    (Right POST, [topic, "add"])  -> mkAddRequest topic <$> strictRequestBody r
    (Right GET,  [topic, "view"]) -> pure . mkViewRequest $ topic
    (Right GET,  ["list"])        -> pure mkListRequest
    _                             -> pure . Left $ InvalidRequest (requestMethod r) (rawPathInfo r)

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq topic comment) =
    pure $ resp200 PlainText
            (textToLbs (T.unlines ["Add:", getTopic topic, getCommentText comment]))
handleRequest (ViewRq topic) =
    pure $ resp200 PlainText
            (textToLbs (T.unlines ["View:", getTopic topic]))
handleRequest ListRq =
    pure $ resp200 PlainText "List"

textToLbs :: Text -> LBS.ByteString
textToLbs = LBS.fromStrict . encodeUtf8

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app r handler =
  (>>= handleRequest) <$> mkRequest r
   >>= handler . either mkErrorResponse id

{- Using do-notation:

app r handler = do
    req <- mkRequest r
    let resp = req >>= handleRequest
    handler . either mkErrorResponse id $ resp

-}
runApp :: IO ()
runApp = run 3000 app
