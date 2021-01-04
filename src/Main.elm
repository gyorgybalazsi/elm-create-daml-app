module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, input, button, ul, li, hr, select, option, h3, p)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import List exposing (append)
import Debug exposing (todo)
import Json.Decode exposing (Error, Decoder, Value, decodeValue, list, string, decodeString, map2, at, map3, map, oneOf, field, succeed, fail, andThen, int)
import Json.Encode as Encode
import Time 
 

-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick 

-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model = LoggedOut LoggedOutData | LoggedIn LoggedInData -- 3rd state LoggedOutGotparties ?

type alias LoggedOutData = 
    { parties : List String 
    , userCandidate : String 
    , userJwtToken : String
    }

type alias LoggedInData = 
    { parties : List String 
    , username : String 
    , userJwtToken : String
    , toFollow : String 
    , following : List String
    , followers : List String
    , draftReceiver : String 
    , draftContent : String 
    , messages : List Message
    }

initialLoggedInData : LoggedInData 
initialLoggedInData = 
    { parties = []
    , username = "" 
    , userJwtToken = ""
    , toFollow = "" 
    , following = []
    , followers = []
    , draftReceiver = "" 
    , draftContent = ""
    , messages = []
    }


type alias User = 
    { username : String  
    , following : List String
    }


type alias Message = 
    { sender: String
    , receiver: String 
    , content: String 
    }

finalizeMessage : String -> String -> String -> Message 
finalizeMessage user draftReceiver draftContent = 
    Message user draftReceiver draftContent

-- In the Sandbox, we can fetch all known parties with any JWT
-- Allocation of parties happens automatically 
-- TODO  how this is done on a real ledger
init : () -> ( Model, Cmd Msg )
init _ = (LoggedOut (LoggedOutData [] "" ""), getParties "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnsibGVkZ2VySWQiOiJjcmVhdGUtZGFtbC1hcHAtc2FuZGJveCIsImFwcGxpY2F0aW9uSWQiOiJmb29iYXIiLCJhY3RBcyI6WyJBbGljZSJdfX0.ahT0RApOdZlrC5u0BH7ybdQcYY_DKLDQkdPCK8dz3R4") 


-- HTTP REQUESTS

---- GET PARTIES 
gotPartiesDecoder : Decoder (List String) 
gotPartiesDecoder = field "result" (list ( field "displayName" string)) 

getParties : String -> Cmd Msg
getParties token = Http.request 
    { method = "GET"
    , headers = [Http.header "Authorization" ("Bearer " ++ token )]
    , url = "http://localhost:7575/v1/parties"
    , body = Http.emptyBody
    , expect = Http.expectJson GotParties gotPartiesDecoder
    , timeout = Maybe.Nothing 
    , tracker = Maybe.Nothing
    }

---- GET ACS and extract initial loggedin data

type Contract = UserContract User | MessageContract Message 

userContractDecoder : Decoder Contract 
userContractDecoder = 
    map UserContract <| map2 User 
        (at ["payload", "username"] string)
        (at ["payload", "following"] (list string))


messageContractDecoder : Decoder Contract
messageContractDecoder = 
    map MessageContract <| map3 Message 
        (at ["payload", "sender"] string) -- TODO "result"
        (at ["payload", "receiver" ] string)
        (at ["payload", "content"] string)

contractDecoder : Decoder Contract 
contractDecoder = oneOf [ userContractDecoder, messageContractDecoder]

getAcs : String -> Cmd Msg
getAcs token = Http.request 
    { method = "GET"
    , headers = [Http.header "Authorization" ("Bearer " ++ token )]
    , url = "http://localhost:7575/v1/query"
    , body = Http.emptyBody
    , expect = Http.expectJson GotAcs (field "result" (list contractDecoder))
    , timeout = Maybe.Nothing 
    , tracker = Maybe.Nothing
    }

getFollowers : String -> List Contract -> List String 
getFollowers user xs = 
    xs |> List.filter (\x -> case x of 
                                UserContract c -> c.username /= user
                                _              -> False)
        |> List.map (\y -> case y of 
                                UserContract c -> c.username
                                _ -> "")

getMessages : List Contract -> List Message 
getMessages xs = 
    xs |> List.filter (\x -> case x of 
                                MessageContract _ -> True 
                                _                 -> False)
        |> List.map (\y -> case y of 
                                MessageContract m -> m 
                                _ -> Message "" "" "")  

getFollowing : String -> List Contract -> List String 
getFollowing user xs = 
    let
        filteredForUser = xs |> List.filter (\x -> case x of 
                                UserContract c -> c.username == user
                                _              -> False)
    in
        case filteredForUser of 
            (UserContract c)::[]    -> c.following 
            _                       -> []

getInitialDraftMessageReceiver : String -> List Contract -> String 
getInitialDraftMessageReceiver user xs = 
    case getFollowers user xs of 
        first::rest -> first 
        []          -> ""


---- CREATE USER
createUserBody : String -> String
createUserBody username = 
    let toEncode = Encode.object 
            [ ("templateId", Encode.string "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User" )
            , ("payload", Encode.object [ ("username", Encode.string username)
                                        , ("following", (Encode.list Encode.string []) )
                                        ]
            )
            ]
    in Encode.encode 0 toEncode

createUser : String -> String -> Cmd Msg 
createUser username token = Http.request 
    { method = "POST"
    , headers = [Http.header "Authorization" ("Bearer " ++ token)]
    , url = "http://localhost:7575/v1/create"
    , body = Http.stringBody "application/json" (createUserBody username)
    , expect = Http.expectJson CreatedUser (at ["result", "key"] string)
    , timeout = Maybe.Nothing 
    , tracker = Maybe.Nothing
    } 


---- FOLLOW USER 

followUserBody : String -> String -> String
followUserBody username toFollow = 
    let toEncode = Encode.object 
            [ ("templateId", Encode.string "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User" )
            , ("key", Encode.string username)
            , ("choice", Encode.string "Follow")
            , ("argument", Encode.object [ ("userToFollow", Encode.string toFollow)])
            ]
    in Encode.encode 0 toEncode 


followUser : String -> String -> String -> Cmd Msg 
followUser userName toFollow token = Http.request 
    { method = "POST"
    , headers = [Http.header "Authorization" ("Bearer " ++ token)]
    , url = "http://localhost:7575/v1/exercise"
    , body = Http.stringBody "application/json" (followUserBody userName toFollow)
    , expect = Http.expectJson FollowingUser (field "status" int)
    , timeout = Maybe.Nothing 
    , tracker = Maybe.Nothing
    }


---- SEND MESSAGE 
sendMessageBody : String -> String -> String -> String
sendMessageBody sender receiver content = 
    let toEncode = Encode.object 
            [ ("templateId", Encode.string "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User" )
            , ("key", Encode.string receiver)
            , ("choice", Encode.string "SendMessage")
            , ("argument", Encode.object [ ("sender", Encode.string sender)
                                         , ("content", Encode.string content)])
            ]
    in Encode.encode 0 toEncode 

sendMessage : String -> String -> String -> String -> Cmd Msg 
sendMessage sender receiver content token = Http.request 
    { method = "POST"
    , headers = [Http.header "Authorization" ("Bearer " ++ token)]
    , url = "http://localhost:7575/v1/exercise"
    , body = Http.stringBody "application/json" (sendMessageBody sender receiver content)
    , expect = Http.expectJson MessageSent (field "status" int)
    , timeout = Maybe.Nothing 
    , tracker = Maybe.Nothing
    }

-- MESSAGES

type Msg = 
    --  UI events triggering...
        -- Update Elm values
          UpdateUserCandidate String 
        | UpdateUserJwtToken String 
        | UpdateToFollow String 
        | UpdateMessageContent String 
        | UpdateMessageReceiver String 
        | Refresh 
        -- Initiate ledger updates
        | LoginOrCreateUser String String 
        | FollowUser 
        | FollowBackFollower String 
        | SendMessage 
    -- Ledger update ACKs
        | GotAcs (Result Http.Error (List Contract))
        | CreatedUser (Result Http.Error String)
        | GotParties (Result Http.Error (List String)) 
        | FollowingUser (Result Http.Error Int) 
        | MessageSent (Result Http.Error Int) 
    -- Timer subscription
        | Tick Time.Posix

-- UPDATE 

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

update : Msg -> Model -> ( Model, Cmd Msg ) 
update msg model = case model of 

    LoggedOut loggedOutData -> 
        case msg of 
            GotParties res                      ->  case res of 
                                                    Ok parties -> (LoggedOut {loggedOutData | parties = parties}, Cmd.none)
                                                    _          -> (LoggedOut loggedOutData, Cmd.none)
            UpdateUserCandidate newCandidate    -> (LoggedOut {loggedOutData | userCandidate = newCandidate}, Cmd.none)
            UpdateUserJwtToken newToken         -> (LoggedOut {loggedOutData | userJwtToken = newToken}, Cmd.none)           
            LoginOrCreateUser user token        ->  if user == "" 
                                                        then (model, Cmd.none)  
                                                    else if List.member user loggedOutData.parties 
                                                        then    (LoggedIn {initialLoggedInData | username = user, userJwtToken = token}
                                                                , Cmd.batch [getAcs token, getParties token]) 
                                                    else (model, createUser user token)
            CreatedUser res                     ->  case res of 
                                                        Ok user ->  (LoggedIn {initialLoggedInData | username = user, userJwtToken = loggedOutData.userJwtToken}
                                                                    , Cmd.batch [ getAcs loggedOutData.userJwtToken
                                                                                , getParties loggedOutData.userJwtToken])
                                                        _       -> (LoggedOut loggedOutData, Cmd.none)
            Tick _                               -> (model, Cmd.batch    [ getAcs loggedOutData.userJwtToken
                                                                        , getParties loggedOutData.userJwtToken])
            _                                   -> (model, Cmd.none)
    LoggedIn loggedInData -> 
        case msg of 
            --UPDATE FOLLOWERS, MESSAGES, PARTIES
            Tick  _                             ->  ( model
                                                    , Cmd.batch [ getAcs loggedInData.userJwtToken
                                                                , getParties loggedInData.userJwtToken]
                                                    )
            GotAcs res                           -> case res of 
                                                        Ok acs ->
                                                            (LoggedIn 
                                                                {loggedInData | 
                                                                    followers = (getFollowers loggedInData.username acs)
                                                                    , messages = (getMessages acs)
                                                                    , following = (getFollowing loggedInData.username acs)
                                                                    , draftReceiver = getInitialDraftMessageReceiver loggedInData.username acs
                                                                    }
                                                            , Cmd.none)
                                                        _   -> (model, Cmd.none) 
            GotParties res                      ->  case res of 
                                                        Ok newParties   ->  (LoggedIn {loggedInData | parties = newParties}
                                                                            , Cmd.none)
                                                        _               ->  (model, Cmd.none)      
            -- FOLLOW USER
            UpdateToFollow newtoFollow          ->  ( LoggedIn {loggedInData | toFollow = newtoFollow}
                                                    , Cmd.none
                                                    )
            FollowUser                          ->  (LoggedIn {loggedInData | toFollow = ""}
                                                    , followUser loggedInData.username loggedInData.toFollow loggedInData.userJwtToken) 
            FollowBackFollower name             ->  (model, followUser loggedInData.username name loggedInData.userJwtToken) 
            -- SEND MESSAGE
            UpdateMessageContent newContent         ->  (LoggedIn {loggedInData | draftContent = newContent}
                                                        , Cmd.none
                                                        ) 
            UpdateMessageReceiver newReceiver        ->  (LoggedIn {loggedInData | draftReceiver = newReceiver}
                                                        , Cmd.none
                                                        ) 
            SendMessage                         ->  (LoggedIn {loggedInData | draftContent = ""}
                                                    , sendMessage loggedInData.username loggedInData.draftReceiver loggedInData.draftContent loggedInData.userJwtToken) 
            MessageSent res                     ->  case res of 
                                                    Ok 200  -> (model, getAcs loggedInData.userJwtToken)
                                                    _       -> (model, Cmd.none)
            _                                  -> (model, Cmd.none) 



-- VIEW 

view : Model -> Html Msg  
view model = 
    case model of 
    -- LOGGED OUT PAGE
        LoggedOut {userCandidate, userJwtToken} -> 
        -- FULL PAGE
            div loggedOutPageContainerAttributes
                [ div loginContainerAttributes 
                        [ div [style "margin" "6px"] [text "Create DAML app"]
                        ,div [style "margin" "6px"] [text "If user doesn't exist, I will create. In any case, I need a JWT for her."]
                        , input [ style "margin" "6px", placeholder "Username", value userCandidate, onInput UpdateUserCandidate][] 
                        , input [ style "margin" "6px", placeholder "JWT token", value userJwtToken, onInput UpdateUserJwtToken][] 
                        , button [ style "margin" "6px", onClick (LoginOrCreateUser userCandidate userJwtToken)] [text "Log in"]
                        ]
                , div jwtContainerAttibutes
                        [ div [style "margin" "6px", style "width" "500px", style "overflow-wrap" "break-word"] [text <| "JWT token for Alice: " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnsibGVkZ2VySWQiOiJjcmVhdGUtZGFtbC1hcHAtc2FuZGJveCIsImFwcGxpY2F0aW9uSWQiOiJmb29iYXIiLCJhY3RBcyI6WyJBbGljZSJdfX0.ahT0RApOdZlrC5u0BH7ybdQcYY_DKLDQkdPCK8dz3R4"]
                        , div [style "margin" "6px", style "width" "500px", style "overflow-wrap" "break-word"] [text <| "JWT token for Bob: " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnsibGVkZ2VySWQiOiJjcmVhdGUtZGFtbC1hcHAtc2FuZGJveCIsImFwcGxpY2F0aW9uSWQiOiJIVFRQLUpTT04tQVBJLUdhdGV3YXkiLCJhY3RBcyI6WyJCb2IiXX19.BicsLjcXTwN8qZcAARfUD0yWi119xX7ywvDt7OsLyJ0"]
                        , div [style "margin" "6px", style "width" "500px", style "overflow-wrap" "break-word"] [text <| "JWT token for Carol : " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnsibGVkZ2VySWQiOiJjcmVhdGUtZGFtbC1hcHAtc2FuZGJveCIsImFwcGxpY2F0aW9uSWQiOiJmb29iYXIiLCJhY3RBcyI6WyJDYXJvbCJdfX0.xP1EXjdur8rZFtKLuajjcMkja9c2lW34OZxAn7elTKI"]
                        , div [style "margin" "6px", style "width" "500px", style "overflow-wrap" "break-word"] [text <| "JWT token for Dave : " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnsibGVkZ2VySWQiOiJjcmVhdGUtZGFtbC1hcHAtc2FuZGJveCIsImFwcGxpY2F0aW9uSWQiOiJmb29iYXIiLCJhY3RBcyI6WyJEYXZlIl19fQ.0VCm2qkWE92hazlnE_6pwTa-JamjkGHbmO1Ou5Bc3q8"]
                        ]
                ]
    -- LOGGED IN PAGE
        LoggedIn {username, toFollow, userJwtToken, following, followers, draftReceiver, draftContent, messages, parties} -> 
        -- FULL PAGE
            div lggedInPageContainerAttributes 
                [ h3 [] [text ("Welcome, " ++ username ++  "!")]
                -- FOLLOWING
                , div followingContainerAttributes
                    <|  append 
                            [ h3 [style "margin" "6px"] [text username]
                            , div [style "margin" "6px"] [text "Users I'm following:"]
                            ]
                    <|  append ( List.reverse (List.map (\li -> div [style "margin" "6px"] [text li]) following) )
                    <|  [ div [style "margin" "6px"]    [text <|    let 
                                                                        otherUsers = List.filter (\x -> (x == username || List.member x following) == False) parties
                                                                    in 
                                                                        if List.isEmpty otherUsers then "No users to follow!"
                                                    else "Users I'm not following: " ++ (String.join ", " otherUsers)]           
                        , input [ placeholder "Username to follow", value toFollow, onInput UpdateToFollow][]
                        , button [ style "margin" "6px", onClick FollowUser] [text "Follow"]
                        ]
                -- NETWORK - FOLLOWERS
                , div networkContainerAttributes
                    <| append
                        [ h3 [style "margin" "6px"] [text "The Network"]
                        , div [style "margin" "6px"] [text "My followers"]
                        ]
                    <| (List.map (\name -> followerContainer following name) followers) 
                -- MESSAGES
                , div messagesContainerAttributes
                    <| append
                        [ h3 [style "margin" "6px"] [text "Messages"]
                        , div [style "margin" "6px"] [text "Send a message to a follower"]
                        , select [onInput UpdateMessageReceiver] (List.map (\x -> option [style "margin" "6px"] [text x]) followers)
                        , input [ style "margin" "6px", placeholder "Write a message", value draftContent, onInput UpdateMessageContent, onEnter SendMessage][] 
                        , button [ style "margin" "6px", onClick SendMessage] [text "Send"]
                        ]
                    <| (List.map (\msg -> div [style "margin" "6px"] [text msg]) (List.map printMessage messages)) 
                ]

followerContainer : List String -> String -> Html Msg 
followerContainer following name = 
    if List.member name following then
        div followerContainerAttributes
            [ div [] [text name]
            ]
    else 
        div followerContainerAttributes
            [ div [] [text name]
            , button [onClick (FollowBackFollower name)][text "Follow"]
            ]
                    
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg
            else
                fail "not ENTER"
    in
        on "keydown" (andThen isEnter keyCode)                

printMessage : Message -> String 
printMessage message = 
    case message of 
    {sender, receiver, content} -> sender ++ " -> " ++ receiver ++ ": " ++ content

-- FLEXBOX CONTAINER STYLES

---- LOGGEDOUT

{- loggedOutPageContainerAttributes : List (Attribute Msg)
loggedOutPageContainerAttributes  = 
    [ style "display" "flex"
    , style "height" "796px"
    , style "align-items" "center"
    , style "justify-content" "space-evenly"
    ]   -}

loggedOutPageContainerAttributes : List (Attribute Msg)
loggedOutPageContainerAttributes  = 
    [ style "display" "flex"
    , style "height" "796px"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]  

loginContainerAttributes : List (Attribute Msg)
loginContainerAttributes = 
    [ style "height" "100px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "margin" "6px"
    ]  

jwtContainerAttibutes = 
    [ style "height" "500px"
    , style "width" "500px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "margin" "6px"
    ] 

--LOGGEDIN 

lggedInPageContainerAttributes : List (Attribute Msg)
lggedInPageContainerAttributes  = 
    [ style "display" "flex"
    , style "height" "796px"
    , style "flex-direction" "column"
    --, style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "justify-content" "flex-start"
    , style "margin" "6px"
    ]  

followingContainerAttributes : List (Attribute Msg)
followingContainerAttributes = 
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "width" "300px"
    , style "border" "1px solid grey"
    , style "margin" "6px"
    ]  

networkContainerAttributes : List (Attribute Msg)
networkContainerAttributes = followingContainerAttributes

messagesContainerAttributes : List (Attribute Msg)
messagesContainerAttributes = followingContainerAttributes

followerContainerAttributes : List (Attribute Msg)
followerContainerAttributes = 
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "justify-content" "center"
    , style "width" "300px"
    , style "margin" "6px"
    ] 


