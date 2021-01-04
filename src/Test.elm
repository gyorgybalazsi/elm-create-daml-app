module Test exposing (..)

import Json.Decode exposing (Error, Decoder, list, string, decodeString, map2, at, map3, map, oneOf, field)
import Debug 
import Json.Encode as Encode
import Http

type Contract = UserContract User | MessageContract Message 

type alias User = 
    { username : String  
    , following : List String
    }

userContractDecoder : Decoder Contract 
userContractDecoder = 
    map UserContract <| map2 User 
        (at ["payload", "username"] string)
        (at ["payload", "following"] (list string))

getFollowers : String -> Result Error (List Contract) -> List String 
getFollowers user acs_ = 
    case acs_ of 
        Err _ -> []
        Ok xs -> xs |> List.filter (\x -> case x of 
                                            UserContract c -> c.username /= user
                                            _              -> False)
                    |> List.map (\y -> case y of 
                                            UserContract c -> c.username
                                            _ -> "")

getMessages : Result Error (List Contract) -> List Message 
getMessages acs_ = 
    case acs_ of 
        Err _ -> []
        Ok xs -> xs |> List.filter (\x -> case x of 
                                            MessageContract _ -> True 
                                            _                 -> False)
                    |> List.map (\y -> case y of 
                                            MessageContract m -> m 
                                            _ -> Message "" "" "")  

type alias Message = 
    { sender: String
    , receiver: String
    , content: String 
    }

messageContractDecoder : Decoder Contract
messageContractDecoder = 
    map MessageContract <| map3 Message 
        (at ["payload", "sender"] string)
        (at ["payload", "receiver" ] string)
        (at ["payload", "content"] string)

contractDecoder : Decoder Contract 
contractDecoder = oneOf [ userContractDecoder, messageContractDecoder]

fullJson = """{
    "result": [
        {
            "agreementText": "",
            "contractId": "006c6ad8d8da27b0bc7acc841a2061bdf335bf42aeb905bd7bc89a20c5b3530e04",
            "observers": [],
            "payload": {
                "sender": "Alice",
                "receiver": "Bob",
                "content": "Hi Bob"
            },
            "signatories": [
                "Alice",
                "Bob"
            ],
            "templateId": "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:Message"
        },
        {
            "agreementText": "",
            "contractId": "00a106f4c1abb6618fe062262e2391616dbeb5ddef7f077895654f6acabdc45a0c",
            "key": "Alice",
            "observers": [],
            "payload": {
                "username": "Alice",
                "following": []
            },
            "signatories": [
                "Alice"
            ],
            "templateId": "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User"
        },
        {
            "agreementText": "",
            "contractId": "0054be1c024a7e50cfe19f1863e5268c35b98b294bd4eb6bdea4af07f98bff6d53",
            "key": "Bob",
            "observers": [
                "Alice"
            ],
            "payload": {
                "username": "Bob",
                "following": [
                    "Alice"
                ]
            },
            "signatories": [
                "Bob"
            ],
            "templateId": "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User"
        }
    ],
    "status": 200
}"""
acs = decodeString (field "result" (list contractDecoder)) fullJson


tom : String 
tom =
    let
        toEncode = Encode.object
            [ ( "name", Encode.string "Tom" )
            , ( "age", Encode.int 42 )
            ]
    in
        Encode.encode 0 toEncode
    

createUserBody :  String -> String
createUserBody username = 
    let toEncode = Encode.object 
            [ ("templateId", Encode.string "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User" )
            , ("payload", Encode.object [ ("username", Encode.string username)
                                        , ("following", (Encode.list Encode.string []) )
                                        ]
            )
            ]
    in Encode.encode 0 toEncode

followUserBody : String -> String -> String
followUserBody username toFollow = 
    let toEncode = Encode.object 
            [ ("templateId", Encode.string "a1acad51ed261c3c30fe7c07d177a87f44dd5007d5105dd6bec7194a866a086d:User:User" )
            , ("key", Encode.string toFollow)
            , ("choice", Encode.string "Follow")
            , ("argument", Encode.object [ ("userToFollow", Encode.string toFollow)])
            ]
    in Encode.encode 0 toEncode 

gotPartiesDecoder : Decoder (List String) 
gotPartiesDecoder = field "result" (list ( field "displayName" string))



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

