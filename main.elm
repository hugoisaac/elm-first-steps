import Html exposing (..)
import Html.App as Html
-- import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = {
    warriors : List Warrior
    , latestId : Int
}

type alias Warrior = {
    id : Id
    , firstName : String
    , lastName : String
    , ki: Int
    , vegetaSays : String
}

type alias Id = Int

model : Model
model = Model warriors (Maybe.withDefault 0 (List.maximum (List.map (\w -> w.id) warriors)))

warriors : List Warrior       
warriors = [pepe, juan, max, goku]

defaultWarrior : Warrior
defaultWarrior = createWarrior 0 "" "" 0

pepe : Warrior
pepe = createWarrior 1 "Pepe" "Pecas" 365

juan : Warrior
juan = createWarrior 2 "Juan" "Jones" 299

max : Warrior
max = createWarrior 3 "Max" "Power" 999

goku : Warrior
goku = createWarrior 4 "Goku" "" 1196

createWarrior : Int -> String -> String -> Int -> Warrior
createWarrior id firstName lastName ki =
    Warrior id firstName lastName ki (powerLevel ki)

powerLevel : Int -> String
powerLevel ki =
    if ki > 9000 then "It's over 9,000!!!" else "¡Insectooo!"

toFullName : Warrior -> String
toFullName warrior = warrior.firstName ++ " " ++ warrior.lastName

boostKi : Int -> Int
boostKi ki =
    ki * 2

-- UPDATE

-- TODO: Add, Remove Id messages.
type Msg = UseKaioken Id

update : Msg -> Model -> Model
update msg model =
    case msg of
        UseKaioken id ->
            let
                boostWarrior : Warrior -> Warrior
                boostWarrior warrior = 
                    if warrior.id == id 
                    then 
                        { warrior 
                          | ki = boostKi warrior.ki
                          , vegetaSays = powerLevel (boostKi warrior.ki)                         
                        } 
                    else warrior
            in                
                { model
                  | warriors = List.map boostWarrior model.warriors
                }

-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    [ 
        h1 [] [text "Warriors"] 
        , table [] (List.map warriorView model.warriors)
        -- , table [] warriorHeaderView, (List.map warriorView model.warriors)            
        , br [] []
        -- , div []
        -- [
        --     text ("model: " ++ (toString model))
        -- ]
    ]

-- warriorHeaderView : Html Msg
-- warriorHeaderView =
--     th [] 
--     [
--         td [] [text "Name"]
--         , td [] [text "Ki"]
--         , td [] []
--         , td [] []
--     ]

warriorView : Warrior -> Html Msg
warriorView warrior =
    tr []
    [
        td [] [text (toString warrior.id)]
        , td [] [text (toFullName warrior)]
        , td [] [text (toString warrior.ki)]
        , td [] [text warrior.vegetaSays]
        , td [] [button [onClick (UseKaioken warrior.id)] [text "Kaio-ken!"]] 
    ]       