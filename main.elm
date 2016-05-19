import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = {
    warriors : List Warrior
    , newWarrior : Warrior
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
model = Model warriors newWarrior

warriors : List Warrior       
warriors = [pepe, juan, max, goku]

newWarrior : Warrior
newWarrior = createNewWarrior warriors

pepe : Warrior
pepe = createWarrior 1 "Pepe" "Pecas" 365

juan : Warrior
juan = createWarrior 2 "Juan" "Solo" 299

max : Warrior
max = createWarrior 3 "Max" "Power" 999

goku : Warrior
goku = createWarrior 4 "Goku" "" 1196

createWarrior : Int -> String -> String -> Int -> Warrior
createWarrior id firstName lastName ki =
    Warrior id firstName lastName ki (powerLevel ki)

createNewWarrior : List Warrior -> Warrior
createNewWarrior warriors =
    createWarrior (getNewWarriorId warriors) "" "" 0

getNewWarriorId : List Warrior -> Id
getNewWarriorId warriors = 
    Maybe.withDefault 1 (List.maximum (List.map (\w -> w.id) warriors)) + 1

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
type Msg = 
    UseKaioken Id 
    | UpdateFirstName Id String 
    | UpdateLastName Id String 
    | UpdateKi Id String 
    | Add
    | Reset Id

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
        UpdateFirstName id firstName ->
            let 
                updateFirstName : WarriorAction
                updateFirstName warrior =
                    transform warrior id (\w -> { w | firstName = firstName })
            in
                { model
                | warriors = List.map updateFirstName model.warriors
                , newWarrior = updateFirstName model.newWarrior
                }
        UpdateLastName id lastName ->
            let 
                updateLastName : Warrior -> Warrior
                updateLastName warrior =
                    transform warrior id (\w -> { w | lastName = lastName })
            in
                { model
                | warriors = List.map updateLastName model.warriors
                , newWarrior = updateLastName model.newWarrior
                }
        UpdateKi id ki ->
            let 
                updateKi : Warrior -> Warrior
                updateKi warrior =
                    transform warrior id (\w -> { w | ki = Result.withDefault 0 (String.toInt ki) })
            in
                { model
                | warriors = List.map updateKi model.warriors
                , newWarrior = updateKi model.newWarrior
                }
        Add ->
            { model
            | warriors = model.warriors ++ [model.newWarrior]
            , newWarrior = reset model.newWarrior |> bumpId
            }
        Reset id ->
            { model
            | warriors = List.map (\w -> transform w id reset) model.warriors
            , newWarrior = (\w -> transform w id reset) model.newWarrior
            }

type alias WarriorAction = Warrior -> Warrior

reset : WarriorAction
reset warrior =
    { warrior
    | firstName = ""
    , lastName = ""
    , ki = 0
    }

bumpId : WarriorAction
bumpId warrior = 
    { warrior
    | id = warrior.id + 1
    }
    
transform : Warrior -> Id -> (WarriorAction) -> Warrior
transform warrior id action  =
    if warrior.id == id then (action warrior) else warrior
        
-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    [ h1 [] [text "Warriors"]
    , addwarriorView model.newWarrior   
    , table [] (warriorHeaderView :: (List.map warriorRowView model.warriors))            
    , br [] []
    -- , modelView model
    ]

addwarriorView : Warrior -> Html Msg
addwarriorView warrior =
    div []
    [ label [ for "firstName" ] [ text "First Name:" ]
    , input 
        [ type' "text"
        , id "firstName"
        , value warrior.firstName
        , onInput (UpdateFirstName warrior.id) ] []
    , label [ for "lastName" ] [ text "Last Name:" ]
    , input 
        [ type' "text"
        , id "lastName"
        , value warrior.lastName
        , onInput (UpdateLastName warrior.id) ] []
    , label [ for "ki" ] [ text "Ki:" ]
    , input 
        [ type' "text"
        , id "ki"
        , value (getNewWarriorKi warrior.ki)
        , onInput (UpdateKi warrior.id) ] []
    , input 
        [ type' "button"
        , value "Add"
        , onClick Add ] []
    ]    

warriorHeaderView : Html Msg
warriorHeaderView =
    tr [] 
    [ th [] [ text "Id" ]
    , th [] [ text "Full Name" ]
    , th [] [ text "Ki" ]
    , th [] [ text "Vegeta Says" ]
    , th [] [ text "Use Kaio-ken" ]
    ]

warriorRowView : Warrior -> Html Msg
warriorRowView warrior =
    tr []
    [ td [] [text (toString warrior.id)]
    , td [] [text (toFullName warrior)]
    , td [] [text (toString warrior.ki)]
    , td [] [text warrior.vegetaSays]
    , td [] [button [onClick (UseKaioken warrior.id)] [text "Kaio-ken!"]] 
    ]
    
-- modelView : Model -> Html Msg
-- modelView model =
--     div [] [ text ("model: " ++ (toString model)) ]
    
getNewWarriorKi : Int -> String
getNewWarriorKi ki =
    if ki == 0 then "" else toString ki 