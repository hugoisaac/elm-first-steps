import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model = { hello : String }

model : Model
model = { hello = "Hello…" }


-- UPDATE

type Msg = HelloWorld

update : Msg -> Model -> Model
update msg model =
  case msg of
    HelloWorld -> 
      { model | hello = model.hello ++ " World!" }


-- VIEW

view : Model -> Html Msg
view model =
  div [] 
  [
    h1 [ onClick HelloWorld ] [ text model.hello ]
  ]