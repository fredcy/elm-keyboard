effect module Keyboard where { subscription = MySub } exposing
  ( keycode
  , keydowns
  , keyups
  )

{-| This library lets you listen to keyboard events.
# Subscriptions

@docs keydowns, keyups

-}

import Char exposing (KeyCode)
import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json exposing ((:=))
import Process
import Task exposing (Task)


keycode : Json.Decoder KeyCode
keycode =
  ("keyCode" := Json.int)

-- Keyboard events


{-| Subscribe to keydown.
-}
keydowns : (KeyCode -> msg) -> Sub msg
keydowns tagger =
  subscription (MySub "keydown" tagger)


{-| Subscribe to keyup.
-}
keyups : (KeyCode -> msg) -> Sub msg
keyups tagger =
  subscription (MySub "keyup" tagger)



-- SUBSCRIPTIONS


type MySub msg
  = MySub String (KeyCode -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
  MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  Dict.Dict String (Watcher msg)


type alias Watcher msg =
  { taggers : List (KeyCode -> msg)
  , pid : Process.Id
  }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
  Dict.Dict String (List (KeyCode -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
  categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
  case subs of
    [] ->
      subDict

    MySub category tagger :: rest ->
      categorizeHelp rest <|
        Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
  case maybeValues of
    Nothing ->
      Just [value]

    Just values ->
      Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed Dict.empty


type alias Msg =
  { category : String
  , keycode : KeyCode
  }


(&>) t1 t2 = t1 `Task.andThen` \_ -> t2


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    leftStep category {pid} task =
      Process.kill pid &> task

    bothStep category {pid} taggers task =
      task
        `Task.andThen` \state ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)

    rightStep category taggers task =
      task
        `Task.andThen` \state ->

      Process.spawn (Dom.onDocument category keycode (Platform.sendToSelf router << Msg category))
        `Task.andThen` \pid ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)
  in
    Dict.merge
      leftStep
      bothStep
      rightStep
      oldState
      (categorize newSubs)
      (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router {category,keycode} state =
  case Dict.get category state of
    Nothing ->
      Task.succeed state

    Just {taggers} ->
      let
        send tagger =
          Platform.sendToApp router (tagger keycode)
      in
        Task.sequence (List.map send taggers)
          `Task.andThen` \_ ->

        Task.succeed state

