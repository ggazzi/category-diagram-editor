module Diagram.Selection
    exposing
        ( Selection
        , empty
        , singleObject
        , singleMorphism
        , hasObject
        , hasMorphism
        , objects
        , morphisms
        , addObject
        , addMorphism
        , removeObject
        , removeMorphism
        )

import Diagram exposing (ObjectId, MorphismId)
import Set exposing (Set)


type alias Selection =
    { objects : Set ObjectId
    , morphisms : Set MorphismId
    }


empty : Selection
empty =
    { objects = Set.empty, morphisms = Set.empty }


singleObject : ObjectId -> Selection
singleObject id =
    { objects = Set.singleton id, morphisms = Set.empty }


singleMorphism : MorphismId -> Selection
singleMorphism id =
    { objects = Set.empty, morphisms = Set.singleton id }


hasObject : ObjectId -> Selection -> Bool
hasObject id =
    .objects >> Set.member id


hasMorphism : MorphismId -> Selection -> Bool
hasMorphism id =
    .morphisms >> Set.member id


objects : Selection -> List ObjectId
objects =
    .objects >> Set.toList


morphisms : Selection -> List MorphismId
morphisms =
    .morphisms >> Set.toList


addObject : ObjectId -> Selection -> Selection
addObject id ({ objects } as selection) =
    { selection | objects = objects |> Set.insert id }


addMorphism : MorphismId -> Selection -> Selection
addMorphism id ({ morphisms } as selection) =
    { selection | morphisms = morphisms |> Set.insert id }


removeObject : ObjectId -> Selection -> Selection
removeObject id ({ objects } as selection) =
    { selection | objects = objects |> Set.remove id }


removeMorphism : MorphismId -> Selection -> Selection
removeMorphism id ({ morphisms } as selection) =
    { selection | morphisms = morphisms |> Set.remove id }
