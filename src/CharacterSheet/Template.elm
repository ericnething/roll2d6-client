module CharacterSheet.Template exposing (..)

import Array exposing (Array)
import CharacterSheet.Model exposing (..)

initialCharacterSheet : CharacterSheet
initialCharacterSheet =
    { name = "Geronimo"
    , description = "A mundane human who constantly finds himself at the center of mysteries involving horror and the occult."
    , aspects
          = Array.fromList
            [ Aspect "Disciple of the Ivory Shroud"
            , Aspect "The Manners of a Goat"
            , Aspect "I Owe Old Finn Everything"
            , Aspect "Smashing is Always an Option"
            , Aspect "An Eye for an Eye"
            ]
    , skills
          = Array.fromList
            [ Skill Great "Fight"
            , Skill Good "Athletics, Physique"
            , Skill Fair "Stealth, Provoke, Rapport"
            , Skill Average  "Crafts, Shoot, Deceive, Will"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList
            [ Stunt
              "Fire Blast"
              "You can shoot or exhale fire. Whenever you succeed with style on a Forceful attack, you may forgo the boost to place an On Fire aspect with a free invoke on the defender or a nearby object. This effect only works if the target could believably catch fire. In addition, you can never become Unarmed."
            , Stunt
                "Poisoned Weapon"
                "You use a poisoned weapon such as dagger, arrows, or darts. Once per scene, when you Sneakily or Quickly attack and deal 2 stress or more, you can force the defender to absorb 2 stress from your attack as a mild consequence. Some targets—robots, inanimate objects, and so on—are immune to poison."
            , Stunt
                "Strong Legs"
                "You have powerful legs that are especially suited for running or jumping. During contests and cliffhangers, +2 when Quickly over-coming physical obstacles or creating advantages."
            ]
    , stress =
        Array.fromList
            [ StressTrack
              "Physical"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   , StressBox 3 False
                   ])
            , StressTrack
              "Mental"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   ])
            ]
    , consequences =
        Array.fromList
            [ Consequence Mild ""
            , Consequence Moderate ""
            , Consequence Severe ""
            ]
    , conditions =
        Array.fromList
            [ Condition
              "Bruised"
              (Array.fromList [ StressBox 2 False])
            , Condition
              "In Peril"
              (Array.fromList [ StressBox 4 False ])
            , Condition
              "Indebted"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   ])
            ]
    }


fateCore : CharacterSheet
fateCore =
    { name = ""
    , description = ""
    , aspects
          = Array.fromList
            [ Aspect ""
            , Aspect ""
            , Aspect ""
            , Aspect ""
            , Aspect ""
            ]
    , skills
          = Array.fromList
            [ Skill Great ""
            , Skill Good ""
            , Skill Fair ""
            , Skill Average ""
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList []
    , stress =
        Array.fromList
            [ StressTrack
              "Physical"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   ])
            , StressTrack
              "Mental"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   ])
            ]
    , consequences =
        Array.fromList
            [ Consequence Mild ""
            , Consequence Moderate ""
            , Consequence Severe ""
            ]
    , conditions =
        Array.fromList []
    }


dresdenFilesAccelerated : CharacterSheet
dresdenFilesAccelerated =
    { name = ""
    , description = ""
    , aspects
          = Array.fromList
            [ Aspect ""
            , Aspect ""
            , Aspect ""
            , Aspect ""
            , Aspect ""
            ]
    , skills
          = Array.fromList
            [ Skill Good ""
            , Skill Fair ""
            , Skill Average ""
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList []
    , stress =
        Array.fromList
            [ StressTrack
              ""
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   ])
            ]
    , consequences =
        Array.fromList []
    , conditions =
        Array.fromList
            [ Condition
              "In Peril"
              (Array.fromList [ StressBox 4 False ])
            , Condition
              "Doomed"
              (Array.fromList [ StressBox 6 False ])
            , Condition
              "Indebted"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   ])
            ]
    }
