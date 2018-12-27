-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

module Fate.CharacterSheet.Template
    exposing
    (blank, dresdenFilesAccelerated, emptyAspect, fateCore, harryDresden_dfa, initialCharacterSheet, sarissa_dfa, tachyonSquadronShip)

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (..)


emptyAspect : Aspect
emptyAspect =
    Aspect "" 0


blank : CharacterSheet
blank =
    { name = ""
    , description = ""
    , aspects =
        Array.fromList []
    , skills =
        Array.fromList []
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList []
    , stress =
        Array.fromList []
    , consequences =
        Array.fromList []
    , conditions =
        Array.fromList []
    , notes = ""
    }


initialCharacterSheet : CharacterSheet
initialCharacterSheet =
    { name = "Geronimo"
    , description = "A mundane human who constantly finds himself at the center of mysteries involving horror and the occult."
    , aspects =
        Array.fromList
            [ Aspect "Disciple of the Ivory Shroud" 1
            , Aspect "The Manners of a Goat" 0
            , Aspect "I Owe Old Finn Everything" 0
            , Aspect "Smashing is Always an Option" 1
            , Aspect "An Eye for an Eye" 0
            ]
    , skills =
        Array.fromList
            [ Skill Great "Fight"
            , Skill Good "Athletics, Physique"
            , Skill Fair "Stealth, Provoke, Rapport"
            , Skill Average "Crafts, Shoot, Deceive, Will"
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
                    ]
                )
            , StressTrack
                "Mental"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 2 False
                    ]
                )
            ]
    , consequences =
        Array.fromList
            [ Consequence Mild "" 0
            , Consequence Moderate "" 0
            , Consequence Severe "" 0
            ]
    , conditions =
        Array.fromList []
    , notes = ""
    }


fateCore : CharacterSheet
fateCore =
    { name = ""
    , description = ""
    , aspects =
        Array.repeat 5 emptyAspect
    , skills =
        Array.fromList
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
                    ]
                )
            , StressTrack
                "Mental"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 2 False
                    ]
                )
            ]
    , consequences =
        Array.fromList
            [ Consequence Mild "" 0
            , Consequence Moderate "" 0
            , Consequence Severe "" 0
            ]
    , conditions =
        Array.fromList []
    , notes = ""
    }


dresdenFilesAccelerated : CharacterSheet
dresdenFilesAccelerated =
    { name = ""
    , description = ""
    , aspects =
        Array.repeat 5 emptyAspect
    , skills =
        Array.fromList
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
                    ]
                )
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
                    ]
                )
            ]
    , notes = ""
    }


tachyonSquadronShip : CharacterSheet
tachyonSquadronShip =
    { name = "SF-46 D \"Blackfish\" Starfighter"
    , description = "Well Armed: Gain Weapon:1 to Gunnery attacks.\nAutomated Ejection System: see page 46"
    , aspects =
        Array.fromList
            [ Aspect "Old but Reliable" 0 ]
    , skills =
        Array.fromList
            [ Skill Great "Maneuver"
            , Skill Good "Attack"
            , Skill Fair "Detection"
            , Skill Average "Defend"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList []
    , stress =
        Array.fromList
            [ StressTrack
                "Shields"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    ]
                )
            ]
    , consequences =
        Array.fromList []
    , conditions =
        Array.fromList []
    , notes = ""
    }


sarissa_dfa : CharacterSheet
sarissa_dfa =
    { name = "Sarissa, The Summer Lady"
    , description = "Her scale is Otherworldy."
    , aspects =
        Array.fromList
            [ Aspect "Mismatched Summer Lady" 0
            , Aspect "Forced to Bear This Mantle" 0
            , Aspect "Mab's Closest Daughter" 0
            , Aspect "Survived a Lifetime in Winter" 0
            , Aspect "I Don't trust Anyone" 0
            ]
    , skills =
        Array.fromList
            [ Skill Good "Focus, Guile"
            , Skill Fair "Haste, Intellect"
            , Skill Average "Flair, Force"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList
            [ Stunt "Glamour"
                "You may cast minor veils and seemings. Use Intellect to resist any disbelief attempt."
            , Stunt "The Lady's Seelie Magic"
                "Sarissa may cast evocations that fit the nature of Summer: growth, abundance, vigor, fire."
            , Stunt "Seelie Rituals"
                "When performing ritual magic that thematically fits within Summe, Sarissa gets +2 to her preparation roll and can use members of her Court to fulfill one cost. Rituals performed for or on player characters may still have a cost in the form of a drawback or specific casting requirement that the PCs must fulfill."
            , Stunt "True Fae"
                "Once per scene, Sarissa may temporarily access any of the general or Summer stunts from the True Fae mantle. This stunt remains in effect as long as Sarissa wishes it or until the end of the scene."
            ]
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
                    ]
                )
            ]
    , consequences =
        Array.fromList []
    , conditions =
        Array.fromList
            [ Condition
                "In Peril (sticky)"
                (Array.fromList [ StressBox 4 False ])
            , Condition
                "Doomed (lasting)"
                (Array.fromList [ StressBox 6 False ])
            , Condition
                "Indebted (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    ]
                )
            , Condition
                "Truth-Bound (special)"
                (Array.fromList
                    [ StressBox 1 True
                    ]
                )
            , Condition
                "Ferroburned (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            , Condition
                "Oathbreaker (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            , Condition
                "Faerie Lady (special)"
                (Array.fromList
                    [ StressBox 1 True
                    ]
                )
            ]
    , notes = ""
    }


harryDresden_dfa : CharacterSheet
harryDresden_dfa =
    { name = "Harry Dresden"
    , description = "He bears the mantle of a Knight of a Faerie Court. He operates at Otherworldy scale."
    , aspects =
        Array.fromList
            [ Aspect "Reluctant Winter Knight" 0
            , Aspect "Tempted By Power" 0
            , Aspect "Never Threaten My Family" 0
            , Aspect "Cold Hands, Warm Heart, Wise Ass" 0
            , Aspect "Wearer of Many Hats" 0
            ]
    , skills =
        Array.fromList
            [ Skill Superb "Force"
            , Skill Good "Flair, Haste"
            , Skill Fair "Focus, Guile"
            , Skill Average "Intellect"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList
            [ Stunt "Faster, Stronger, Tougher" ""
            , Stunt "Unseelie Magic" ""
            , Stunt "Evocation" ""
            , Stunt "Thaumaturgy" ""
            , Stunt "Soulgaze" ""
            , Stunt "Wanderer of the Ways" ""
            , Stunt "Evocation Specialist" ""
            , Stunt "Combat Wizard" ""
            , Stunt "Duelist Wizard" ""
            , Stunt "White Council Membership" ""
            , Stunt "Warden" ""
            , Stunt "Brand New Staff" ""
            , Stunt "Listening" ""
            , Stunt "Warden of Demonreach" ""
            , Stunt "Za-Lord" ""
            , Stunt "Soulfire" ""
            , Stunt "PARKOUR!" ""
            ]
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
                    ]
                )
            ]
    , consequences =
        Array.fromList []
    , conditions =
        Array.fromList
            [ Condition
                "Combat Wizard"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 1 False
                    ]
                )
            , Condition
                "In Peril (sticky)"
                (Array.fromList [ StressBox 4 False ])
            , Condition
                "Doomed (lasting)"
                (Array.fromList [ StressBox 6 False ])
            , Condition
                "Indebted (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    ]
                )
            , Condition
                "Winter Power (fleeting)"
                (Array.fromList
                    [ StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    , StressBox 1 False
                    ]
                )
            , Condition
                "Impaled by Cold Iron (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            , Condition
                "Disfavored (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            , Condition
                "The Third Eye (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            , Condition
                "Exhausted (sticky)"
                (Array.fromList
                    [ StressBox 1 False
                    ]
                )
            ]
    , notes = ""
    }
