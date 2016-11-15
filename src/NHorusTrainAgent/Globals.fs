﻿[<AutoOpen>]
module Globals

open Fake.EnvironmentHelper

module Config =

    let ScMachineFolder = @"D:\Study\Univer\OSTIS\ostis\sc-machine"

    let ConfigFilePath = ScMachineFolder </> "bin/sc-memory.ini"
    let RepoPath = ScMachineFolder </> "bin/repo"
    let ExtensionPath = ScMachineFolder </> "bin/extensions"
    let NetExtensionPath = ""

module KbIds =
    let nrelRwStation = "nrel_rw_station"
    let nrelArrivalTime = "nrel_arrival_time"
    let nrelDepartureTime = "nreL_departure_time"
    let nrelValue = "nrel_value"
    let rrelHours = "rrel_hours"
    let rrelMinutes = "rrel_minutes"
    let conceptRwRouteCheckPoint = "concept_rw_route_check_point"
    let nrelNextPoint = "nrel_next_point"