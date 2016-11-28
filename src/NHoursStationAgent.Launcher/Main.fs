module Config =

    open Fake.EnvironmentHelper

    let ScMachineFolder = @"D:/Study/Univer/OSTIS/ostis/sc-machine/"

    let ConfigFilePath   = ScMachineFolder + "bin/sc-memory.ini.example"
    let RepoPath         = ScMachineFolder + "bin/repo"
    let ExtensionPath    = ScMachineFolder + "bin/extensions"
    let NetExtensionPath = ScMachineFolder + "bin/netextensions"


module KbIds =
    
    let rrelParamDeparture     = "rrel_param_departure_time"
    let rrelParamHours         = "rrel_param_hours"
    let rrelParamStation       = "rrel_param_departure_station"
    let nrelNetAgentParams     = "nrel_sc_net_agent_params"
    let nrelTupleOfParams      = "nrel_tuple_of_params"
    let scNetAgent             = "sc_net_agent"
    let nrelNetAgentResult     ="nrel_sc_net_agent_result"
    let nHoursStationAgentName = "nHoursAwayStationsSearchAgent"


open System
open FSharpx
open ScMemoryNet
open ScEngineNet.ScElements
open ScEngineNet.LinkContent

let initializeScMemory () =
    let parameters = ScMemoryParams(true,
                        configFile        = Config.ConfigFilePath,
                        repoPath          = Config.RepoPath,
                        extensionsPath    = Config.ExtensionPath,
                        netExtensionsPath = Config.NetExtensionPath)
    if (not ScMemory.IsInitialized)
        then ScMemory.Initialize(parameters)


[<EntryPoint>]
let main argv = 
    do initializeScMemory()
    use ctx = new ScMemoryContext(ScAccessLevels.MinLevel)

    let node id = ctx.FindNode (Identifier id)

    let setParams (station :string) (departureTime :TimeSpan) (hours :int) =
        
        let addRolesTo (root :ScNode) (roles :(ScLink * ScNode) list) =
            let addRole (el, role) =
                let arc = ctx.CreateArc (root, el, ScTypes.ArcAccessConstantPositivePermanent)
                let relArc = ctx.CreateArc (role, arc, ScTypes.ArcAccessConstantPositivePermanent)
                (arc, el, role, relArc)

            roles |> List.map addRole

        use stationLc = new ScString (station)
        use depTimeLc = new ScString (departureTime.ToString())
        use hoursLc = new ScInt32 (hours)

        use stationNode = ctx.CreateLink (stationLc)
        use depTimeNode = ctx.CreateLink (depTimeLc)
        use hoursNode = ctx.CreateLink (hoursLc)
        
        use rrelParStation = node KbIds.rrelParamStation
        use rrelParDepTime = node KbIds.rrelParamDeparture
        use rrelParHours = node KbIds.rrelParamHours

        use root = ctx.CreateNode(ScTypes.NodeConstant)
        let roles =
            [
                stationNode, rrelParStation
                depTimeNode, rrelParDepTime
                hoursNode, rrelParHours
            ] |> addRolesTo root

        let paramsNode = ctx.CreateNode (ScTypes.NodeConstant)
        use paramsArc = ctx.CreateArc (root, paramsNode, ScTypes.ArcCommon)
        use nrelTupleOfParams = node KbIds.nrelTupleOfParams
        use nrelTupleOfParamsArc = ctx.CreateArc (nrelTupleOfParams, paramsArc, ScTypes.ArcAccessConstantPositivePermanent)

        paramsNode
    
    let rec inputLoop () =

        do printfn "Station system id: "
        let stationId = Console.ReadLine()
        do printfn "Departure time: "
        let depTime = Console.ReadLine()
                        |> TimeSpan.TryParse
        do printfn "Number of hours: "
        let hours = Console.ReadLine()
                        |> Int32.TryParse
    
        match (depTime, hours) with
        | ((true, dt), (true, h)) ->
            use agentNode = node KbIds.nHoursStationAgentName
            use paramsNode = setParams stationId dt h
            use paramsArc = agentNode.AddOutputArc (paramsNode, ScTypes.ArcCommon)
            printfn "Params created. Agent should start doing its task."

        | _ ->
            printfn "Invalid input. Try again."
            inputLoop ()
    
    inputLoop()

    ignore <| Console.ReadKey()

    if ScMemory.IsInitialized
    then ignore <| ScMemory.ShutDown(true)
    0
