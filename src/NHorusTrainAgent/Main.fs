module NHorusTrainAgent

open ScEngineNet.Extras
open ScEngineNet
open ScEngineNet.ScElements
open ScMemoryNet


type ConstructionTemplates(ctx :ScMemoryContext) =

    let node idStr =
        ctx.FindNode(Identifier(idStr)) :> ScElement
    
    let hours (root :ScElement) =
        (root --> ScTypes.Link) |<- (Rrel (node "rrel_hours"))
        |> ctx.CreateIterator

    let minutes (root :ScElement) =
        (root --> ScTypes.Link) |<- (Rrel (node "rrel_minutes"))
        |> ctx.CreateIterator

    let value (eIn :ScElement) (eOut :ScElement) =
        (eIn ==> eOut) |<- (Nrel (node "nrel_value"))
        |> ctx.CreateIterator

    let time (timeType :string) (root :ScElement) =
        (root ==> ScTypes.Node) |<- (Nrel (node timeType))
        |> ctx.CreateIterator
    let departureTime = time "nrel_departure_time"
    let arrivalTime = time "nrel_arrival_time"

    let rwStation stationId =
        (ScTypes.NodeConstant ==> (node stationId)) |<- (Nrel (node "nrel_rw_station"))
        |> ctx.CreateIterator
        |> Seq.head
        |> fun x -> x.[0]
        |> fun x -> if x.IsValid then Some x else None
    
    let isRwRouteCheckPoint (root :ScElement) =
        (node "concept_rw_route_check_point" --> root)
        |> ctx.CreateIterator
    
    let nextPoint (root :ScElement) =
        (root ==> ScTypes.NodeConstant) |<- (Nrel (node "nrel_next_point"))
        |> ctx.CreateIterator
        
let initializeScMemory () =
    let parameters = ScMemoryParams(true,
                        configFile = Config.ConfigFilePath,
                        repoPath = Config.RepoPath,
                        extensionsPath = Config.ExtensionPath,
                        netExtensionsPath = Config.NetExtensionPath)
    if (not ScMemory.IsInitialized)
        then ScMemory.Initialize(parameters)

[<EntryPoint>]
let main argv =
    
    use ctx = new ScMemoryContext(ScAccessLevels.MinLevel)

    printfn "%A" argv
    0 // return an integer exit code
