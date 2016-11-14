module NHorusTrainAgent

open System
open ScEngineNet.Extras
open ScEngineNet
open ScEngineNet.ScElements
open ScMemoryNet
open ScEngineNet.LinkContent
open FSharpx
open FSharpx.Option


type RoutePoint = {
    Id          : string
    Departure   : TimeSpan
    Arrival     : TimeSpan
}

    

type KbTraverse(ctx :ScMemoryContext) =
    
    let stations = System.Collections.Generic.List<RoutePoint>()

    let node idStr =
        ctx.FindNode(Identifier(idStr)) :> ScElement    


    let tryValue (root :ScElement) =
        (ScTypes.NodeConstant ==> root) |<- (Nrel (node "nrel_value"))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 0


    let tryGetTime (time :ScNode) =
        let tryGetTimeUnits rrelId (root :ScElement) =
            (root --> ScTypes.Link) |<- (Rrel (node rrelId))
            |> ctx.CreateIterator
            |> fun itr -> maybe {
                    let! lContent = Seq.head itr
                                    |> tryGetElement 2
                                    >>= tryFetchLinkContent
                    let v = lContent :?> ScInt32
                    return v.Value
                }
        let tryGetHours = tryGetTimeUnits "rrel_hours"
        let tryGetMinutes = tryGetTimeUnits "rrel_minutes"
        
        maybe {
            let! h_min = tryValue time
            let! h = tryGetHours h_min
            let! min = tryGetMinutes h_min
            return TimeSpan(h, min, 0)
        }


    let tryTime (timeId :string) (checkPoint :ScNode) =
        (checkPoint ==> ScTypes.NodeConstant) |<- (Nrel (node timeId))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 2
        
    let tryDepartureTime = tryTime "nrel_departure_time"
    let tryArrivalTime = tryTime "nrel_arrival_time"


    let getRwCheckPoints (station :ScNode) =
        (ScTypes.NodeConstant ==> station) |<- (Nrel (node "nrel_rw_station"))
        |> ctx.CreateIterator
        |> Seq.map (tryGetNode 0)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
    
    let isRwRouteCheckPoint (x :ScNode) =
        (node "concept_rw_route_check_point" --> x)
        |> ctx.CreateIterator
        |> (Seq.isEmpty >> not)
    
    let tryGetNextPoint (root :ScElement) =
        (root ==> ScTypes.NodeConstant) |<- (Nrel (node "nrel_next_point"))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 2


    let getRoutePoint (station :ScNode) (chPoint :ScNode) =
        maybe {
            let! departure = tryDepartureTime chPoint
                                >>= tryGetTime
            let! arrival = tryArrivalTime chPoint
                                >>= tryGetTime
            return { Id = station.SystemIdentifier.Value
                     Departure = departure;
                     Arrival = arrival }
        }

    let findStations (station :ScNode) (departure :TimeSpan) (hours :int) =
        let maxTime = departure + TimeSpan(hours, 0, 0)

        getRwCheckPoints station
        |> Seq.map (getRoutePoint station)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        

        
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
