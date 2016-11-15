module NHorusTrainAgent

open System
open ScEngineNet.Extras
open ScEngineNet
open ScEngineNet.ScElements
open ScMemoryNet
open ScEngineNet.LinkContent
open FSharpx
open FSharpx.Option

type RoutePointInfo = {
    Station     : ScNode
    CheckPoint  : ScNode
    Departure   : TimeSpan
    Arrival     : TimeSpan
}

type RwStation = ScNode
type RwCheckPoint = ScNode

    

type KbTraverse(ctx :ScMemoryContext) =
    
    let stations = System.Collections.Generic.List<RoutePointInfo>()

    let node idStr =
        ctx.FindNode(Identifier(idStr)) :> ScElement    


    let tryValue (root :ScElement) =
        (ScTypes.NodeConstant ==> root) |<- (Nrel (node KbIds.nrelValue))
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
                    let v = lContent |> ScLinkContent.ToInt32
                    return v
                }
        let tryGetHours = tryGetTimeUnits KbIds.rrelHours
        let tryGetMinutes = tryGetTimeUnits KbIds.rrelMinutes
        
        maybe {
            let! h_min = tryValue time
            let! h = tryGetHours h_min
            let! min = tryGetMinutes h_min
            return TimeSpan(h, min, 0)
        }


    let tryTime (timeId :string) (checkPoint :RwCheckPoint) =
        (checkPoint ==> ScTypes.NodeConstant) |<- (Nrel (node timeId))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 2
        
    let tryDepartureTime = tryTime KbIds.nrelDepartureTime
    let tryArrivalTime = tryTime KbIds.nrelArrivalTime


    let getChPointStation (chPoint :RwCheckPoint) =
        (chPoint ==> ScTypes.NodeConstant) |<- (Nrel (node KbIds.nrelRwStation))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 2


    let getRoutePointInfo (chPoint :RwCheckPoint) =
        maybe {
            let! station = getChPointStation chPoint
            let! departure = tryDepartureTime chPoint
                                >>= tryGetTime
            let! arrival = tryArrivalTime chPoint
                                >>= tryGetTime
            return { Station = station
                     CheckPoint = chPoint
                     Departure = departure;
                     Arrival = arrival }
        }
    

    let getRwCheckPoints (station :RwStation) =
        (ScTypes.NodeConstant ==> station) |<- (Nrel (node KbIds.nrelRwStation))
        |> ctx.CreateIterator
        |> Seq.choose (tryGetNode 0)
    
    let isRwRouteCheckPoint (x :ScNode) =
        (node KbIds.conceptRwRouteCheckPoint --> x)
        |> ctx.CreateIterator
        |> (Seq.isEmpty >> not)
    
    let tryGetNextPoint (chPoint :RwCheckPoint) =
        (chPoint ==> ScTypes.NodeConstant) |<- (Nrel (node KbIds.nrelNextPoint))
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 2

    let tryGetNextChPointStation (chPoint :RwCheckPoint) =
        tryGetNextPoint chPoint
        >>= getChPointStation

    let getNearbyStations (station :RwStation) :RwStation seq =
        station
        |> getRwCheckPoints
        |> Seq.choose tryGetNextChPointStation
    

    let getAllReachableStations (start :RwStation) =
        
        let rec traverse (current :RwStation seq) (passed :RwStation seq) =
            if Seq.isEmpty current then
                passed
            else
                let nexts = current
                            |> Seq.collect getNearbyStations
                            |> Seq.distinct
                let passed' = passed |> Seq.append current                               
                traverse nexts passed'

        traverse (Seq.singleton start) (Seq.empty)
    

    let findStationsByTime (startStation :RwStation) (departure :TimeSpan) (hours :int) =
        let maxTime = departure + TimeSpan(hours, 0, 0)

        let timeCheck (routePoint :RoutePointInfo) =
            routePoint.Arrival <= maxTime
        
        let rec traverse (current :RoutePointInfo seq) (passed :RoutePointInfo seq) =
            if Seq.isEmpty current then passed
            else
                let nexts = current
                            |> Seq.distinctBy (fun x -> x.CheckPoint)
                            |> Seq.filter timeCheck
                let passed' = passed |> Seq.append current
                traverse nexts passed'

        let starts = startStation
                        |> getRwCheckPoints
                        |> Seq.choose getRoutePointInfo

        let result = traverse starts Seq.empty
        
        result |> Seq.map (fun x -> x.Station)
        


        
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
