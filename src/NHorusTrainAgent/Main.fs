module NHorusTrainAgent

open System
open ScEngineNet
open ScEngineNet.ExtensionsNet
open ScEngineNet.Extras
open ScEngineNet.ScElements
open ScEngineNet.LinkContent
open ScMemoryNet
open FSharpx
open FSharpx
open FSharpPlus

let maybe = FSharpx.Option.maybe

type RoutePointInfo = {
    Station     : ScNode
    CheckPoint  : ScNode
    Departure   : TimeSpan
    Arrival     : TimeSpan
}

type RwStation = ScNode
type RwCheckPoint = ScNode
    

type RwStationsSearch(ctx :ScMemoryContext) =

    let node idStr =
        ctx.FindNode(Identifier(idStr))

    
    let tryValue (root :ScElement) =
        (ScTypes.NodeConstant ==> root) |<- (node KbIds.nrelValue)
        |> ctx.CreateIterator
        |> Seq.head
        |> tryGetNode 0


    let tryGetTime (time :ScNode) =
        let tryGetTimeUnits rrelId (root :ScElement) =
            (root --> ScTypes.Link) |<- (node rrelId)
            |> ctx.CreateIterator
            |> fun itr -> maybe {
                    let! link = Seq.head itr |> tryGetElement 2
                    let! lContent = tryFetchLinkContent link
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
        (checkPoint ==> ScTypes.NodeConstant) |<- (node timeId)
        |> ctx.CreateIterator
        |> Seq.tryHead
        >>= tryGetNode 2
        
    let tryDepartureTime = tryTime KbIds.nrelDepartureTime
    let tryArrivalTime = tryTime KbIds.nrelArrivalTime


    let getChPointStation (chPoint :RwCheckPoint) =
        (chPoint ==> ScTypes.NodeConstant) |<- (node KbIds.nrelRwStation)
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
        (ScTypes.NodeConstant ==> station) |<- (node KbIds.nrelRwStation)
        |> ctx.CreateIterator
        |> Seq.choose (tryGetNode 0)
    
    let isRwRouteCheckPoint (x :ScNode) =
        (node KbIds.conceptRwRouteCheckPoint --> x)
        |> ctx.CreateIterator
        |> (Seq.isEmpty >> not)
    
    let tryGetNextPoint (chPoint :RwCheckPoint) =
        (chPoint ==> ScTypes.NodeConstant) |<- (node KbIds.nrelNextPoint)
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

    
    member self.FindStationsByTime (start, departureTime, hours) =
        findStationsByTime start departureTime hours



type RwStationSearchAgent() =
    
    let agentName = "nHoursAwayStationsSearchAgent"

    let mutable ctx :ScMemoryContext = null


    let node idStr =
        ctx.FindNode(Identifier(idStr))


    let writeResultToScMemory (stations :RwStation seq) =
        let resultNodeId =
            agentName + "_Result_" + (
                DateTime.Now.ToString()
                |> String.splitChar [| ' '; ':'; '/'; '.' |]
                |> String.Concat)
            |> Identifier
        use resultNode = ctx.CreateNode(ScTypes.NodeConstant, resultNodeId)
        do  stations
            |> Seq.iter (fun st ->
                    ctx.CreateArc(resultNode, st, ScTypes.ArcAccessConstantPositivePermanent)
                    |> ignore
                )

    let fetchparams (paramsNode :ScNode) = maybe {
        let! root =
            (ScTypes.NodeConstant --> paramsNode) |<- (node KbIds.nrelTupleOfParams)
            |> ctx.CreateIterator
            |> Seq.head
            |> tryGetNode 0

        let fetchParamValue id castF =
            (root --> ScTypes.Link) |<- (node id)
            |> ctx.CreateIterator
            |> Seq.head
            |> tryGetNode 2
            >>= tryFetchLinkContent
            |> Option.map castF

        let! station = maybe {
            let! idStr =
                fetchParamValue KbIds.rrelParamStation ScLinkContent.ToStr
            let id = Identifier(idStr)
            return ctx.FindNode(id)
        }
        let! hours =
            fetchParamValue KbIds.rrelParamHours ScLinkContent.ToStr
            >>= Option.tryParseWith Int32.TryParse
        let! departureTime =
            fetchParamValue KbIds.rrelParamDeparture ScLinkContent.ToStr
            >>= Option.tryParseWith TimeSpan.TryParse

        return (station, departureTime, hours)
    }
        
    interface IScExtensionNet with
        
        member self.NetExtensionName
            with get() = agentName
        member self.NetExtensionDescription
            with get() = "Агент поиска станций, находящихся в N часах езды от текущей"
        
        member self.Initialize() =
            ctx <- new ScMemoryContext(ScAccessLevels.MinLevel)

            use agentNode = node agentName

            let core = RwStationsSearch(ctx)

            agentNode.OutputArcAdded.Add(fun args ->
                maybe {
                    let out = args.Arc.EndElement
                    let! paramsNode =
                        if out.ElementType = ScTypes.NodeConstant
                        then Some (out :?> ScNode)
                        else None
                    let! agentParams = fetchparams paramsNode
                    do  core.FindStationsByTime agentParams
                        |> writeResultToScMemory
                    return ()
                } |> ignore
            )
            
            ScResult.SC_RESULT_OK

        member self.ShutDown() =
            do ctx.Dispose()
            ScResult.SC_RESULT_OK

