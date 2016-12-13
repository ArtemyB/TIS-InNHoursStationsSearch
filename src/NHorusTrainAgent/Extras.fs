module ScEngineNet.Extras

open FSharp.Reflection

open ScEngineNet
open ScEngineNet.ScElements


type El3<'a, 'b> = 'a * ScTypes * 'b
type El5<'a, 'b, 'c> = 'a * ScTypes * 'b * ScTypes * 'c

type ScConstruction<'a, 'b, 'c> =
    | El3 of El3<'a, 'b>
    | El5 of El5<'a, 'b, 'c>


let inline (==>) e1 e2 =
    (e1, ScTypes.ArcCommonConstant, e2)

let inline (-->) e1 e2 =
    (e1, ScTypes.ArcAccessConstantPositivePermanent, e2)

let inline (|<-) (c3 :El3<'a,'b>) (rel :ScNode) =
    let a,b,c = c3
    (a,b,c, ScTypes.ArcAccessConstantPositivePermanent, rel)



open FSharpx
open FSharpx.Option

let tryGetElement i (constr :ScConstruction) =
    constr
    |> fun x -> x.[i]
    |> fun x -> if x.IsValid then Some x else None


let tryGetNode i constr =
    tryGetElement i constr
    >>= function    | :? ScNode as n -> Some n
                    | _ -> None

    
let tryFetchLinkContent (element :ScElement) =
    maybe {
        let! link = match element with
                    | :? ScLink as l -> Some l
                    | _ -> None
        return link.LinkContent
    }

open ScEngineNet.LinkContent

type ScLinkContent with
    
    static member ToStr (linkContent :ScLinkContent) =
        ScLinkContent.ToString linkContent.Bytes
      