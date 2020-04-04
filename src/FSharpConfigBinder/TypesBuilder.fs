namespace FSharpConfigBinder

open TypeShape.Core
open TypeShape.HKT

module TypesBuilder =
    
    type IConfigurationTypesBuilder<'F,'G> =
        inherit IFSharpRecordBuilder<'F,'G>
        inherit IStringBuilder<'F>
        inherit IFSharpOptionBuilder<'F>
        inherit IInt32Builder<'F>
        inherit IFSharpListBuilder<'F>
        inherit IBoolBuilder<'F>
        inherit IFSharpUnionBuilder<'F,'G>

    type IExtendedGenericProgram<'F> = abstract member TryResolve: IGenericProgram<'F> -> App<'F, 'a> option

    module Fold = 

        let (|FSharpUnionSingleCase|_|) (builder : IFSharpUnionBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
            match shape with
            | Shape.FSharpUnion (:? ShapeFSharpUnion<'t> as s) ->
                if s.UnionCases |> Array.exists (fun e -> e.Arity <> 0) then None
                else 
                    match shape with
                    | Fold.FSharpUnion builder self r -> Some r
                    | _ -> None
            | _ -> None
            
      
    let internal mkGenericProgram<'F,'G> (builder:IConfigurationTypesBuilder<'F,'G>) (extensions:IExtendedGenericProgram<'F> list) = 

        let baseResolve root: App<'F, 'a> = 
            match shapeof<'a> with
            | Fold.FSharpOption builder root r -> r
            | Fold.FSharpRecord builder root r -> r
            | Fold.String builder r -> r
            | Fold.Int32 builder r -> r
            | Fold.FSharpList builder root r -> r
            | Fold.FSharpUnionSingleCase builder root r -> r
            | Fold.Bool builder r -> r
            | _ -> failwithf  "unsupported type %O" typeof<'a>

        { 
            new IGenericProgram<'F> 
                with 
                    member this.Resolve<'a>() : App<'F, 'a> = 
                        extensions 
                        |> List.tryPick (fun ext -> ext.TryResolve this)
                        |> Option.defaultWith (fun () -> baseResolve this)  
        }


    [<RequireQualifiedAccess>]
    module Extensions = 

        // Type shape internals
        let private unwrap (x : App<'F,_> ) : App<'F,_> = unbox x
        let inline private test<'T> (s : TypeShape) =
            match s with
            | :? TypeShape<'T> -> Some ()
            | _ -> None

        let private (|MappedType|_|) (program:IGenericProgram<'F>) (map:App<'F,'b> -> App<'F,'c>) shape = 

            match test<'c> shape with
            | Some () -> 
                program.Resolve<'b>()
                |> map
                |> unwrap
                |> Some
                
            | None -> None
       
        let mkExtension<'F,'b>(extension:App<'F,'b>) = 
            { 
                new IExtendedGenericProgram<'F> with
                    member __.TryResolve<'a>(_:IGenericProgram<'F>) : App<'F, 'a> option = 
                        match test<'b> shapeof<'a> with
                        | Some () -> 
                            extension
                            |> unwrap
                            |> Some
                        | _ -> None
                        
                        
            }

        let mkFromMapping<'F,'b,'c> (map:App<'F,'b> -> App<'F,'c>) =  
            { 
                new IExtendedGenericProgram<'F> with
                    member __.TryResolve<'a>(program:IGenericProgram<'F>) : App<'F, 'a> option = 
                        match shapeof<'a> with
                        | MappedType program map r -> Some r
                        | _ -> None
                        
            }


                        



                        
            



