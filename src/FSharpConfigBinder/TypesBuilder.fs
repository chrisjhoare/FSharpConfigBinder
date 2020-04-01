namespace FSharpConfigBinder

open TypeShape.Core
open TypeShape.HKT

module internal TypesBuilder =
    
    type internal IConfigurationTypesBuilder<'F,'G> =
        inherit IFSharpRecordBuilder<'F,'G>
        inherit IStringBuilder<'F>
        inherit IFSharpOptionBuilder<'F>
        inherit IInt32Builder<'F>
        inherit IFSharpListBuilder<'F>
        inherit IBoolBuilder<'F>

    let internal mkGenericProgram (builder:IConfigurationTypesBuilder<'F,'G>) =
        { 
            new IGenericProgram<'F> with
                member this.Resolve<'a>() : App<'F, 'a> = 
                    match shapeof<'a> with
                    | Fold.FSharpOption builder this r -> r
                    | Fold.FSharpRecord builder this r -> r
                    | Fold.String builder r -> r
                    | Fold.Int32 builder r -> r
                    | Fold.FSharpList builder this r -> r
                    | Fold.Bool builder r -> r
                    | _ -> failwithf  "unsupported type %O" typeof<'a> 
        }