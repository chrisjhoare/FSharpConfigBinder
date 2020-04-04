namespace FSharpConfigBinder

open System
open TypesBuilder
open TypeShape.HKT
open System.Collections.Generic
open Microsoft.Extensions.Configuration

/// Given a type, builds the keyvalue pairs for the internal dictionary that would exist in a valid IConfiguration for the type.
module ConfigKeyValuePairBuilder = 

    type ConfigKeyValuePairBuilder =
        static member Assign(_ : App<ConfigKeyValuePairBuilder, 'a>, _ : Map<string,string> -> string -> 'a -> Map<string,string>) = ()

    type FieldConfigKeyValuePairBuilder =
        static member Assign(_ : App<FieldConfigKeyValuePairBuilder, 'a>, _ : Map<string,string> -> string -> 'a -> Map<string,string>) = ()

    let private configValueBuilder = 
        { new IConfigurationTypesBuilder<ConfigKeyValuePairBuilder, FieldConfigKeyValuePairBuilder> with

            member __.Union shape _ = 
                
                HKT.pack (fun kv path item ->

                    let tag = shape.GetTag item
                    let case = shape.UnionCases.[tag]

                    kv.Add (path, case.CaseInfo.Name)
                )

            member __.List (HKT.Unpack fcs) =
                HKT.pack 
                    (fun kv path items -> 
                        let updateFromItem state (index, item) = 
                            let path = sprintf "%s:%d" path index
                            fcs state path item

                        items |> List.indexed |> List.fold updateFromItem kv)

            member __.Field shape (HKT.Unpack fc) =
                HKT.pack 
                    (fun kv path item -> 
                        let newPath = 
                            let prefix = if String.IsNullOrEmpty path then String.Empty else sprintf "%s:" path
                            sprintf "%s%s" prefix shape.Label

                        let field = shape.Get item
                        fc kv newPath field)
                  
            member __.Option (HKT.Unpack fc) =
                HKT.pack 
                    (fun kv path item ->  
                        match item with
                        | Some v -> fc kv path v
                        | None -> kv)

            member __.Record _ (HKT.Unpacks fcs) = 
                HKT.pack 
                    (fun kv path record ->
                        let updateFromField (kvs:Map<string,string>) item :Map<string,string> =
                            let next = item kvs path record
                            next
                        fcs |> Array.fold updateFromField kv)

            member __.String()  = HKT.pack (fun kv path item -> kv.Add (path, sprintf "%s" item))
            member __.Int32()   = HKT.pack (fun kv path item -> kv.Add (path, sprintf "%d" item))
            member __.Bool()    = HKT.pack (fun kv path item -> kv.Add (path, sprintf "%O" item))
        }

    let mkKeyValuePairBuilderExtended<'t> extensions = 
        let program = (mkGenericProgram configValueBuilder extensions).Resolve<'t> () |> HKT.unpack
        fun (t:'t) -> 
            program Map.empty String.Empty t
            |> Map.toList
            |> List.map KeyValuePair

    let mkKeyValuePairBuilder<'t> = mkKeyValuePairBuilderExtended<'t> []
                  
    let buildConfigurationForExtended (t:'t) extensions = 
            
        let configuration = new ConfigurationBuilder()
        let values = t |> mkKeyValuePairBuilderExtended extensions
        configuration.AddInMemoryCollection(values).Build() :> IConfiguration

    let buildConfigurationFor (t:'t) = buildConfigurationForExtended t []
            

