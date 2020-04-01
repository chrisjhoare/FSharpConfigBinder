namespace FSharpConfigBinder

open System
open System.Collections.Generic
open TypeShape
open TypeShape.HKT
open TypeShape.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Configuration.Binder
     
[<AutoOpen>]
module Exceptions = 

    exception ConfigurationPathMissingException of string

module ConfigBinder = 

    open TypesBuilder
    
    [<AutoOpen>]
    module internal ConfigurationExtensions =

        type IConfiguration with
            member private this.CaseInsensitivePaths = 
                let keys = this.AsEnumerable() |> Seq.map (fun s -> s.Key) 
                HashSet(keys, StringComparer.InvariantCultureIgnoreCase)

            member this.ContainsPath (path:string) = 
                this.CaseInsensitivePaths.Contains path

            static member GetFromPath<'T> (path:string) (config:IConfiguration) = 
                if config.ContainsPath path 
                then config.GetSection(path).Get<'T>()
                else raise <| ConfigurationPathMissingException path

    type ConfigBinder =
        static member Assign(_ : App<ConfigBinder, 'a>, _ : string -> IConfiguration -> 'a) = ()

    type FieldConfigBinder =
        static member Assign(_ : App<FieldConfigBinder, 'a>, _ : (IConfiguration * string) -> 'a -> 'a) = ()

    let private configValuesBuilder = 
        { new IConfigurationTypesBuilder<ConfigBinder,FieldConfigBinder> with
            
            member __.List (HKT.Unpack fcs) =
                HKT.pack 
                    (fun path config -> 
                        config.GetSection(path).GetChildren()
                        |> Seq.indexed 
                        |> Seq.map (fst >> sprintf "%s:%d" path >> fun path -> fcs path config)
                        |> Seq.toList)

            member __.Option (HKT.Unpack fc) = 
                HKT.pack 
                    (fun path config -> 
                        if config.ContainsPath path 
                        then Some <| fc path config
                        else None)

            member __.Field shape (HKT.Unpack fc) =
                HKT.pack 
                    (fun (config, prefix) src -> 
                        let path = sprintf "%s%s" prefix shape.Label
                        shape.Set src <| fc path config)

            member __.Record shape (HKT.Unpacks fields) =
                HKT.pack
                    (fun path config -> 
                        let prefix = 
                            if String.IsNullOrEmpty path 
                            then String.Empty 
                            else sprintf "%s:" path

                        let mutable t' = shape.CreateUninitialized()
                        for f in fields do t' <- f (config, prefix) t'
                        t')

            member __.Int32()   = HKT.pack IConfiguration.GetFromPath 
            member __.String()  = HKT.pack IConfiguration.GetFromPath
            member __.Bool()    = HKT.pack IConfiguration.GetFromPath
        }
    
    let mkConfigurationBinder<'t> = 
        let program = (mkGenericProgram configValuesBuilder).Resolve<'t> () |> HKT.unpack
        fun (c:IConfiguration) -> program String.Empty c