namespace FSharpConfigBinder.Tests

open TypeShape.HKT
open TypeShape.Core
open FSharpConfigBinder.ConfigBinder
open FSharpConfigBinder.TypesBuilder
open System
open Microsoft.Extensions.Configuration

module ConfigBinderExtensionTests =

    open NUnit.Framework
    open System.Collections.Generic

    [<AutoOpen>]
    module String10 = 

        type String10 = private String10 of string
            with static member TryCreate (s:string) = if s.Length > 10 then None else Some <| String10 s

        exception StringTooLongException

        let string10Binder (HKT.Unpack fc) = 
            HKT.pack (fun path config -> 
                        let value = fc path config
                        match String10.TryCreate value with
                        | Some s10 -> s10
                        | None -> raise StringTooLongException)

   
    [<AutoOpen>]
    module NonEmptyList = 
        
        type NonEmptyList<'T> = private NonEmptyList of 'T list
            with static member TryCreate (list:'T list) = if list.Length = 0 then None else Some (NonEmptyList list)

        exception ListEmptyException

        let nonEmptyListBinder<'T> :App<ConfigBinder,'T list> -> App<ConfigBinder,NonEmptyList<'T>> = 
            fun (HKT.Unpack fc) ->
                
                HKT.pack (fun path config -> 
                    
                    match NonEmptyList.TryCreate <| fc path config with
                    | Some l -> l
                    | None -> raise ListEmptyException

                )

    type SubType = { Int: int }
    type TestRecord = { String10: String10; NeedOne: NonEmptyList<SubType> }

    let binder = 
        mkExtendedConfigurationBinder<TestRecord> 
            [
                Extensions.mkFromMapping string10Binder
                Extensions.mkFromMapping nonEmptyListBinder<SubType>
            ]
    
    let validKeyValuePairs = 
        [ 
            "String10", "String10"
            "NeedOne:0:Int", "1"
        ] 
        |> List.map KeyValuePair.Create

    [<Test>]
    let ``Can bind validated fields`` () = 

        let validConfiguration = 
            validKeyValuePairs
            |> TestUtil.createConfig

        let bound = binder validConfiguration 

        Assert.Pass()
        
    [<Test>]
    let ``Runs string validation`` () = 

        let invalidConfiguration = TestUtil.createConfig [KeyValuePair.Create("String10", "WayToooolongForYouuuuu")]

        let exn = Assert.Catch(fun () -> invalidConfiguration |> binder |> ignore)

        Assert.AreEqual(StringTooLongException, exn)

    [<Test>]
    let ``Runs list validation`` () = 

        let invalidConfiguration = 
            validKeyValuePairs
            |> List.filter (fun kv -> not <| kv.Key.StartsWith("NeedOne"))
            |> TestUtil.createConfig 

        let exn = Assert.Catch(fun () -> invalidConfiguration |> binder |> ignore)

        Assert.AreEqual(ListEmptyException, exn)
    
    type CustomExtensionRecord = { Int: int }
        with static member AlwaysOne = { Int = 1 }:CustomExtensionRecord

    [<Test>]
    let ``Can create custom extension`` () = 

        let extension = Extensions.mkExtension (HKT.pack (fun path config -> CustomExtensionRecord.AlwaysOne ))

        let binder = mkExtendedConfigurationBinder<CustomExtensionRecord> [extension]

        let emptyConfig = TestUtil.createConfig []

        let bound = binder emptyConfig

        Assert.AreEqual (CustomExtensionRecord.AlwaysOne, bound)



