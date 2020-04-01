namespace FSharpConfigBinder.Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open System.Collections.Generic
open Microsoft.Extensions.Configuration
open FSharpConfigBinder
open TypeShape

module ConfigBinderTests = 

    module TypeHandling = 

        type NotNullString() = 
            static member String() :Arbitrary<string> = 
                Arb.Default.String() |> Arb.filter (isNull >> not)

        type HandledTypesSubsection = { String: string }

        type HandledTypesConfig = {
            String: string
            StringList: string list
            Bool: bool
            OptionalString: string option
            Int: int
            Subsection: HandledTypesSubsection
            OptionalSubSection: HandledTypesSubsection option
            SubsectionList: HandledTypesSubsection list
        }
    
        let bindTypeFromConfig  = ConfigBinder.mkConfigurationBinder<HandledTypesConfig>

        let buildConfigFromType = ConfigKeyValuePairBuilder.buildConfigurationFor
        let kvPairBuilder       = ConfigKeyValuePairBuilder.mkKeyValuePairBuilder<HandledTypesConfig>

        [<Property(Arbitrary = [| typeof<NotNullString>|])>]
        let ``Handles types`` (original:HandledTypesConfig) = 
           
            let roundTripped = 
                original 
                |> buildConfigFromType 
                |> bindTypeFromConfig
            
            original = roundTripped

        [<Test>]
        let ``Fails when non optional field is missing`` () = 

            let sampleType:HandledTypesConfig = Empty.empty

            let missingRequiredField = 

                let sampleTypeKeyValuePairs = kvPairBuilder sampleType
                sampleTypeKeyValuePairs |> List.filter (fun kv -> kv.Key <> "String")

            let configuration =
                let builder = new ConfigurationBuilder()
                builder.AddInMemoryCollection(missingRequiredField).Build () :> IConfiguration
            
            let expected = ConfigurationPathMissingException "String"
            let exn = Assert.Catch(fun () -> configuration |> bindTypeFromConfig |> ignore)

            Assert.AreEqual (expected, exn)


    module ConfigurationSources = 

        type SourceConfigSubType = { Int: int }
        type SourceConfigType = {
            String: string
            SubSection: SourceConfigSubType
        }

        let expected:SourceConfigType = { String = "test"; SubSection = { Int = 2 } }

        let binder = ConfigBinder.mkConfigurationBinder<SourceConfigType>
        
        [<Test>]
        let ``Works from environment variables`` () =

            System.Environment.SetEnvironmentVariable("string", "test")
            System.Environment.SetEnvironmentVariable("subsection__Int", "2")

            let configuration =
                let builder = new ConfigurationBuilder()
                builder.AddEnvironmentVariables().Build () 

            let bound = binder configuration

            Assert.AreEqual(expected, bound)

        [<Test>]
        let ``Works from json`` () = 

            let json = """{
                "string": "test",
                "subSection": { 
                    "int": 2
                }
            }"""
        
            use stream = new System.IO.MemoryStream(System.Text.Encoding.UTF8.GetBytes json)
            let configuration = ConfigurationBuilder().AddJsonStream(stream).Build()
            
            let bound = binder configuration

            Assert.AreEqual(expected, bound)

        
        type CaseTestType = { CaseTest: string }

        [<Test>]
        let ``Case insensitive for field names`` () = 

            let caseTypeTestBinder = ConfigBinder.mkConfigurationBinder<CaseTestType>

            let test kvp = 
                let builder = new ConfigurationBuilder()
                let configuration = builder.AddInMemoryCollection([kvp]).Build ()
                let output = caseTypeTestBinder configuration
                Assert.AreEqual (output.CaseTest, "test")

            ["CaseTest"; "caseTest";"CaseTest"]
            |> List.map (fun c -> KeyValuePair.Create(c, "test"))
            |> List.iter test
            

            