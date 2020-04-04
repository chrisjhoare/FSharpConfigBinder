namespace FSharpConfigBinder.Tests

open Microsoft.Extensions.Configuration


[<RequireQualifiedAccess>]
module TestUtil =

    let createConfig (kvp) = 
        
        let builder = new ConfigurationBuilder()
        builder.AddInMemoryCollection(kvp).Build () :> IConfiguration

