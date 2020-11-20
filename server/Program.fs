module Program

open Saturn
open Giraffe
open Shared
open Server
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open FSharp.Data
open FSharp.Collections.ParallelSeq


let webApi =
    Remoting.createApi()
    |> Remoting.fromContext (fun (ctx: HttpContext) -> ctx.GetService<ServerApi>().Build())
    |> Remoting.withRouteBuilder routerPaths
    |> Remoting.buildHttpHandler

let webApp = choose [ webApi; GET >=> text "Welcome to full stack F#" ]

let serviceConfig (services: IServiceCollection) =
    services
      .AddSingleton<ServerApi>()
      .AddLogging()

let application = application {
    use_router webApp
    use_static "wwwroot"
    use_gzip
    use_iis
    service_config serviceConfig
    webhost_config Env.configureHost
}
(*
seq { 1 .. 100 } |>
    Seq.iter (fun _ ->
        let g = randomGenomeOfDepth 5
        printfn $"%A{g}\n%A{replaceAtDepth 2 (grabAtDepth 3 g) g}\n\n")
*)

//genBacktestingData ()
//printfn $"{Seq.length <| Seq.head backtestData}"
//printfn $"{nodeCount (randomGenomeOfDepth 5 30)}"

//let x = randomGenomeOfDepth 5 1

//printfn "%s" (show x)


//seq { 0 .. 1000 } |> Seq.iter (fun _ -> testBacktest (Trader.New 5))

Universe().Go()

//Multiverse().Go()

//printfn $"%s{genVisStr ()}"

//gridSearch ()

//run application
(*

*)

