module Server

open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Microsoft.ML
open Microsoft.ML.Data

open FSharpPlus
open FSharp.Data
open MathNet.Numerics.Distributions
open FSharp.Collections.ParallelSeq

open Shared


type Genome
    = Const of float
    | Column of int
    | Add of Genome * Genome
    | Sub of Genome * Genome
    | Mul of Genome * Genome
    | Div of Genome * Genome
    | Pow of Genome * Genome
    | Neg of Genome

let rec depth (g : Genome) : int =
    1 + match g with
        | Const _ | Column _ -> 0
        | Add (a, b) -> max (depth a) (depth b)
        | Sub (a, b) -> max (depth a) (depth b)
        | Mul (a, b) -> max (depth a) (depth b)
        | Div (a, b) -> max (depth a) (depth b)
        | Pow (a, b) -> max (depth a) (depth b)
        | Neg a -> depth a

let rec nodeCount (g : Genome) : int =
    1 + match g with
        | Const _ | Column _ -> 0
        | Add (a, b) -> nodeCount a + nodeCount b
        | Sub (a, b) -> nodeCount a + nodeCount b
        | Mul (a, b) -> nodeCount a + nodeCount b
        | Div (a, b) -> nodeCount a + nodeCount b
        | Pow (a, b) -> nodeCount a + nodeCount b
        | Neg a -> nodeCount a

let rec show (g : Genome) : string =
    match g with
    | Const c -> $"(con %f{c})"
    | Column c -> $"(col %i{c})"
    | Add (a, b) -> $"(+ %s{show a} %s{show b})"
    | Sub (a, b) -> $"(- %s{show a} %s{show b})"
    | Mul (a, b) -> $"(* %s{show a} %s{show b})"
    | Div (a, b) -> $"(/ %s{show a} %s{show b})"
    | Pow (a, x) -> $"(pow %s{show a} %s{show x})"
    | Neg a -> $"(neg %s{show a})"


let rec distance (a : Genome) (b : Genome) =
    match a, b with
    | Const a, Const b -> abs (a - b) / 3.
    | Column a, Column b -> float (abs (a - b)) / 3.
    | Add (a, b), Add (a', b') -> min (distance a a' + distance b b') (distance a b' + distance b a')
    | Sub (a, b), Sub (a', b') -> distance a a' + distance b b'
    | Mul (a, b), Mul (a', b') -> min (distance a a' + distance b b') (distance a b' + distance b a')
    | Div (a, b), Div (a', b') -> distance a a' + distance b b'
    | Pow (a, b), Pow (a', b') -> distance a a' + distance b b'
    | Neg a, Neg b -> distance a b
    | a, Neg (Neg b) | Neg (Neg a), b -> distance a b
    | Add (a, b), Sub (a', b') -> distance a a' * distance b b'
    | Add (a, b), Mul (a', b') -> distance a a' * distance b b'
    | Add (a, b), Div (a', b') -> distance a a' * distance b b'
    | Add (a, b), Pow (a', b') -> distance a a' * distance b b'
    | Sub (a, b), Add (a', b') -> distance a a' * distance b b'
    | Sub (a, b), Mul (a', b') -> distance a a' * distance b b'
    | Sub (a, b), Div (a', b') -> distance a a' * distance b b'
    | Sub (a, b), Pow (a', b') -> distance a a' * distance b b'
    | Mul (a, b), Add (a', b') -> distance a a' * distance b b'
    | Mul (a, b), Sub (a', b') -> distance a a' * distance b b'
    | Mul (a, b), Div (a', b') -> distance a a' * distance b b'
    | Mul (a, b), Pow (a', b') -> distance a a' * distance b b'
    | Div (a, b), Add (a', b') -> distance a a' * distance b b'
    | Div (a, b), Sub (a', b') -> distance a a' * distance b b'
    | Div (a, b), Mul (a', b') -> distance a a' * distance b b'
    | Div (a, b), Pow (a', b') -> distance a a' * distance b b'
    | Pow (a, b), Add (a', b') -> distance a a' * distance b b'
    | Pow (a, b), Sub (a', b') -> distance a a' * distance b b'
    | Pow (a, b), Mul (a', b') -> distance a a' * distance b b'
    | Pow (a, b), Div (a', b') -> distance a a' * distance b b'
    | a, Neg b | Neg a, b -> distance a b ** 2.
    | a, Const _ | a, Column _ | Const _, a | Column _, a -> float (nodeCount a) * 10.

type OptionBuilder () =
    member _.Bind (x, f) = match x with | None -> None | Some a -> f a
    member _.Return x = Some x
    member _.Zero () = None

let option = new OptionBuilder ()

let parseNum<'t> (f : string -> 't option) (cs : char list) : 't * char list =
    let charListToStr = Array.ofList >> System.String
    let mutable stop = false
    let mutable len = 0
    let cs = List.skipWhile ((=) ' ') cs
    while not stop do
        len <- len + 1
        match f (cs.[0..len] |> charListToStr) with
        | Some _ -> ()
        | None -> stop <- true
    (f (cs.[0..len-1] |> charListToStr)).Value, cs.[len..]

let tryFloat (s : string) : float option = try Some (float s) with _ -> None
let tryInt   (s : string) :   int option = try Some (int   s) with _ -> None

let parseFloat = parseNum tryFloat
let   parseInt = parseNum tryInt

let rec parse (cs : char list) : (Genome * char list) option =
    let go (xs : char list) =
        option {
            let! (a, xs') = parse xs
            let! (b, xs'') = parse xs'
            return (a, b, xs'')
        }

    match cs with
    | ' '::xs | '\n'::xs -> parse xs
    | '('::xs ->
        match xs with
        | ' '::xs | '\n'::xs -> parse xs
        | 'c'::'o'::'n'::' '::xs ->
            let f, xs' = parseFloat xs
            Some (Const f, xs'.[1..])
        | 'c'::'o'::'l'::' '::xs ->
            let i, xs' = parseInt xs
            Some (Column i, xs'.[1..])
        | '+'::' '::xs ->
            option {
                let! a, b, xs' = go xs
                return Add (a, b), xs'.[1..]
            }
        | '-'::' '::xs ->
            option {
                let! a, b, xs' = go xs
                return Sub (a, b), xs'.[1..]
            }
        | '*'::' '::xs ->
            option {
                let! a, b, xs' = go xs
                return Mul (a, b), xs'.[1..]
            }
        | '/'::' '::xs ->
            option {
                let! a, b, xs' = go xs
                return Div (a, b), xs'.[1..]
            }
        | 'p'::'o'::'w'::' '::xs ->
            option {
                let! a, x, xs' = go xs
                return Pow (a, x), xs'.[1..]
            }
        | 'n'::'e'::'g'::' '::xs ->
            option {
                let! a, xs' = parse xs
                return Neg a, xs'.[1..]
            }
        | _ -> None
    | _ -> None

let rec interp (cols : float []) (g : Genome) : float =
    let interp = interp cols
    match g with
    | Const d -> d
    | Column i -> cols.[i]
    | Add (a, b) -> interp a + interp b
    | Sub (a, b) -> interp a - interp b
    | Mul (a, b) -> interp a * interp b
    | Div (a, b) -> interp a / interp b
    | Pow (a, x) -> interp a ** interp x
    | Neg a -> -interp a

[<CLIMutable>]
type SnapShot96 =
    {
        [<ColumnName("Label"); LoadColumn(0)>] Label    : float32
        [<VectorType(96);   LoadColumn(1,96)>] Features : float32 []
    }

[<CLIMutable>]
type Prediction =
    {
        [<ColumnName("Score")>] Prediction : float32
    }

let dir = "C:/Users/Ace/source/repos/AdxMiner/AdxMiner"
let     testDataDir = "C:/Users/Ace/Documents/cAlgo/Sources/Robots/tickgrabber/BTCUSD.test.csv"
let backtestDataDir = $"{dir}/Backtest.BTCUSD.csv"

let genPredictorData () =
    let f = System.IO.File.ReadAllLines testDataDir
    printfn $"writing: %s{backtestDataDir}"
    use sw = new System.IO.StreamWriter (backtestDataDir)
    f |> Seq.skip 1 |> Seq.iter (fun l -> sw.WriteLine ($"0,%s{l}"))


let olsModels = [
    "2020-11-03-00-58_BTCUSD_0000001_OlsRegression_rms_979937_r2_099.zip"
    "2020-11-03-01-05_BTCUSD_0000002_OlsRegression_rms_1311568_r2_099.zip"
    "2020-11-03-01-10_BTCUSD_0000003_OlsRegression_rms_1585608_r2_099.zip"
    "2020-11-03-01-16_BTCUSD_0000006_OlsRegression_rms_2172180_r2_099.zip"
    "2020-11-03-01-22_BTCUSD_0000009_OlsRegression_rms_2600254_r2_099.zip"
    "2020-11-03-01-28_BTCUSD_0000016_OlsRegression_rms_3369103_r2_099.zip"
    "2020-11-03-01-34_BTCUSD_0000025_OlsRegression_rms_4149410_r2_099.zip"
    "2020-11-03-01-39_BTCUSD_0000041_OlsRegression_rms_5196465_r2_099.zip"
    "2020-11-03-01-45_BTCUSD_0000067_OlsRegression_rms_6580654_r2_099.zip"
    "2020-11-03-01-51_BTCUSD_0000109_OlsRegression_rms_8227550_r2_099.zip"
    "2020-11-03-01-57_BTCUSD_0000177_OlsRegression_rms_10383734_r2_099.zip"
    "2020-11-03-02-03_BTCUSD_0000287_OlsRegression_rms_13232246_r2_099.zip"
    "2020-11-03-02-09_BTCUSD_0000464_OlsRegression_rms_16946888_r2_099.zip"
    "2020-11-03-02-15_BTCUSD_0000751_OlsRegression_rms_21983420_r2_098.zip"
    "2020-11-03-02-21_BTCUSD_0001215_OlsRegression_rms_30130236_r2_097.zip"
    (*
    "2020-09-05-20-52_AUDUSD_0000001_OlsRegression_rms_00015_r2_099.zip"
    "2020-09-05-20-58_AUDUSD_0000002_OlsRegression_rms_00021_r2_099.zip"
    "2020-09-05-21-04_AUDUSD_0000003_OlsRegression_rms_00025_r2_099.zip"
    "2020-09-05-21-10_AUDUSD_0000006_OlsRegression_rms_00036_r2_099.zip"
    "2020-09-05-21-15_AUDUSD_0000009_OlsRegression_rms_00043_r2_099.zip"
    "2020-09-05-21-21_AUDUSD_0000016_OlsRegression_rms_00058_r2_099.zip"
    "2020-09-05-21-27_AUDUSD_0000025_OlsRegression_rms_00073_r2_099.zip"
    "2020-09-05-21-33_AUDUSD_0000041_OlsRegression_rms_00096_r2_099.zip"
    "2020-09-05-21-39_AUDUSD_0000067_OlsRegression_rms_00125_r2_099.zip"
    "2020-09-05-21-45_AUDUSD_0000109_OlsRegression_rms_00166_r2_099.zip"
    "2020-09-05-21-50_AUDUSD_0000177_OlsRegression_rms_00225_r2_099.zip"
    "2020-09-05-21-56_AUDUSD_0000287_OlsRegression_rms_00312_r2_098.zip"
    "2020-09-05-22-02_AUDUSD_0000464_OlsRegression_rms_00442_r2_097.zip"
    "2020-09-05-22-08_AUDUSD_0000751_OlsRegression_rms_00645_r2_095.zip"
    "2020-09-05-22-14_AUDUSD_0001215_OlsRegression_rms_00948_r2_090.zip"
    *)
]

let sdcaModels = [
    (*
    "2020-09-06-19-42_AUDUSD_0000464_SdcaRegression_rms_00313_r2_098.zip"
    "2020-09-06-20-04_AUDUSD_0000751_SdcaRegression_rms_00404_r2_098.zip"
    "2020-09-06-20-26_AUDUSD_0001215_SdcaRegression_rms_00510_r2_097.zip"
    "2020-09-06-20-48_AUDUSD_0001967_SdcaRegression_rms_00637_r2_095.zip"
    "2020-09-06-21-09_AUDUSD_0003183_SdcaRegression_rms_00919_r2_091.zip"
    *)
    "2020-11-03-05-06_BTCUSD_0000464_SdcaRegression_rms_20053139_r2_098.zip"
    "2020-11-03-05-28_BTCUSD_0000751_SdcaRegression_rms_26182991_r2_097.zip"
    "2020-11-03-05-50_BTCUSD_0001215_SdcaRegression_rms_35589481_r2_095.zip"
]

let genBacktestingData () =
    let step1 () =
        let mlContext = MLContext ()
        let  olsDataView = mlContext.Data.LoadFromTextFile<SnapShot96>(backtestDataDir, hasHeader=false, separatorChar=',')
        let sdcaDataView = mlContext.Data.LoadFromTextFile<SnapShot96>(backtestDataDir, hasHeader=false, separatorChar=',')
        let  olsPredictors = ( olsModels |> Seq.map (fun model -> mlContext.Model.Load $"{dir}/models/models/{model}")) |> Seq.map fst
        let sdcaPredictors = (sdcaModels |> Seq.map (fun model -> mlContext.Model.Load $"{dir}/models/models/{model}")) |> Seq.map fst
        let  olsPredictions =  olsPredictors |> Seq.map (fun predictor -> predictor.Transform  olsDataView |> fun view -> view.GetColumn<float32> "Score")
        let sdcaPredictions = sdcaPredictors |> Seq.map (fun predictor -> predictor.Transform sdcaDataView |> fun view -> view.GetColumn<float32> "Score")
        use olsSw = new System.IO.StreamWriter $"{dir}/data/backtest.ols.csv"
        use sdcaSw = new System.IO.StreamWriter $"{dir}/data/backtest.sdca.csv"
        Seq.transpose  olsPredictions |> Seq.iter (fun row -> (row |> Seq.map string |> Seq.intersperse "," |> Seq.iter  olsSw.Write);  olsSw.Write "\n")
        Seq.transpose sdcaPredictions |> Seq.iter (fun row -> (row |> Seq.map string |> Seq.intersperse "," |> Seq.iter sdcaSw.Write); sdcaSw.Write "\n")
    let step2 () =
        let  olsData = System.IO.File.ReadAllLines $"{dir}/data/backtest.ols.csv"
        let sdcaData = System.IO.File.ReadAllLines $"{dir}/data/backtest.sdca.csv"
        let testData = System.IO.File.ReadAllLines testDataDir |> Array.skip 1
        use sw     = new System.IO.StreamWriter $"{dir}/data/backtest.csv"
        use swFull = new System.IO.StreamWriter $"{dir}/data/backtest.full.csv"
        Array.zip3 olsData sdcaData testData |> Array.iter (fun (ols, sdca, test) ->
            sw    .WriteLine        $"{ols},{sdca}"
            swFull.WriteLine $"{test},{ols},{sdca}")
    step1 ()
    step2 ()

let backtestData : float [] [] =
    System.IO.File.ReadAllLines $"{dir}/data/backtest.csv" |> Array.map (fun row -> row.Split ',' |> Array.map float)

let halfLen = Array.length backtestData / 2

let trainData = Array.take halfLen backtestData
let  testData = Array.skip halfLen backtestData

let ROW_LEN = backtestData |> Array.head |> Array.length

let ITE_PROB = 0.4

let randomGenomeOfDepth (rowLen : int) (depth : int) : Genome =
    let rng = System.Random ()
    let rec go (depth : int) =
        let go () = go (depth - 1)
        if depth = 1
        then
            match rng.NextDouble () with
            | n when n < 0.2 -> Const (rng.NextDouble () * 100. * if rng.NextDouble () > 0.5 then 1. else -1.)
            | _              -> Column (rng.Next (0, rowLen))
        else
            match rng.Next (1, 7) with
            | 1 -> Add (go (), go ())
            | 2 -> Sub (go (), go ())
            | 3 -> Mul (go (), go ())
            | 4 -> Div (go (), go ())
            | 5 -> Pow (go (), go ())
            | _ -> Neg (go ())
    go depth

let rec grabAtDepth (depth : int) (g : Genome) : Genome =
    if depth = 1 then g
    else
        let grabAtDepth = grabAtDepth (depth - 1)
        let rng = System.Random ()
        let aOrB = rng.Next (1, 3) > 1
        match g with
        | Const c -> Const c
        | Column c -> Column c
        | Add (a, b) -> grabAtDepth (if aOrB then a else b)
        | Sub (a, b) -> grabAtDepth (if aOrB then a else b)
        | Mul (a, b) -> grabAtDepth (if aOrB then a else b)
        | Div (a, b) -> grabAtDepth (if aOrB then a else b)
        | Pow (a, b) -> grabAtDepth (if aOrB then a else b)
        | Neg a -> grabAtDepth a

let rec replaceAtDepth (depth : int) (g : Genome) (replacement : Genome) : Genome =
    if depth = 1 then replacement
    else
        let replaceAtDepth = replaceAtDepth (depth - 1) replacement
        let rng = System.Random ()
        let go a b = if rng.Next (1, 3) > 1 then (replaceAtDepth a, b) else (a, replaceAtDepth b)
        match g with
        | Const _ | Column _ -> replacement
        | Add (a, b) -> Add (go a b)
        | Sub (a, b) -> Sub (go a b)
        | Mul (a, b) -> Mul (go a b)
        | Div (a, b) -> Div (go a b)
        | Pow (a, x) -> Pow (go a x)
        | Neg a -> Neg (replaceAtDepth a)

type      TradeId =      TradeId of System.Guid

type Direction = Buy | Sell

let STARTING_EQUITY = 100000.

type Trader =
    {
        Genome : Genome
    }

    static member New (depth : int) =
        {
            Genome = randomGenomeOfDepth (ROW_LEN - 1) depth
        }

let weightedAvg (xs : float seq) : float =
    let len = float (Seq.length xs)
    xs
        |> Seq.zip (seq { 1. .. len } |> Seq.map (fun x -> 1. / x))
        |> Seq.map (fun (a, b) -> a * b)
        |> Seq.sum
        |> fun x -> x / len

type Open = {
        Dir   : Direction
        Price : float
    }

type Trade
    = Open of TradeId * Open
    | Close of TradeId * float

let backtest (fs : float [] []) (x : Trader) : Trade seq =
    try
        let mutable tradeStack = []
        seq {
            for row in fs do
                match interp row x.Genome with
                | sentiment when sentiment = System.Double.NaN -> ()
                | sentiment ->
                    while List.length tradeStack < 10 &&
                          List.length tradeStack < abs (int sentiment) do
                        let id = TradeId (System.Guid.NewGuid ())
                        tradeStack <- id :: tradeStack
                        Open
                            ( id
                            , {
                                Dir   = if sentiment > 0. then Sell else Buy
                                Price = row.[0]
                              }
                            )
                    while List.length tradeStack > abs (int sentiment) do
                        match tradeStack with
                        | trade :: trades ->
                            tradeStack <- trades
                            Close (trade, row.[0])
                        | [] -> ()
        }
    with _ -> Seq.empty

type Crossover = Equal | Larger | Smaller

type Generation =
    {
        Traders : (float * float * Trader) []
    }

let evalHistory (ts : Trade seq) : float =
    try
        let mutable profit = 0.
        let mutable s = []
        for trade in ts do
            match trade with
            | Open (i, o) -> s <- Open (i, o) :: s
            | Close (i, c) ->
                match s with
                | Open (i', o) :: trades when i' = i ->
                    s <- trades
                    profit <- (profit - 1.) + match o.Dir with | Buy -> c - o.Price | Sell -> o.Price - c
                | Open (i', _o) :: _ -> () //printfn "Err Open %A" i'
                | Close (i, _c) :: _ -> () //printfn "Err Close %A" i
                | [] -> () //printfn "Err [] %A %A" i c
        profit
    with _ -> 0.

let header = """
<!doctype html>
<html>
<head>
  <script type="text/javascript" src="https://unpkg.com/vis-graph3d@latest/dist/vis-graph3d.min.js"></script>
  <script type="text/javascript">

    function drawVisualization() {
      var data = new vis.DataSet();
      var data2 = new vis.DataSet();
      var counter = 0;
"""

let options = """
      var options = {
        width:  '600px',
        height: '600px',
        style: 'bar-color',
        showPerspective: true,
        showGrid: true,
        showShadow: false,
        keepAspectRatio: true,
        verticalRatio: 0.4
      };
"""

let genVisStr () =
    header +
    """
      var arr = [
""" +
    (seq { 0 .. ROW_LEN - 1 } |> Seq.fold (fun acc_i i ->
        let row = seq { 0 .. ROW_LEN - 1 } |> Seq.fold (fun acc_j j ->
            acc_j + sprintf $"%9.1f{evalHistory (backtest trainData { Genome = Sub (Column j, Column i) })}, ") ""
        acc_i + $"[%s{row}],") "")
    + "];"


let gridSearch () =
    printf $"%9.1f{evalHistory (backtest testData { Genome = Sub (Column 0, Column 1) })}\n"
    printf $"%9.1f{evalHistory (backtest testData { Genome = Sub (Column 3, Column 4) })}\n"
    printf $"%9.1f{evalHistory (backtest testData { Genome = Sub (Column 14, Column 9) })}\n"
    for i in seq { 0 .. ROW_LEN - 1 } do
        for j in seq { 0 .. ROW_LEN - 1 } do
            printf $"%9.1f{evalHistory (backtest testData { Genome = Sub (Column i, Column j) })}, "
        printfn ""
    printfn "\n\n"
    for i in seq { 0 .. ROW_LEN - 1 } do
        for j in seq { 0 .. ROW_LEN - 1 } do
            printf $"%9.1f{evalHistory (backtest testData { Genome = Neg (Sub (Column i, Column j)) })}, "
        printfn ""


let buySellNovelty (ts : Trade seq) : float =
    try
        ts
            |> Seq.fold (fun buys x ->
                match x with
                | Close _ -> buys
                | Open (_, o) ->
                    match o.Dir with
                    | Buy -> buys + 1
                    | Sell -> buys) 0
            |> fun buys -> float buys / float (Seq.length ts / 2)
            |> fun ratio -> 1. / (abs (0.5 - ratio))
    with _ -> 0.

let closeNovelty (ts : Trade seq) : float =
    float (Seq.length (ts |> Seq.filter (function | Close _ -> true | Open _ -> false)))


type Universe () =
    let INIT_POP_SIZE     = 2000

    let TOTAL_GENERATIONS = 40

    //let mutable MUTATION_MOMENTUM = 1000.

    let mutable POOL = Set.empty

    let noveltyAndProfit (d : float [] []) (t : Trader) =
        let bt = backtest d t
        let profit = evalHistory bt
        try
            let n
                = (if profit < 0. then 1. else Math.Log profit)
                * (Math.Log (abs profit))
                * (1. / Math.Log (if profit = 0. then Math.E else abs profit))
                * (Math.Log (buySellNovelty bt))
                * (Math.Log (closeNovelty bt))
                * (1. / Math.Log (float (nodeCount t.Genome)))
                * (Math.Log (POOL |> Set.remove t |> Set.fold (fun acc x -> (acc + distance t.Genome x.Genome) / 2.) 0.))
            if Double.IsInfinity n || Double.IsNaN n
            then 1., profit
            else n, profit
        with _ -> 0., 0.

    let rateTraders (d : float [] []) (ts : Trader []) : (float * float * Trader) [] =
        ts
            |> PSeq.map (fun t -> let n, p = noveltyAndProfit d t in n, p, t)
            |> Seq.toArray

    let initGen =
        ((seq { 1 .. 500 } |> Seq.map (fun _ -> Trader.New 2)) ++
         (seq { 1 .. 500 } |> Seq.map (fun _ -> Trader.New 3)) ++
         (seq { 1 .. 500 } |> Seq.map (fun _ -> Trader.New 4)) ++
         (seq { 1 .. 500 } |> Seq.map (fun _ -> Trader.New 5)))
            |> Set.ofSeq
            |> Set.toArray
            |> fun ts -> POOL <- (Set.ofSeq ts); ts
            |> fun ts ->
                {
                    Traders = rateTraders trainData ts |> Array.filter (fun (n, _, _) -> n <> nan && n > 0.)
                }

    let mutable generation             = 0
    let mutable generations            = [initGen]
    let mutable crossoverMutationRatio = 0.
    let mutable noveltyFitnessRatio    = 1.
    let mutable topNovelty             = []
    let mutable topProfit              = []
    let mutable heuristicScores        = []
    let mutable heuristicDeltaDeriv    = 1.
    let mutable previousTopScore       = 0.


    let crossoverGenomes (a : Genome) (b : Genome) =
        let crossoverDepth = int <| float (depth a) * (1. - Beta(2., 3.).Sample ())
        let crossoverBias =
            match (System.Random ()).NextDouble () with
            | n when n >= 0.75 -> Larger
            | n when n <= 0.25 -> Smaller
            | _ -> Equal
        let bBit = grabAtDepth (crossoverDepth - match crossoverBias with | Larger -> 1 | Smaller -> -1 | Equal -> 0) b
        replaceAtDepth crossoverDepth a bBit

    let crossoverTraders (a : Trader) (b : Trader) : Trader =
        {
            Genome = if (System.Random ()).NextDouble () >= 0.5
                     then crossoverGenomes a.Genome b.Genome
                     else crossoverGenomes b.Genome a.Genome
        }

    let rec mutate (t : Trader) =
        let rng = System.Random ()
        let rec mutationCatamorphism (f : Genome -> Genome) (x : Genome) : Genome =
            if rng.NextDouble () >= 0.66
            then f x
            else
                let mc = mutationCatamorphism
                match x with
                | Const c -> Const c
                | Column c -> Column c
                | Add (a, b) -> Add (mc f a, mc f b)
                | Sub (a, b) -> Sub (mc f a, mc f b)
                | Mul (a, b) -> Mul (mc f a, mc f b)
                | Div (a, b) -> Div (mc f a, mc f b)
                | Pow (a, b) -> Pow (mc f a, mc f b)
                | Neg a -> Neg (mc f a)
        let changeNode (g : Genome) =
            let nodes = [ Add; Sub; Mul; Div; Pow ]
            match g with
            | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b) -> nodes.[rng.Next (0, Seq.length nodes)] (a, b)
            | _ -> g
        let switchBranches (g : Genome) =
            match g with
            | Add (a, b) -> Add (b, a)
            | Sub (a, b) -> Sub (b, a)
            | Mul (a, b) -> Mul (b, a)
            | Div (a, b) -> Div (b, a)
            | Pow (a, b) -> Pow (b, a)
            | _ -> g
        let rec mutateLeaf (g : Genome) =
            match g with
            | Const c -> Const (c * (rng.NextDouble () * (0.1 + (1000. * noveltyFitnessRatio)) * if rng.NextDouble () > 0.5 then 1. else -1.))
            | Column _ -> Column (rng.Next (0, ROW_LEN + 1))
            | _ -> g
        let ret =
            mutationCatamorphism
                (match rng.NextDouble () with
                | n when n >= 0.6 ->     changeNode
                | n when n >= 0.3 -> switchBranches
                | _               ->     mutateLeaf) t.Genome

        if ret = t.Genome then mutate t else { t with Genome = ret }

    let metaHeuristicSort ((n, p, t) : float * float * Trader) : float =
        (0.1 + (n * noveltyFitnessRatio)) * ((p * (1. - noveltyFitnessRatio)) * 0.5) * (1. / float (nodeCount t.Genome))

    let nextTraders (ts : (float * float * Trader) []) : Trader [] =
        let ts =
            ts
                |> Array.filter (fun (n, _, _) -> n > 1. && nan <> n)
                |> Set.ofArray
                |> Set.toArray
                |> Array.sortByDescending (fun (n,_,_) -> n)
                |> Array.map (fun (_,_,t) -> t)
        let dist = Beta (2., float generation)
        let rng = System.Random ()
        let mutable acc = []
        let newIdx () = int (dist.Sample () * float (Array.length ts - 1))
        while List.length acc <> INIT_POP_SIZE do
            let go () =
                if rng.NextDouble () < crossoverMutationRatio
                then crossoverTraders ts.[newIdx ()] ts.[newIdx ()]
                else mutate ts.[newIdx ()]
            let mutable nextTrader = go ()
            while POOL |> Set.contains nextTrader do
                nextTrader <- go ()
            POOL <- POOL |> Set.add nextTrader
            acc <- nextTrader :: acc
        acc |> Seq.toArray

    member this.Go () =
        generation <- generation + 1
        crossoverMutationRatio <- 1. / float (TOTAL_GENERATIONS - generation)
        noveltyFitnessRatio <- 1. - crossoverMutationRatio
        let ratedTraders = generations.[0].Traders
        printfn $"\n\n\n~~~~\n\n\nGen: %i{generation}"
        ratedTraders
            |> Seq.sortByDescending (fun (n,_,_) -> n)
            |> Seq.take 50
            |> PSeq.mapi (fun i (n,p,t) -> let n',p' = noveltyAndProfit testData t in i,n,n',p,p',t)
            |> fun ts ->
                ts
                    |> fun ts -> (Seq.toList ts) ++ topNovelty
                    |> Seq.sortByDescending (fun (_,n,n',p,p',t) -> ((n + n') / 2.) * ((p + p') / 2.) * (1. / float (nodeCount t.Genome)))
                    |> fun ts -> topNovelty <- List.take 20 ts
                topNovelty |> List.iter (fun (_,_,_,p,p',t) -> printfn $"%f{(p + p') / 2.} %s{show t.Genome}")
                printfn ""
                ts
                    |> fun ts -> (Seq.toList ts) ++ topProfit
                    |> Seq.sortByDescending (fun (_,_,_,p,p',t) -> ((p + p') / 2.) * (1. / float (nodeCount t.Genome)))
                    |> fun ts -> topProfit <- List.take 20 ts
                topProfit |> List.iter (fun (_,_,_,p,p',t) -> printfn $"%f{(p + p') / 2.} %s{show t.Genome}")
                printfn ""
                ts
            |> Seq.sortByDescending (fun (_,_,_,p,p',_) -> (p + p') / 2.)
            |> Seq.iteri (fun i (j,n,n',p,p',t) -> printfn $"%03i{i} %03i{j} Novelty: %f{(n + n') / 2.} $: %f{(p + p') / 2.} %s{show t.Genome}")
        //heuristicScores <- (ratedTraders |> Array.head |> fst) :: List.take WEIGHTED_HEURISTIC_LEN heuristicScores
        //let weightedScore = weightedAvg heuristicScores
        //heuristicDeltaDeriv <- weightedScore - previousTopScore
        //previousTopScore <- weightedScore
        generations <-
            {
                Traders =
                    nextTraders
                        (generations
                            |> Seq.fold (fun acc x -> x.Traders :: acc) []
                            |> Seq.concat
                            |> Seq.toArray) |> rateTraders testData
            } :: generations
        if generation < TOTAL_GENERATIONS
        then this.Go ()
        else
            (*generations |> List.rev |> List.skip 2 |> List.iteri (fun i g ->
                printfn $"Gen: %i{i+3}\n"
                g.Traders
                    |> Seq.sortByDescending (fun (n, _) -> n)
                    |> fun xs -> Seq.take (if Seq.length xs < 50 then Seq.length xs else 50) xs
                    |> Seq.iter (fun (n, t) -> printfn $"Novelty: %f{n} ($): %f{evalHistory (backtest trainData t)} $: %f{evalHistory (backtest testData t)}")
                printfn "\n")*)
            let genomes = generations |> Seq.fold (fun acc x -> x.Traders |> Seq.map (fun (_,_,t) -> t) |> Seq.fold (fun acc x -> acc.Add x.Genome : Set<_>) acc) Set.empty
            use genomesSw = new System.IO.StreamWriter "genomes.csv"
            use genomesWeightedSw = new System.IO.StreamWriter "genomesWeighted.csv"
            printfn "Writing genomes!"
            let go (d : float [] []) (g : Genome) = evalHistory (backtest d { Genome = g })
            genomes
                |> PSeq.map (fun g -> go trainData g, go testData g, g)
                |> Seq.sortByDescending (fun (tp,vp,t) -> ((tp + vp) / 2.))
                |> fun g -> g |> Seq.iter (fun (tp, vp, t) -> genomesSw.WriteLine $"%f{(tp + vp) / 2.}, %s{show t}"); g
                |> Seq.sortByDescending (fun (tp,vp,t) -> ((tp + vp) / 2.) * (1. / float (nodeCount t)))
                |> Seq.iter (fun (tp, vp, t) -> genomesWeightedSw.WriteLine $"%f{(tp + vp) / 2.}, %s{show t}")
            printfn "Training done!"


/// An implementation of the Shared IServerApi protocol.
/// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =
    member this.Counter () =
        async {
            logger.LogInformation("Executing {Function}", "counter")
            do! Async.Sleep 1000
            return { value = 10 }
        }

    member this.Build() : IServerApi =
        {
            Counter = this.Counter
        }