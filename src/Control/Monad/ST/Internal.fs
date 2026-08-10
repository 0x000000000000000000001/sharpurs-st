let map_ = 
    fun (f: obj) -> fun (a: obj) ->
        let a' = a :?> (unit -> obj)
        (fun () -> Sharpurs_Prelude.sharpurs_apply f (a' ())) :> obj

let bind_ = 
    fun (a: obj) -> fun (f: obj) ->
        let a' = a :?> (unit -> obj)
        (fun () -> 
            let res = Sharpurs_Prelude.sharpurs_apply f (a' ()) :?> (unit -> obj)
            res ()) :> obj

let pure_ = 
    fun (a: obj) -> 
        (fun () -> a) :> obj

let run = 
    fun (f: obj) -> 
        let f' = f :?> (unit -> obj)
        f' ()

let ``while`` = 
    fun (cond: obj) -> fun (a: obj) ->
        let cond' = cond :?> (unit -> obj)
        let a' = a :?> (unit -> obj)
        (fun () ->
            while (unbox<bool> (cond' ())) do
                a' () |> ignore
            null :> obj) :> obj

let ``new`` = 
    fun (val': obj) -> 
        (fun () -> ref val' :> obj) :> obj

let read = 
    fun (r: obj) -> 
        let r' = r :?> obj ref
        (fun () -> r'.Value) :> obj

let modifyImpl = 
    fun (f: obj) -> fun (r: obj) ->
        let r' = r :?> obj ref
        (fun () ->
            let res = Sharpurs_Prelude.sharpurs_apply f (r'.Value) :?> Map<string, obj>
            r'.Value <- Map.find "state" res
            Map.find "value" res) :> obj

let write = 
    fun (a: obj) -> fun (r: obj) ->
        let r' = r :?> obj ref
        (fun () ->
            r'.Value <- a
            a) :> obj

let ``for`` =
    fun (lo: obj) -> fun (hi: obj) -> fun (f: obj) ->
        let lo' = lo :?> int
        let hi' = hi :?> int
        (fun () ->
            for i = lo' to hi' - 1 do
                let step = Sharpurs_Prelude.sharpurs_apply f (box i) :?> (unit -> obj)
                step () |> ignore
            null :> obj) :> obj

let foreach =
    fun (xs: obj) -> fun (f: obj) ->
        let arr = xs :?> obj[]
        (fun () ->
            for x in arr do
                let step = Sharpurs_Prelude.sharpurs_apply f x :?> (unit -> obj)
                step () |> ignore
            null :> obj) :> obj
