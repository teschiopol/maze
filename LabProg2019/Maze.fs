(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx
open Globals
open Engine


type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Blue
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall



// Riferimenti alla matrice logica
let H = 10
let W = 40

// sapere dove sono i muri per muovermi
type ifwall () =
    member val wall = false with get, set
    member val visite = false with get, set

type cell () =
    member val visite = false with get, set
    member val up = true with get, set
    member val down = true with get, set
    member val left = true with get, set
    member val right = true with get, set

let matri = Array2D.init<cell> H W (fun _ _ -> cell())


let mutable lst = []
let mutable stack = []

// ricavo coordinate in due variabili data la coppia
let first (x,y) = x
    
let second (x,y) = y

// Considero le 4 direzioni possibili
let intorno x y =  [(x, y-1);(x-1, y);(x+1, y);(x, y+1)]

// Verifico se le direzioni sono fuori dai limiti
let rec out_bound lst =
    match lst with
    | [] -> []
    | (x,y)::xs -> if (y < 0 || x > H-1 || x < 0 || y > W-1) then out_bound xs else (x,y)::out_bound xs
   
// Visitate oppure no
let rec visita lst = 
    match lst with 
    | [] -> []
    | (x,y)::xs -> if matri.[x, y].visite = true then visita xs else (x,y)::visita xs
  
// prendo valore a caso della lista
let rec pick lst n= 
    match lst , n with
    | [] , _ -> (100,100)
    | (x,y)::xs , n when n = 0 -> (x,y)
    | (x,y)::xs , n  when n <> 0 -> pick xs (n-1)
    | _ -> failwith "Errore scelta valore"

// generazione logica del labirinto
let rec generate x  y =
    
    if matri.[x,y].visite = false then 
        matri.[x,y].visite <- true
        stack <- [(x,y)]@stack

    lst <- intorno x y
    lst <- out_bound lst
    lst <- visita lst
    let next = rnd_int 0 ((List.length lst)-1)
    let x1, y1 = pick lst next

    if x1 = 100 && stack = [] then matri

    else if x1 = 100 then stack <- List.tail stack ; if stack = [] then matri else generate (first (List.head stack)) (second (List.head stack))
    
    else if x < x1 then  matri.[x1, y1].up <- false; matri.[x, y].down <- false;   generate x1 y1  // giu
        
    else if x > x1 then  matri.[x1, y1].down <- false; matri.[x, y].up <- false; generate x1 y1  // su
                
    else if y < y1 then  matri.[x1, y1].left <- false; matri.[x, y].right <- false; generate x1 y1  // destra   

    else matri.[x1, y1].right <- false; matri.[x, y].left <- false; generate x1 y1  // sinistra   


 // visualizzazione del labirinto 
let mutable altz = 5
let mutable largz = 5

let wallbang = Array2D.init<ifwall> ((H*2)+1) ((W*2)+1) (fun _ _ -> ifwall())

let visualize (en : engine) = 
        
        matri.[H-1,W-1].right <- false // exit

        for i=0 to H-1 do 
            largz <- 5
            for j=0 to W-1 do
                if matri.[i,j].right = true  then
                    wallbang.[i*2, j*2+2].wall <- true; wallbang.[i*2+1, j*2+2].wall <- true ; wallbang.[i*2+2, j*2+2].wall <- true
                    ignore <| en.create_and_register_sprite (image.rectangle(1, 3, pixel.wall , pixel.wall ), largz+2, altz,0)
                if matri.[i,j].down = true then
                    wallbang.[i*2+2, j*2].wall <- true; wallbang.[i*2+2, j*2+1].wall <- true ; wallbang.[i*2+2, j*2+2].wall <- true
                    ignore <| en.create_and_register_sprite (image.rectangle(3, 1, pixel.wall , pixel.wall ), largz, altz+2,0)
                if matri.[i,j].left = true  then
                    ignore <| en.create_and_register_sprite (image.rectangle(1, 3, pixel.wall , pixel.wall ), largz, altz,0)
                if matri.[i,j].up = true then
                    ignore <| en.create_and_register_sprite (image.rectangle(3, 1, pixel.wall , pixel.wall ), largz, altz,0)

                largz <- largz + 2
                
            altz <- altz + 2

        for i=0 to (W*2) do
            wallbang.[0, i].wall <- true 

        for i=0 to (H*2) do
            wallbang.[i, 0].wall <- true 

        

        

                

    