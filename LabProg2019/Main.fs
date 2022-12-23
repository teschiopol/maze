(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Main.fs: main code
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Main

open System
open System.Diagnostics
open Globals
open Gfx
open System.IO.Pipes
open System.IO
open Maze
open Automatic
open Engine



// game mode (client)
//

// riferimenti dell'engine 
let W_e = 150
let H_e = 150

// genero due coordinate a caso per dare più casualità alla generazione del maze
let mutable v = rnd_int 0 (W-1)
let mutable o = rnd_int 0 (H-1)
 
let myengine = new engine (W_e, H_e)   

[< NoEquality; NoComparison>]
type state = {
    player : sprite
}
  
let main_game () =
    use p = new Process ()
    p.StartInfo.UseShellExecute <- true
    p.StartInfo.CreateNoWindow <- false
    p.StartInfo.Arguments <- "1"
    p.StartInfo.FileName <- Process.GetCurrentProcess().MainModule.FileName
    ignore <| p.Start ()

    use client = new NamedPipeClientStream (".", Config.log_pipe_name, PipeDirection.Out)
    client.Connect ()
    Log <- new remote_logger (client)

// scelta delle due opzioni    
    printfn "%s Scegliere modalità:%s m -> manuale%s other -> automatico " Environment.NewLine Environment.NewLine Environment.NewLine
    
    let line = Console.ReadLine()

    if line = "m" then 

        // modalità interattiva

        let maze =  Maze.generate o v 
        Maze.visualize myengine

        let my_update (key: ConsoleKeyInfo) (screen: wronly_raster) (st : state) =
            
            let dx, dy =
                match key.KeyChar with
                | 'w'  ->  0 , -1
                | 's'  ->  0 , 1
                | 'a'  -> -1 , 0
                | 'd'  -> 1 , 0
                | _ -> 0 , 0
            
            if st.player.x = (5. + (2.*(float)W)) && st.player.y = (4. + (2.*(float)H)) then st.player.move_by(0,0) ;  screen.draw_text (" Complimenti. Hai vinto!!! ", 5, ((H*2) + 10),  Color.Yellow, Color.Magenta) 
            else if  wallbang.[(int)st.player.y + dy - 5, (int)st.player.x + dx - 5].wall = true then st.player.move_by(0,0) else st.player.move_by(dx,dy)
            
            st, key.KeyChar = 'q'

        let player = myengine.create_and_register_sprite (image.rectangle(1, 1, pixel.filled Color.DarkGreen, pixel.filled Color.DarkGreen), 6,6,0)

        let st0 = {
            player = player
        }

        myengine.loop_on_key my_update st0 

        0

    else 
        
        // mostra in modo del tutto autonomo il percorso corretto, per step
        let maze =  Maze.generate o v 
        Maze.visualize myengine
        
        let mutable res = Automatic.supreme 1 1

        res <- List.rev res

        let auto_update (key: ConsoleKeyInfo option) (screen: wronly_raster) (st : state) =
            
            // vittoria
            if st.player.x  = (4. + (2.*(float)W )) && st.player.y  = (4. + (2.*(float)H )) then st.player.move_by(0,0) ;  screen.draw_text (" Complimenti. Hai vinto!!! ", 5, ((H*2) + 10),  Color.Yellow, Color.Magenta) 

            else if res <> [] then let a = second(List.head res)
                                   let b = first (List.head res)
                                   res <- List.tail res

                                   ignore <| myengine.create_and_register_sprite (image.rectangle(1, 1, pixel.autom Color.Cyan, pixel.autom Color.Cyan), a+5,b+5,0) ;
                                   st.player.x <-  (float)a + 5.  ;
                                   st.player.y <- (float)b + 5.
                                   
            st, st.player.x  = (4. + (2.*(float)W )) && st.player.y  = (4. + (2.*(float)H )) && key.IsSome 

        let player = myengine.create_and_register_sprite (image.rectangle(1, 1, pixel.autom Color.Cyan, pixel.autom Color.Cyan), 6,6,0)

        let st1 = {
            player = player
        }

        myengine.loop auto_update st1
        
        0


// log mode (server)
//

let main_log_server () =
    Log.msg "log server process started"
    Console.Title <- Config.log_console_title
    let server = new NamedPipeServerStream (Config.log_pipe_name, PipeDirection.In)
    Log.msg "waiting for incoming connection..."
    server.WaitForConnection ()
    Log.msg "client connected"
    use r = new StreamReader (server)
    while not r.EndOfStream do
        try
            let fg = r.ReadLine ()
            let parse c = Enum.Parse (typeof<Color>, c) :?> Color
            let s = r.ReadLine().Replace (Config.log_pipe_translate_eol, '\n')
            Console.ForegroundColor <- parse fg
            Console.WriteLine s
            Console.ResetColor ()
        with e -> Log.error "exception caught:%s\nstack trace: %O" e.Message e

    Log.warn "EOF reached: quitting."
    0

// main 
//

[<EntryPoint>]
let main argv = 
    #if TEST
    Test.main ()
    printfn "\nPress any key to quit..."
    Console.ReadKey () |> ignore
    0
    #else
    if argv.Length > 0 then 
        main_log_server ()
    else
        let code = main_game ()
        printfn "\nPress any key to quit..."
        Console.ReadKey () |> ignore
        code
    #endif

