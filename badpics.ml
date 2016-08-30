open Printf
open Mysql

let process_lines in_ch process =
    let rec read_all () =
        try
            let line = input_line in_ch in
            process line;
            read_all ()
        with End_of_file -> () in
    read_all ();
    close_in in_ch

let () =
    let in_ch = open_in Sys.argv.(1) in
    let my = Mysql_config.connection in
    let prep = Prepared.create my "SELECT Idx FROM movies WHERE cover=?" in
    process_lines in_ch (fun line ->
        let cover =
            let pos = String.index line ':' in
            String.sub line 0 pos in
        let rs = Prepared.execute prep [| cover |] in
        match Prepared.fetch rs with
        | None -> printf "rm %s\n" cover
        | Some row ->
            let idx = not_null (*opt*) int2ml row.(0) in
            printf "cp /tmp/covers/%d %s\n" idx cover
    );
    Prepared.close prep
