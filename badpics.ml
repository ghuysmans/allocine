open Lib
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
    let dl_dir = Sys.argv.(2) in
    let my = Mysql_config.connection in
    let get_idx = Prepared.create my "SELECT Idx FROM movies WHERE cover=?" in
    let set_a = Prepared.create my "UPDATE movies SET allocine=? WHERE idx=?" in
    process_lines in_ch (fun line ->
        let cover =
            let pos = String.index line ':' in
            String.sub line 0 pos in
        let rs = Prepared.execute get_idx [| cover |] in
        match Prepared.fetch rs with
        | None -> printf "rm %s\n" cover
        | Some row ->
            let idx = not_null str2ml row.(0) in
            (* TODO fetch search results, find the right one *)
            let code = 42 in
            ignore (Prepared.execute set_a [| string_of_int code; idx |]);
            (* TODO fetch the poster *)
            let url = "http://localhost/FIXME" in
            let destination = dl_dir ^ "/" ^ idx in
            Lwt_main.run (download url destination);
            printf "cp %s %s\n" destination cover
    );
    Prepared.close get_idx;
    Prepared.close set_a
