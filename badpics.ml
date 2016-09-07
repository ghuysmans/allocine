open Lib
open Printf
open Mysql
open Lwt
open Allocine
open Movies

let () = Lwt_main.run (
    let in_ch = open_in Sys.argv.(1) in
    let dl_dir = Sys.argv.(2) in
    let my = Mysql_config.connection in
    let get_idx =
        let sql = "SELECT m.Idx, Title_Orig, " ^
            "CONCAT(forename, ' ', lastname) AS name " ^
            "FROM movies m LEFT JOIN directors d ON m.MainDirector=d.Idx " ^
            "WHERE cover=?" in
        Prepared.create my sql in
    let set_a = Prepared.create my "UPDATE movies SET allocine=? WHERE idx=?" in
    let rst_c = Prepared.create my "UPDATE movies SET cover=NULL WHERE idx=?" in
    let searches = ref [] in
    process_lines in_ch (fun line ->
        let cover =
            let pos = String.index line ':' in
            String.sub line 0 pos in
        let rs = Prepared.execute get_idx [| cover |] in
        match Prepared.fetch rs with
        | None -> printf "rm %s\n" cover
        | Some row ->
            let idx = not_null str2ml row.(0) in
            let title_orig = not_null str2ml row.(1) in
            let main_director = match opt str2ml row.(2) with
                | None -> None
                | Some d -> Some (clean_director d) in
            searches := (search title_orig 10 >>= fun results ->
                match process_one (function
                    | Movie x
                    | Series x ->
                        (match main_director with
                        | None -> Some x
                        | Some director ->
                            let l = List.map clean_director x.directors in
                            if List.exists ((=) director) l then
                                Some x
                            else
                                None)
                    | _ -> None
                ) results with
                | None ->
                    printf "echo unmatched %s (%s) by %s\n"
                        title_orig idx (string_of_option main_director);
                    printf "rm %s\n" cover;
                    ignore (Prepared.execute rst_c [| idx |]);
                    return ()
                | Some x ->
                    let params = [| string_of_int x.ms_code; idx |] in
                    ignore (Prepared.execute set_a params);
                    match x.poster with
                    | None ->
                        printf "echo poster deleted from Allocine %s (%s)\n"
                            cover title_orig;
                        return ()
                    | Some url ->
                        let destination = dl_dir ^ "/" ^ idx in
                        printf "cp %s %s\n" destination cover;
                        if exists destination then
                            return ()
                        else
                            download url destination
            ) :: !searches
    );
    join !searches >>= (fun _ ->
        Prepared.close get_idx;
        Prepared.close set_a;
        Prepared.close rst_c;
        return ()
    )
)
