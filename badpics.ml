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
            "FROM movies m INNER JOIN directors d ON m.MainDirector=d.Idx " ^
            "WHERE cover=?" in
        Prepared.create my sql in
    let set_a = Prepared.create my "UPDATE movies SET allocine=? WHERE idx=?" in
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
            let main_director = not_null str2ml row.(2) in
            searches := (search title_orig 10 >>= fun results ->
                match process_one (fun x ->
                    match x with
                    | Movie x | Series x
                        when List.exists ((=) main_director) x.directors ->
                        Some x
                    | _ -> None
                ) results with
                | None ->
                    printf "echo failed idx %s\n" idx;
                    return ()
                | Some x ->
                    let params = [| string_of_int x.ms_code; idx |] in
                    ignore (Prepared.execute set_a params);
                    match x.poster with
                    | None ->
                        return ()
                    | Some url ->
                        let destination = dl_dir ^ "/" ^ idx in
                        printf "cp %s %s\n" destination cover;
                        download url destination
            ) :: !searches
    );
    join !searches >>= (fun _ ->
        Prepared.close get_idx;
        Prepared.close set_a;
        return ()
    )
)
