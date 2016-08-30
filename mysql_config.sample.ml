open Mysql
let connection = connect ~options:[SET_CHARSET_NAME "utf8"] {
    dbhost = None;
    dbname = Some "movman";
    dbport = None;
    dbpwd = None;
    dbuser = None;
    dbsocket = None
}
