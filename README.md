# Allociné

Cette bibliothèque permet d'accéder à l'API Allociné. Elle a besoin :
- d'une implémentation de `Cohttp_lwt.S.Client`
  (par exemple, `Cohttp_lwt_unix.Client`)
- de paramètres de connexion à l'API (clés, URL...)
