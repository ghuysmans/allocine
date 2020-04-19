# Allociné

Cette bibliothèque permet d'accéder à l'API Allociné. Elle a besoin :
- d'une implémentation de `Cohttp_lwt.S.Client`
  (par exemple, `Cohttp_lwt_unix.Client`)
- de paramètres de connexion à l'API (voir `Allocine.Api.CONFIG`)

Un utilitaire en ligne de commande permet de tester la lib depuis GNU/Linux.
Ses paramètres de connexion doivent être renseignés dans `config/net_config.ml`,
un modèle se trouve dans le même dossier.

La version Web est laissée comme exercice au lecteur qui peut s'inspirer de la
démo Web d'un autre binding :
[Dvdfr](https://github.com/ghuysmans/dvdfr/blob/master/web/client.ml).
