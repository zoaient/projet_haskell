# Compilation et tests: 

à la racine : 
```cabal run```

# Bibliothèques externes utilisées 

Aucune

# Architecture et conception.

Nous avons choisi d'utilliser des structures de données récursvies plutot que des tableaux de string pour pouvoir representer les types donnés. 

Pour l'implémentation, nous sommes partis d'un schema décomposant les différentes parties d'une URL puis nous avons implémenté le parseur via des fonctions encapsuluées les unes dans les autres. Le tout en utilisant principalement une syntaxe en "case"

La décomposition de l'url se fait via le schéma : 

# Fonctionnalités 

Nous avons fait l'intégralité de la partie 1 et nous sommes allés jusqu'au parsing de la partie 2. 