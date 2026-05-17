# Habanga

## Mise en place

Après avoir cloné le dépôt, activer les hameçons git :

```sh
git config core.hooksPath .githooks
```

Les hameçons gèrent Git LFS et vérifient que les objets LFS sont bien
envoyés vers le serveur configuré dans `.lfsconfig`.
