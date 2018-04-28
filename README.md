### GXTAGS-EXT

This package redefines the gxtags executable for gerbil scheme. The underlying data structure has
also been redone and tags are now written out in json format.

An indexing system has been implemented. This can be used for having differnet tags for different workspaces for example.

All queries and writes should be properly implemented through actors. This means full concurency.
