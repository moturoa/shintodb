![](https://badgen.net/badge/shintolabs/production/green)
# shintodb


Database layer for shinto applications.

The only allowed way to make connections with our databases, since passwords are locally encrypted
with an environment variable specific to you.

See Confluence, section [Database config: password encryption](https://shintolabs.atlassian.net/wiki/spaces/PRODDEV/pages/2152235048/Developing+Deploying+Shiny)

## Docs

```
help(package=shintodb)
```

See especially `?databaseClass`, a useful class to make a database layer, with:

```
db <- shintodb::databaseClass$new(what = "data_entry", schema = "data_schema")
```

For an entry in your config.yml named 'data_entry', to connect with default schema 'data_schema'.

## Contact

Wesley Brants
