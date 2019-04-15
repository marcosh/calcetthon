# calcetthon

Backend application to collect foosball results.

Developed while working in [MVlabs](https://www.mvlabs.it/), where foosball is a serious thing.

## Run the project

Compile the code with

```
stack build
```

Then run it with

```
stack exec calcetthon-exe
```

## Tests

Run the tests with

```
stack test
```

## Development

Live recompiling after saving a project file with

```
stack build --file-watch
```

Bring up the postgres database and pgadmin with

```
docker-compose up
```

Postgres will be exposed on the default port 5432. The database is called `calcetthon` and also the username and the password are `calcetthon`.

Pgadmin is accessible on the default 5050 port

## Swagger Documentation

It is possible to generate an up-to-date swagger documentation using

```
stack exec swagger-exe
```

You will then find the `swagger.json` file in the `swagger` directory
