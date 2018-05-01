# calcetthon

Backend application to collect our foosball results

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

## Swagger Documentation

It is possible to generate an up-to-date swagger documentation using

```
stack exec swagger-exe
```

You will then find the `swagger.json` file in the `swagger` directory
