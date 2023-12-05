# CONTRIBUTING

## SETUP

> Make sure running the below commands in `/src`.

1. Create a sandbox with opam

```
opam switch create spice 4.14.0
```

2. Install dependencies

```
opam install . --deps-only
```

3. Build

```
dune build
```

4. Test

Make sure running tests in `/test`

```
cd test

(install dependencies)
bun i

(build --watch)
bun run res:clean && bun run res:watch
```
