# ocaml-snippet

> [!NOTE]
> This project has not been extensively tested and may produce unexpected results.

`ocaml-snippet` is a tool designed to extract VSCode snippets from OCaml project files and output them in JSON format.

This project was inspired by [cargo-snippet](https://github.com/hatoo/cargo-snippet).

## Features

- Extracts code snippets with the `[@@snippet]` attribute from OCaml files.
- Converts the extracted snippets into VSCode snippet format.
- Recursively processes all OCaml files in the specified directory.

## Usage

### With Nix

You can run the tool using Nix with the following command:

```sh
nix run github:s3igo/ocaml-snippet -- PROJECT_DIR
```

### Without Nix

To build and run the tool without Nix, use dune.

## Example

Given the following files:

```sh
$ ls
baz.ml foobar.ml
```

`foobar.ml:`

```ml
let foo_func x =
  x + 1
[@@snippet foo]

let bar_binding = "bar" [@@snippet "bar"]
```

`baz.ml`

```ml
type baz_type = {
  baz : int;
  qux : string;
} [@@snippet baz]
```

Running the tool:

```sh
$ nix run github:s3igo/ocaml-snippet -- .
```

Produces the following JSON output:

```json
{
  "baz": {
    "prefix": "baz",
    "body": [ "type baz_type = {", "  baz : int;", "  qux : string;", "}" ],
    "description": "Generated snippet"
  },
  "foo": {
    "prefix": "foo",
    "body": [ "let foo_func x =", "  x + 1" ],
    "description": "Generated snippet"
  },
  "bar": {
    "prefix": "bar",
    "body": [ "let bar_binding = \"bar\"" ],
    "description": "Generated snippet"
  }
}
```
