# webshow

Run `webshow` in a directory and get pretty browsing.

## Usage

Supports only Haskell Show values at the moment.

```
Usage: webshow [--version] [--help] [-p|--port ARG] [-d|--directory ARG]
  Show printed output from languages

Available options:
  --version                Show version
  --help                   Show this help text
  -p,--port ARG            Port number to listen on
  -d,--directory ARG       Directory to look at
```

Example
```
$ webshow -d /my/path -p 1234
```
Put a file like `[1,2,3]` in `x.hs` in the `/my/path` directory and then browse to it.

## Example

<img src="https://i.imgur.com/ZnO5wBp.png">
