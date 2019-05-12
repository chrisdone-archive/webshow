# webshow

Run `webshow` of a directory and get pretty browsing of the data structure.

## Usage

Supports only Haskell `Show` values at the moment.

```
Usage: webshow [--version] [--help] [-p|--port ARG] [-d|--directory ARG]
  Show printed output from languages

Available options:
  --version                Show version
  --help                   Show this help text
  -p,--port ARG            Port number to listen on
  -d,--directory ARG       Directory to look at
```

E.g.

```
$ webshow -d /my/path -p 1234
```
Put a file like `[1,2,3]` in `x.hs` in the `/my/path` directory and then browse to it.

In my case I've made a dir `/webshow` and then when I want to view something I do

```haskell
writeFile "/webshow/thing.hs" (show thing)
```

And then go to `http://localhost:1234/thing.hs`.

## Example

You click the constructor names or parentheses or list brackets to expand/collapse them interactively, like web browser's consoles that view JS objects.

<img src="https://i.imgur.com/ZnO5wBp.png">
