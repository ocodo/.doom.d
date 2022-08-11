# Make yasnippet from region

 - - -
## Functions

### make-yas-from-region [command]

Make a yasnippet from the current region `begin` `end`.

You should use standard snippet formatting in place, e.g. $1,
${1:default value} and so on.  See the yasnippet docs for more info.

You'll be prompted for a name, trigger key and when `prefix-arg` is
specified, a snippet group.

<sup>function signature</sup>
```lisp
(make-yas-from-region (begin end))
```

- - -
