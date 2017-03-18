# kixi.mallet

A Clojure library for interacting with Mallet. This is achieved in two ways currently:

1. A set of boot tasks wrapping Mallet's command-line utility
2. Macros for defining Mallet pipes in Clojure

The boot tasks are designed to be chained with other tasks, facilitating ad-hoc exploratory analysis.

## Pipes

Pipes can be defined thusly:

```clojure
(ns my.project
  (:require [kixi.mallet.pipe :refer [defpipe]]
            [clojure.string :as str]))

;; Pipes are defined as transducers:
;; map, mapcat, filter, etc
;; will all work as expected.

(defpipe DownCase
  (map (fn [instance]
         (update instance :data str/lower-case))))
```

The pipe can now be passed to Mallet as `my.project.DownCase`.

Note: the `my.project` namespace will need to be aot compiled.

## License

Copyright © 2017 Mastodon C

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
