(require 'asdf)
(asdf:operate 'asdf:load-op 'fiveam)
(compile-file "../internals/WC-internals")
(load "../internals/WC-internals")

(WC-initialise-internals "../internals/")
