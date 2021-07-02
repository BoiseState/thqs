# Type Hierarchy Query System  #

This is a simple Java type hierarchy query system.It uses the [Soot][soot-oss]
API to parse and load Java bytecode code for analysis.  The program currently
will print some findings related to structural similarity comparisons for the
input program.

It's also usable from the [Clojure][clojure] REPL for more exploratory
analysis.

## Building ##

`thqs` is written in [Clojure][clojure] and uses the [Leiningen][lein] build
tool.  To build a standalone JAR, run `lein uberjar`.

It's also possible to use [nix][nix] to start a shell that contains the
necessary version of [Clojure][clojure] and [Leiningen][lein] installed for use
via `nix-shell`.

## Experiments ##

There is an `experiments` directory in the root of the project that contains
the [OrgMode][orgmode] notebooks for the various experiments performed so far.

## License ##

Copyright © 2020 Kenny Ballou

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, see <https://www.gnu.org/licenses>.

Classpath Exception -- Additional permission under GNU GPL version 3 section 7

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.

[soot-oss]: https://soot-oss.github.io/soot/

[clojure]: https://clojure.org/

[lein]: https://leiningen.org/

[nix]: https://nixos.wiki/wiki/Nix

[orgmode]: https://orgmode.org/
