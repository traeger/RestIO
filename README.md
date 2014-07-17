RestIO
======

Standard IO via a Restful Interface

Use
------

###### linux / macos

```
./RestIO AnyInteractiveProgramName
```

open `http://localhost:3000/client/index.html` in a browser of your choice, enjoy.

###### windows

```
RestIO.exe AnyInteractiveProgramName
```

open `http://localhost:3000/client/index.html` in a browser of your choice, enjoy.

Install
------

###### linux / macos

```
runhaskell Setup.lhs configure --user
runhaskell Setup.lhs build
```

copy `/dist/build/RestIO/RestIO` to `/RestIO`

###### windows

```
runhaskell Setup.lhs configure --user
runhaskell Setup.lhs build
```

copy `/dist/build/RestIO/RestIO.exe` to `/RestIO.exe`

Copyright
------

(c) 2014 Marco Traeger
MIT License (see http://opensource.org/licenses/MIT)
