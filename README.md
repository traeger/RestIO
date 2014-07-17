RestIO
======

<a href="http://en.wikipedia.org/wiki/Standard_streams" target="_blank">Standard IO</a>
via a Restful Interface using the <a href="https://hackage.haskell.org/package/warp" target="_blank">fast Http-Server Warp</a>

Use
------

###### linux / macos

```
./RestIO AnyInteractiveProgramName
```

open `http://localhost:3000` in a browser of your choice, enjoy.

a list of optional parameters is avaliable via
```
./RestIO --help
```

###### windows

```
RestIO.exe AnyInteractiveProgramName
```

open `http://localhost:3000` in a browser of your choice, enjoy.

a list of optional parameters is avaliable via
```
RestIO.exe --help
```

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
