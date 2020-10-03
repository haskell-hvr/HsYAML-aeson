# HsYAML-aeson: JSON to YAML Adapter  
[![Hackage](https://img.shields.io/hackage/v/HsYAML-aeson.svg)](http://hackage.haskell.org/package/HsYAML-aeson)  

## About  

The `YAML 1.2` format provides a much richer data-model and feature-set than the JavaScript Object Notation (JSON) format.  
However, sometimes it's desirable to ignore the extra capabilities and treat YAML as if it was merely a more convenient markup format for humans to write JSON data. 

So, `HsYAML-aeson` module provides a compatibility layer atop `HsYAML` which allows decoding YAML documents in the more limited JSON data-model while also providing convenience by reusing aeson's `FromJSON` instances for decoding the YAML data into native Haskell data types.

See what's changed in recent (and upcoming) releases [here](CHANGELOG.md).

## Documentation
The primary API documentation for `HsYAML-aeson` is its Haddock documentation which can be found [here](http://hackage.haskell.org/package/HsYAML-aeson).  

## Installation

Install the `HsYAML-aeson` package. 
```
cabal install HsYAML-aeson
```

See [dependencies](http://hackage.haskell.org/package/HsYAML-aeson).

## Developers and Maintainers

The library is developed and maintained by [Herbert Valerio Riedel](https://github.com/hvr)

# License

This project is licensed under X-SPDX-License-Identifier: [GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)