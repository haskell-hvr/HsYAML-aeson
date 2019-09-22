See also http://pvp.haskell.org/faq

## 0.2.0.0

This release incorporates the work from [Vijay Tadikamalla's GSOC 2019 Project](https://vijayphoenix.github.io/blog/gsoc-the-conclusion/).

* **Breaking change**: The result types of `decode1`, `decode1'`, `decodeValue`, and `decodeValue'` have been changed from `Either String _` to `Either (Pos,String) _` to mirror the error-reporting change in the `HsYAML-0.2` API
* New functions `encode1`, `encode1Strict`, `encodeValue`, and `encodeValue'` for serializing JSON Values as YAML documents
* New convenience function `decode1Strict`
* New (orphan) `instance ToYAML Data.Aeson.Value`

----

## 0.1.0.0

* First release. Released on an unsuspecting world.
