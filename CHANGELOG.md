# Revision history for variety

## 0.2.0.0 -- 2025-07-22

* Altered the type/interface for all BitVec decoding functions to
  leverage the prefix property, returning `Maybe (a, BitVec)` which
  includes the remaining bits upon successful decoding and `Nothing` in
  case of a lack of bits instead of throwing an error.
* Specify ranges for which each Elias codes are optimal.
* `BitVec` interfaces added to `Combinatorics` to facilitate
  serialization and interactivity with the other modules.

## 0.1.0.2 -- 2025-06-05

* Improved documentation.

## 0.1.0.1 -- 2025-06-05

* Removed dependency on extra.

## 0.1.0.0 -- 2025-06-04

* First version.
