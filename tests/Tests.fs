module Tests

open System
open Xunit
open Hedgehog
open Hedgehog.Xunit
open Program

[<Property>]
let ``Random numbers are the same`` (x: decimal) (a: decimal) (b: decimal) (c: decimal) =
    x + a = b + c