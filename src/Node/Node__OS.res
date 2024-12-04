/* Node OS API */

@bs.module("os") external type_: unit => string = "type"

@module("node:os") @val external arch: unit => string = "arch"