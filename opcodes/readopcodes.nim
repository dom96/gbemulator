import os, parsecsv, streams, parseutils, strutils

type
  Opcode = tuple
    mnemonic: string
    len: int ## Length in bytes
    cycles: int ## Duration in cycles
    idleCycles: int
    z, n, h, c: char ## "0" denotes the flag is reset after instruction
                     ## "1" denotes the flag is set
                     ## "Z/N/H/C" the flag is affected as expected by its function.
                     ## '-' denotes no change

var s = newFileStream("prefixcb", fmRead)
if s == nil: quit("cannot open the file" & paramStr(1))

proc parseRow(r: string): Opcode =
  if r == "":
    return ("", 0, 0, 0, '-', '-', '-', '-')

  var i = 0
  i.inc skipUntil(r, {'\t'}, i)
  result.mnemonic = r[0 .. i-1]
  i.inc

  i.inc parseInt(r, result.len, i)

  i.inc skipWhitespace(r, i)

  i.inc parseInt(r, result.cycles, i)

  if r[i] == '/':
    i.inc
    i.inc parseInt(r, result.idleCycles, i)
  else:
    result.idleCycles = result.cycles

  i.inc skipWhitespace(r, i)

  result.z = r[i]
  i.inc 2
  result.n = r[i]
  i.inc 2
  result.h = r[i]
  i.inc 2
  result.c = r[i]

var x: CsvParser
open(x, s, "")
var str = ""
while readRow(x):
  str.add("  ")
  for val in items(x.row):
    let foo = parseRow(val)
    echo(foo.repr)
    str.add("(\"$#\", $#, $#, $#, '$#', '$#', '$#', '$#'), " %
      [foo.mnemonic, $foo.len, $foo.cycles, $foo.idleCycles, $foo.z, $foo.n, $foo.h, $foo.c])
  str.add("\n")
writeFile(getCurrentDir() / "output.nim", str)
close(x)
