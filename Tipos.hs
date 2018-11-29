module Tipos where

type Simbolo = Char
type Alfabeto = [Simbolo]
type Palabra = [Simbolo]
type Estado = Integer
type Estados = [Estado]
type Transicion = (Estado, Simbolo, Estado)
type Transiciones = [Transicion]
type Af = (Estados, Alfabeto, Transiciones, Estado, Estados)
