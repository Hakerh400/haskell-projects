module Lisp (
  s,
  v,
  ident,
  -- ident',
  -- list,
  -- uni,
  -- n,
  -- m,
  -- isNat,
  -- isInt,
  -- getNat,
  -- getInt,
  -- len,
  -- lenp,
  -- lenm,
  -- e,
  -- a,
  -- ta,
  -- empty,
  -- t,
  -- fst,
  -- snd,
  -- last,
  -- elems,
  -- chNum,
  -- getCh,
  -- topDown,
  -- bottomUp,
  -- iter,
  -- cont,
  -- break,
  -- toStr,
  -- spacingType,
  -- spacingStart,
  -- assert,
  -- err,
  -- warn
) where

import Types
import Error

type M = Either String

s :: Node -> M Bool
s node = case getElem node of
  Ident _ -> Right True
  _       -> Right False

v :: Node -> M Bool
v node = case getElem node of
  List _ -> Right True
  _      -> Right False

ident :: Node -> M ()
ident node = case getElem node of
  Ident _ -> pure ()
  _       -> err node "Expected an identifier, but got a list"

ident' :: Node -> String -> M ()
ident' node name = do
  ident node
  let Ident s = getElem node
  if s == name
    then pure ()
    else err node $ concat ["Expected ", show name, ", but got ", show s]

err :: Node -> String -> M a
err node msg = let
  file = getFile node
  pos = getPos node
  in Left $ show Error {
    getErrFile = file,
    getErrPos = pos,
    getMsg = msg
  }