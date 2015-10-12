{-# LANGUAGE CPP #-}

module CPPTest where

#define F(x) x

x = F(id) F(1)

