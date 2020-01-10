package io.deftrade
package model

/**
  * Contract specification, representation and evaluation,
  * following the ''Composing Contracts'' work
  * by Simon Peyton Jones and Jean-Marc Eber circa 2001.
  *
  * This is a work in progress.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - [[https://www.lexifi.com/apropos/ LexiFi]] sells a mature implementation of this technology in OCaml (founded by J.C. Eber, who co-authored above papers.)
  *   - Anton van Straaten's [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ Haskell implementation]]
  *       - highly referenced; no longer maintained
  *       - see in particular his section ''A note about the original papers''
  *   - Channing Walton's [[https://github.com/channingwalton/scala-contracts scala implementation]] (also not actively maintained)
  *   - [[http://netrium.org/ Netrium]]'s open source
  *   [[http://hackage.haskell.org/package/netrium Haskell implemenation]] (status?)
  *   - Financial DSLs [[http://www.dslfin.org/resources.html resource page]]
  *
  */
package object contracts