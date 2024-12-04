{-# LANGUAGE OverloadedStrings #-}
import           Clay            hiding (contents, menu)
import qualified Clay.Media      as Media
import           Clay.Stylesheet
import           Data.Monoid     ()
import           Prelude         hiding (all, div, span)

unit :: Double -> Size LengthUnit
unit = px . (* 24)

pageWidth :: Size LengthUnit
pageWidth = unit 45

whenNarrow, whenWide :: Css -> Css
whenNarrow = query (MediaType "all") [Media.maxWidth pageWidth]
whenWide = query (MediaType "all") [Media.minWidth pageWidth]

-- http://www.bestwebfonts.com/
baseFont, monoSpace, navbarFont, headerFont :: Css
baseFont = fontFamily ["Source Sans Pro"] [sansSerif]
monoSpace = fontFamily ["Source Code Pro"] [monospace]
navbarFont = fontFamily ["Amaranth"] [serif]
headerFont = fontFamily ["Quando"] [serif]

-- colours
hlC, blC, h2C, bgC, txC, nvC :: Color
hlC = "#1FE0B3"  -- highlight
blC = "#8A37A3"  -- block     #B32FE0 adjusted
h2C = "#E0B31F"  -- header 2
bgC = (rgb 40 40 40) -- background
txC = lightgrey  -- text
nvC = grey       -- nav

-- actual css blocks
site :: Css
site = body ?
  do background  bgC
     baseFont
     whenWide $ fontSize (pt 14)
     whenNarrow $ fontSize (pt 13)
     sym margin  (pt 0)
     lineHeight  (pct 150)
     overflowY   scroll

header1 :: Css
header1 = h1 ?
  do headerFont
     margin (em 0.2) 0 (em 0.2) 0

header2 :: Css
header2 = h2 ?
  do headerFont
     fontSize (em 1.1)
     fontWeight bold
     margin (em 0.4) 0 (em 0.4) 0
     paddingTop (em 1.2)
     color h2C

header3 :: Css
header3 = h3 ?
  do headerFont
     margin (em 0.2) 0 (em 0.2) 0
     marginBottom (em (- 0.5))

divColumn :: Css
divColumn = body |> "div" ?
  do boxSizing borderBox
     whenWide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     whenNarrow $ width (pct 95)
     marginBottom (unit 5)

contents :: Css
contents = ".content" ? do
     color txC
     padding (unit 1) (unit 1) (unit 2) (unit 1)
     a ? do
       color inherit
       -- https://css-tricks.com/styling-links-with-real-underlines/
       textDecorationColor nvC
       "text-decoration-thickness" -: "0.125em"
       "text-underline-offset" -: "2.5px"
       link & do
         transitions [("text-decoration-color", sec 0.2, ease, sec 0.04)]
       hover & do
           textDecorationColor hlC

-- navigation bar
menu :: Css
menu = nav ? do
    navFont
    navList
    whenWide $ marginTop (unit 1.5)
    paddingLeft (unit 1)
    lineHeight  (unit 1)

navFont :: Css
navFont =
  do navbarFont
     color txC
     fontSize      (em 1.8)
     lineHeight    (em 1.8)
     textTransform lowercase

navList :: Css
navList = ul ? do
  listStyleType none
  display inline
  li ? do
    whenWide $ float floatLeft
    a ? do
      display block
      whenWide $ padding (px 0) (px 5) (Clay.rem 0.5) (px 5)
      link & do
        color nvC
        textAlign (alignSide sideCenter)
        textDecoration none
        fontWeight bold
        whenWide $ marginRight (Clay.rem 1)
        whenNarrow $ do
          sym margin auto
          width (pct 40)
          marginBottom (Clay.rem 0.75)
        borderBottom (px 3) solid transparent
        transitions [("color", sec 0.2, ease, sec 0.04)]
        transitions [("borderColor", sec 0.2, ease, sec 0.04)]
      hover & do
        color hlC
        borderColor hlC

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     floatRight
     marginRight (unit (2))
     Clay.span ? display block

articleBlock :: Css
articleBlock = article ? do
    Main.meta
    margin (px 5) auto (px 5) auto
    width (pct 100)
    overflow hidden

-- for pandtoc-citeproc references
refsPubs :: Css
refsPubs = div # ".references" ? do
  fontSize (pct 100)
  lineHeight (pct 135)
  div # ".csl-entry" ? paddingBottom (Clay.rem 1.0)

sectionBlock :: Css
sectionBlock = section ? do
    Main.meta
    float floatLeft
    whenWide $ width (pct 74)
    whenNarrow $ width (pct 100)

asideBlock :: Css
asideBlock = aside ? do
    textAlign (alignSide sideCenter)
    whenWide $ do
      float floatRight
      width (pct 25)
    whenNarrow $ do
      float Clay.none
      width (pct 100)
    a ? do
      textDecoration none

pBlock :: Css
pBlock = p ? do
    textAlign justify
    "text-justify" -: "inter-word"

blockQuote :: Css
blockQuote = blockquote ? do
    borderLeft (px 25) solid blC
    borderRadius (px 10) 0 0 (px 2)
    marginLeft (em 0)
    marginRight (em 3)
    paddingLeft (em 0.5)
    p ? display inline

footerBlock :: Css
footerBlock = footer ? do
    float floatLeft
    width (pct 100)
    margin (px 30) auto auto auto
    fontSize (em 1)
    textAlign (alignSide sideCenter)

contactTable :: Css
contactTable = table # ".contact" ? do
    width (pct 100)
    borderSpacing (px 0)
    fontSize (pct 90)
    lineHeight (pct 115)
    color txC

preBlock :: Css
preBlock = pre ? do
    monoSpace
    fontSize (vw 0.7)
    lineHeight (pct 140)
    "white-space" -: "pre-wrap"       -- /* css-3 */
    "white-space" -: "-moz-pre-wrap"  -- /* Mozilla, since 1999 */
    "white-space" -: "-pre-wrap"      -- /* Opera 4-6 */
    "white-space" -: "-o-pre-wrap"    -- /* Opera 7 */
    "word-wrap"   -: "break-word"     -- /* Internet Explorer 5.5+ */

imgBlock :: Css
imgBlock = img ? do
    maxWidth (pct 90)
    height auto
    width auto

imgDisp :: Css
imgDisp = img # ".displayed" ? do
    display block
    marginLeft auto
    marginRight auto
    sym borderRadius (px 400)

-- main - required for hakyll
main :: IO ()
main = putCss $
  do site
     divColumn
     contents
     header1
     header2
     header3
     articleBlock
     refsPubs
     sectionBlock
     asideBlock
     pBlock
     blockQuote
     footerBlock
     menu
     contactTable
     preBlock
     imgBlock
     imgDisp
     ".code" ? do sym margin (em 1)
     selection & do
       background h2C
       color bgC

-- extra css for codeblocks
-- https://github.com/Anomareh/pygments-styles-dump
