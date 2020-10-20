{-# LANGUAGE OverloadedStrings #-}
import           Clay            hiding (contents, menu)
import qualified Clay.Media      as Media
import           Clay.Stylesheet
import           Data.Monoid     ()
import           Prelude         hiding (all, div, span)

nil, u1, u2, u3, u4 :: Size LengthUnit
nil = unit 0
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

unit :: Double -> Size LengthUnit
unit = px . (* 24)

pageWidth :: Size LengthUnit
pageWidth = unit 45

whenNarrow :: Css -> Css
whenNarrow = query (MediaType "all") [Media.maxWidth pageWidth]

whenWide :: Css -> Css
whenWide = query (MediaType "all") [Media.minWidth pageWidth]

box :: Css
box = boxSizing borderBox

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)

-- http://www.bestwebfonts.com/
sourceCodePro, sourceSansPro, montserrat, amaranth, quando :: Css
sourceCodePro = fontFamily ["Source Code Pro"] [monospace]
sourceSansPro = fontFamily ["Source Sans Pro"] [sansSerif]
montserrat = fontFamily ["Montserrat"] [sansSerif]
amaranth = fontFamily ["Amaranth"] [serif]
quando = fontFamily ["Quando"] [serif]

baseFont, monoSpace, navbarFont, headerFont :: Css
monoSpace = sourceCodePro
baseFont = sourceSansPro -- montserrat
navbarFont = amaranth
headerFont = quando

-- colours
hlC, blC, h2C, txC, nvC :: Color
hlC = "#1FE0B3"  -- highlight
blC = "#8A37A3"  -- block     #B32FE0 adjusted
h2C = "#E0B31F"  -- header 2
txC = lightgrey  -- text
nvC = grey       -- nav

-- actual css blocks
site :: Css
site = body ?
  do background  (rgb 20 20 20, url "../images/bkg.png")      -- black
     baseFont
     whenWide $ fontSize (vw 0.8)
     whenNarrow $ fontSize (pt 10)
     sym margin  (pt 0)
     lineHeight  (pct 130)
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
  do centered
     marginBottom (unit 5)

centered :: Css
centered =
  do box
     whenWide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     whenNarrow $ width (pct 95)

contents :: Css
contents = ".content" ? do
     color txC
     padding u1 u1 u2 u1
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

menu :: Css
menu = nav ? do
    navFont
    navList
    whenWide $ marginTop (unit 1.5)
    paddingLeft u1
    lineHeight  u1

navFont :: Css
navFont =
  do navbarFont
     color txC
     fontSize      (em 1.4)
     lineHeight    (em 1.4)
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
        alignCenter
        textDecoration none
        fontWeight bold
        whenWide $ marginRight (Clay.rem 1)
        whenNarrow $ do
          sym margin auto
          width (pct 40)
          marginBottom (Clay.rem 0.35)
        borderBottom solid (px 3) transparent
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
  lineHeight (pct 130)
  p ? textAlign (alignSide sideLeft)

sectionBlock :: Css
sectionBlock = section ? do
    Main.meta
    float floatLeft
    whenWide $ width (pct 74)
    whenNarrow $ width (pct 100)

asideBlock :: Css
asideBlock = aside ? do
    alignCenter
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
    borderLeft solid (px 25) blC
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
    alignCenter

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

myCode :: Css
myCode = ".code" ? do
    sym margin (em 1)

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

-- http://webdesignerwall.com/tutorials/css-elastic-videos
videoContainer :: Css
videoContainer = ".videoContainer" ? do
    position relative
    paddingBottom (pct 56.25) -- 16:9
    paddingTop (px 25)
    height (px 0)
    "iframe" ? do
        position absolute
        top (px 0)
        left (px 0)
        width (pct 100)
        height (pct 100)

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
     videoContainer
     myCode

-- extra css for codeblocks
-- https://github.com/Anomareh/pygments-styles-dump
