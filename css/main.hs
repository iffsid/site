{-# LANGUAGE OverloadedStrings #-}
import           Clay            hiding (contents, menu)
import qualified Clay.Media      as Media
import           Clay.Stylesheet
import           Data.Monoid     ()
import           Data.Text       (Text)
import           Prelude         hiding (all, div, span)
-- helpers
-- nil :: Size Abs
-- nil = px 0

nil, u1, u2, u3, u4 :: Size LengthUnit
nil = unit 0
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

unit :: Double -> Size LengthUnit
unit = px . (* 24)

pageWidth :: Size LengthUnit
pageWidth = unit 43

whenNarrow :: Css -> Css
whenNarrow = query (MediaType "all") [Media.maxWidth pageWidth]

whenWide :: Css -> Css
whenWide = query (MediaType "all") [Media.minWidth pageWidth]

box :: Css
box = boxSizing borderBox

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)

-- http://www.bestwebfonts.com/
sourceCodePro, montserrat, amaranth, quando :: Css
-- dejaVu = fontFamily ["DejaVu"] [sansSerif]
-- anonymousPro = fontFamily ["Anonymous Pro"] [monospace]
-- kreon = fontFamily ["Kreon"] [sansSerif]
sourceCodePro = fontFamily ["Source Code Pro"] [monospace]
montserrat = fontFamily ["Montserrat"] [sansSerif]
amaranth = fontFamily ["Amaranth"] [serif]
quando = fontFamily ["Quando"] [serif]

baseFont, monoSpace, navbarFont, headerFont :: Css
monoSpace = sourceCodePro
baseFont = montserrat
navbarFont = amaranth
headerFont = quando

-- actual css blocks
site :: Css
site = body ?
  do background  (rgb 20 20 20, url "../images/bkg.png")      -- black
     baseFont
     fontSize    (pt 10)
     sym margin  (pt 0)
     lineHeight  (pct 115)
     overflowY   scroll

header1 :: Css
header1 = h1 ?
  do headerFont
     -- fontWeight normal
     margin (em 0.2) 0 (em 0.2) 0

header2 :: Css
header2 = h2 ?
  do headerFont
     -- color "#666"
     -- fontSize (em 1.4)
     -- fontWeight bold
     fontVariant smallCaps
     margin (em 0.4) 0 (em 0.4) 0
     color "#8e5f1c"

divColumn :: Css
divColumn = body |> "div" ?
  do centered
     marginBottom (unit 5)

centered :: Css
centered =
  do box
     whenWide $
       do width       pageWidth
          "margin-left" -: "auto"
          "margin-right" -: "auto"
          -- marginLeft  auto
          -- marginRight auto
     whenNarrow $ width (pct 95)

contents :: Css
contents = ".content" ? do
     color lightgrey
     -- backgroundColor (setA 200 black)
     padding         u1 u1 u2 u1
     a ? do
       link & do
           color "#606060"
           textDecoration none
           fontWeight bold
           transitions [("color", sec 0.2, ease, sec 0.04)]
       visited & do
           color "#c0c0c0"
           textDecoration none
           fontWeight bold
       hover & do
           color "#fff"

menu :: Css
menu = nav ? do
    navFont
    navHR
    alignCenter
    marginTop   u1
    paddingLeft u1
    lineHeight  u2

-- http://matthewlein.com/ceaser/
navHR :: Css
navHR = ".navsep" ? do
    "border" -: "0"
    alignCenter
    width (pct 30)
    height (px 1)
    backgroundImage
        (linearGradient (straight sideLeft)
         [(rgba 50 50 50 40, pct 0),
          (rgba 250 250 250 90, pct 50),
          (rgba 50 50 50 40, pct 100)])

navFont :: Css
navFont =
  do navbarFont
     color lightgrey
     fontSize      (em 1.8)
     lineHeight    (em 1.5)
     -- fontVariant smallCaps
     textTransform lowercase
     a ? do
       link & do
           color "#606060"
           alignCenter
           textDecoration none
           fontWeight bold
           marginRight (px 10)
           transitions [("color", sec 0.2, ease, sec 0.04)]
       hover & color "#4ccccf" -- "#44e3e0"

articleBlock :: Css
articleBlock = article ? do
    -- sym margin 0
    -- margin 0 (em 4) 0 (em 6)
    Main.meta
    "margin-left" -: "auto"
    "margin-right" -: "auto"
    -- marginLeft auto
    -- marginRight auto
    marginTop (px 5)
    marginBottom (px 5)
    width (pct 100)
    overflow hidden
    Clay.div ? do
      transitions [("color", sec 0.8, ease, sec 0.5)]
      target & color "#8293ad"

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     floatRight
     marginRight (unit (2))
     Clay.span ?
       do display block
          color "#947662"

articlePubs :: Css
articlePubs = article # ".pubs" ? width (pct 85)

sectionBlock :: Css
sectionBlock = section ? do
    Main.meta
    float floatLeft
    width (pct 74)

asideBlock :: Css
asideBlock = aside ? do
    float floatRight
    width (pct 25)

pBlock :: Css
pBlock = p ? do
    textAlign justify
    "text-justify" -: "inter-word"

footerBlock :: Css
footerBlock = footer ? do
    float floatLeft
    width (pct 100)
    color "#909090"
    "margin" -: "30px auto auto auto"
    -- margin (px 30) auto auto auto
    fontSize (em 1)
    alignCenter

contactTable :: Css
contactTable = table # ".contact" ? do
    borderSpacing (px 0)
    sym padding (px 0)
    fontSize (em 1)

preBlock :: Css
preBlock = pre ? do
    monoSpace
    sym margin (px 0)
    fontSize (em 0.75)
    -- transform (scale 0.95 0.95)
    -- display inlineBlock
    box
    sym padding (em 0.5)
    sym borderRadius (px 5)
    -- lineHeight (pct 100)
    "white-space" -: "-moz-pre-wrap"
    "white-space" -: "-pre-wrap"
    "white-space" -: "-o-pre-wrap"
    "white-space" -: "pre-wrap"
    "word-wrap"   -: "break-word"

myCode :: Css
myCode = ".code" ? do
    sym margin (em 1)
    -- whenWide $ do
    --   marginLeft (em 18)
    --   marginRight (em 17)
    -- whenNarrow $ do
    --   marginLeft (em 1)
    --   marginRight (em 1)

iBlock :: Css
iBlock = i # ".venue" ? do
    borderSpacing (px 0)
    sym padding (px 0)
    color "#9aa6ab"

bBlock :: Css
bBlock = b # ".title" ? do
    fontSize (em 1.2)
    lineHeight (pct 130)

imgBlock :: Css
imgBlock = img ? do
    maxWidth (pct 90)
    "height" -: "auto"
    "width" -: "auto"
    -- whenNarrow $ do
    --   display none

imgDisp :: Css
imgDisp = img # ".displayed" ? do
    display block
    "margin-left" -: "auto"
    "margin-right" -: "auto"
    -- marginLeft auto
    -- marginRight auto
    sym borderRadius (px 4)
    boxShadow (px 0) (px 0) (px 12) (rgba 30 30 30 190)

imgAside :: Css
imgAside = aside |> img ? do
    sym borderRadius (px 5)

-- http://webdesignerwall.com/tutorials/css-elastic-videos
videoContainer :: Css
videoContainer = ".videoContainer" ? do
    position relative
    -- paddingBottom (pct 56.25) -- 16:9
    "padding-bottom" -: "56.25%"
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
     articleBlock
     articlePubs
     sectionBlock
     asideBlock
     pBlock
     footerBlock
     menu
     contactTable
     preBlock
     iBlock
     bBlock
     imgBlock
     imgDisp
     imgAside
     videoContainer
     myCode

-- extra css for codeblocks
-- https://github.com/Anomareh/pygments-styles-dump
