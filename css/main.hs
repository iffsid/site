{-# LANGUAGE OverloadedStrings #-}
import           Clay        hiding (contents, menu)
import qualified Clay.Media  as Media
import           Data.Monoid
import           Data.Text   (Text)
import           Prelude     hiding (all)
import           Clay.Stylesheet
-- helpers
nil :: Size Abs
nil = px 0

u1, u2, u3, u4 :: Size Abs
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

unit, half :: Integer -> Size Abs
unit = px . (* 24)
half = px . (* 12)

pageWidth :: Size Abs
pageWidth = unit 43

whenNarrow :: Css -> Css
whenNarrow = query (MediaType "all") [Media.maxWidth pageWidth]

whenWide :: Css -> Css
whenWide = query (MediaType "all") [Media.minWidth pageWidth]

box :: Css
box = boxSizing borderBox

borderSpacing :: Text -> Css
borderSpacing space = "border-spacing" -: space

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)

-- http://www.bestwebfonts.com/
dejaVu = fontFamily ["DejaVu"] [sansSerif]
anonymousPro = fontFamily ["Anonymous Pro"] [monospace]
-- kreon = fontFamily ["Kreon"] [sansSerif]
montserrat = fontFamily ["Montserrat"] [sansSerif]
amaranth = fontFamily ["Amaranth"] [serif]

baseFont, monoSpace, navbarFont :: Css
monoSpace = anonymousPro
baseFont = montserrat
navbarFont = amaranth

-- actual css blocks
site :: Css
site =
  do body ?
       do background black
          baseFont
          fontSize (pt 10)
          sym margin  0
          lineHeight  (pct 115)
          overflowY   scroll

header1 :: Css
header1 =
    do h1 ?
          do fontWeight normal
             margin (em 0.2) 0 0 0

header2 :: Css
header2 =
    do h2 ?
          do color "#666"
             fontSize (em 1.4)
             fontWeight bold
             fontVariant smallCaps
             margin (em 0.4) 0 (em 0.2) 0
             color "#8e5f1c"

column :: Css
column = body |> "div" ?
  do centered
     marginBottom (unit 5)

centered :: Css
centered =
  do box
     whenWide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     whenNarrow $
       do width       (pct 95)

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
navHR = ".navsep" ? do
    "border" -: "0"
    alignCenter
    width (pct 30)
    height (px 1)
    backgroundImage
        (linearGradient (straight sideLeft)
         [((rgba 50 50 50 40), (pct 0)),
          ((rgba 250 250 250 90), (pct 50)),
          ((rgba 50 50 50 40), (pct 100)) ])

navFont :: Css
navFont =
  do navbarFont
     fontSize      (em 1.8)
     lineHeight    (em 1.5)
     -- fontVariant smallCaps
     textTransform lowercase
     a ? do
       -- color          grey
       textDecoration none
       alignCenter
       -- borderBottom solid (px 3) "#666"
       marginRight (px 10)
       transitions [("color", sec 0.2, ease, sec 0)] --("background-color", sec 0.5, ease, sec 0)
       hover & do
           color      white

articleBlock :: Css
articleBlock = article ? do
    -- sym margin 0
    -- margin 0 (em 4) 0 (em 6)
    Main.meta
    marginLeft auto
    marginRight auto
    marginTop (px 5)
    marginBottom (px 5)
    width (pct 100)
    overflow hidden
    Clay.div ? do
      transitions [("color", sec 0.8, ease, sec 0.5)]
      target & do
          color "#8293ad"

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     floatRight
     marginRight (unit (2))
     Clay.span ?
       do display block
          color "#947662"

articlePubs :: Css
articlePubs = article # ".pubs" ?
  do width (pct 85)

sectionBlock :: Css
sectionBlock = section ? do
    Main.meta
    float floatLeft
    width (pct 74)

asideBlock :: Css
asideBlock = aside ? do
    float floatRight
    width (pct 25)

footerBlock :: Css
footerBlock = footer ? do
    float floatLeft
    width (pct 100)
    color "#909090"
    margin (px 30) auto auto auto
    fontSize (em 1)
    alignCenter

contactTable :: Css
contactTable = table # ".contact" ? do
    borderSpacing "0px"
    sym padding 0
    fontSize (em 1)

preBlock :: Css
preBlock = pre ? do
    monoSpace
    sym margin 0
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
    borderSpacing "0px"
    sym padding 0
    color "#9aa6ab"

bBlock :: Css
bBlock = b # ".title" ? do
    fontSize (em 1.2)
    lineHeight (pct 130)

imgBlock :: Css
imgBlock = img ? do
    maxWidth (pct 90)
    height auto
    "width" -: "auto\9"
    -- whenNarrow $ do
    --   display none

imgDisp :: Css
imgDisp = img # ".displayed" ? do
    display block
    marginLeft auto
    marginRight auto
    sym borderRadius (px 5)
    boxShadow (px 0) (px 0) (px 30) (rgba 50 50 50 190)

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
        top 0
        left 0
        width (pct 100)
        height (pct 100)

-- absolute centering
-- http://codepen.io/shshaw/full/gEiDt

-- main - required for hakyll
main :: IO ()
main = putCss $
  do site
     column
     contents
     header1
     header2
     articleBlock
     articlePubs
     sectionBlock
     asideBlock
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
