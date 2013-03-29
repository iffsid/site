{-# LANGUAGE OverloadedStrings #-}
import Clay hiding (menu, contents)
import Data.Monoid
import Data.Text (Text)
import Prelude hiding (all)
import qualified Clay.Media as Media

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
pageWidth = unit 40

whenNarrow :: Css -> Css
whenNarrow = query Media.all [Media.maxWidth pageWidth]

whenWide :: Css -> Css
whenWide = query Media.all [Media.minWidth pageWidth]

box :: Css
box = boxSizing borderBox

borderSpacing :: Text -> Css
borderSpacing space = "border-spacing" -: space

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)

-- http://www.bestwebfonts.com/
dejaVu = fontFamily ["DejaVu"] [sansSerif]
anonymousPro = fontFamily ["Anonymous Pro"] [monospace]
kreon = fontFamily ["Kreon"] [sansSerif]
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
       do baseFont
          fontSize (pt 10)
          sym margin  0
          background  black
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
       do width       (pct 100)

contents :: Css
contents = ".content" ? do
     color lightgrey
     backgroundColor (setA 200 black)
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
menu = nav ?
  do navFont
     navHR
     alignCenter
     marginTop   u1
     paddingLeft u1
     lineHeight  u2
     a ? marginRight (px 10)

animate :: Css
animate =
  transitions
    [ ("background-color" , sec 0.5, ease, sec 0)
    , ("color"            , sec 0.2, ease, sec 0)
    ]

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
     a ?
       do color          grey
          textDecoration none
          alignCenter
          -- borderBottom solid (px 3) "#666"
          animate
          hover &
            do color      white
               background black

articleBlock :: Css
articleBlock = article ? do
    -- sym margin 0
    -- margin 0 (em 4) 0 (em 6)
    marginLeft auto
    marginRight auto
    marginTop (px 5)
    marginBottom (px 5)
    width (pct 100)
    overflow hidden


meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     sideRight
     -- marginTop (unit (-1))
     Clay.span ?
       do display block
          color "#947662"

sectionBlock :: Css
sectionBlock = section ? do
    Main.meta
    float sideLeft
    width (pct 74)

asideBlock :: Css
asideBlock = aside ? do
    float sideRight
    width (pct 25)

footerBlock :: Css
footerBlock = footer ? do
    float sideLeft
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
    fontSize (px 8)
    transform (scale 0.95 0.95)
    -- display inlineBlock
    lineHeight (pct 100)
    "white-space" -: "-moz-pre-wrap"
    "white-space" -: "-pre-wrap"
    "white-space" -: "-o-pre-wrap"
    "white-space" -: "pre-wrap"
    "word-wrap"   -: "break-word"

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
    maxWidth (pct 100)
    height auto
    "width" -: "auto\9"

imgAside :: Css
imgAside = aside |> img ? do
    borderRadius (px 5)

-- main - required for hakyll
main :: IO ()
main = putCss $
  do site
     column
     contents
     header1
     header2
     articleBlock
     sectionBlock
     asideBlock
     footerBlock
     menu
     contactTable
     preBlock
     iBlock
     bBlock
     imgBlock
     imgAside
