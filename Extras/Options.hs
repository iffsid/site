module Extras.Options (
    pandocHtml5Compiler
  , hakyllConf
) where

import qualified Data.Set    as S
import           Hakyll
import           Text.Pandoc
import qualified Text.Pandoc.Extensions as E

dontIgnoreHtaccess :: String -> Bool
dontIgnoreHtaccess ".htaccess" = False
dontIgnoreHtaccess path        = ignoreFile defaultConfiguration path

-- deployCommand = "rsync -ave ssh _site/ xinitrc@corvus.uberspace.de:html/"
hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { ignoreFile = dontIgnoreHtaccess
  , deployCommand  = "rsync -avzc _site/ oxrobots:~/WWW/" }

pandocHtml5Compiler :: Compiler (Item String)
pandocHtml5Compiler =
  pandocCompilerWith pandocReaderOptions pandocWriterOptions

pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathML -- MathJax ""
  , writerSectionDivs = False
  , writerReferenceLinks = True
  }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
  { readerExtensions = myPandocExtensions }

-- http://hackage.haskell.org/packages/archive/pandoc/1.11.1/doc/html/Text-Pandoc-Options.html
myPandocExtensions :: Extensions
myPandocExtensions = E.extensionsFromList
                     [ Ext_footnotes
                     , Ext_inline_notes
                     , Ext_pandoc_title_block
                     , Ext_table_captions
                     , Ext_implicit_figures
                     , Ext_simple_tables
                     , Ext_multiline_tables
                     , Ext_grid_tables
                     , Ext_pipe_tables
                     , Ext_citations
                     , Ext_raw_tex
                     , Ext_raw_html
                     , Ext_tex_math_dollars
                     , Ext_tex_math_single_backslash
                     , Ext_latex_macros
                     , Ext_fenced_code_blocks
                     , Ext_fenced_code_attributes
                     , Ext_backtick_code_blocks
                     , Ext_inline_code_attributes
                     , Ext_markdown_in_html_blocks
                     , Ext_escaped_line_breaks
                     , Ext_fancy_lists
                     , Ext_startnum
                     , Ext_definition_lists
                     , Ext_example_lists
                     , Ext_all_symbols_escapable
                     , Ext_intraword_underscores
                     , Ext_blank_before_blockquote
                     , Ext_blank_before_header
                     , Ext_strikeout
                     , Ext_superscript
                     , Ext_subscript
                     , Ext_auto_identifiers
                     , Ext_header_attributes
                     , Ext_implicit_header_references
                     , Ext_line_blocks
                     ]
