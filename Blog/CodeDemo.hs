module Blog.CodeDemo
( codeDemoCollapsableJS
, codeDemoJS
) where

import PageTypes
import PageStructure

codeDemoCollapsableJS code =
  tag "div" [("class", "expandable_code")] $ do
    tag "a" [("href", "javascript:void(0)")] $ text "Show/Hide code"
    tag "pre" [("class", "hidden")] $ do
      tag "code" [("class", "language-javascript")] $ text code

codeDemoJS code =
  tag "div" [] $ do
    tag "pre" [] $ do
      tag "code" [("class", "language-javascript")] $ text code
