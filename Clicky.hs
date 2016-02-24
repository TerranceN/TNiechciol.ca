module Clicky
( clickyTrackingCode
) where

-- This is in a separate module because the weird quoting messes up my code highlighting

clickyTrackingCode = " \n\
  \<script type=\"text/javascript\"> \n\
  \var clicky_site_ids = clicky_site_ids || []; \n\
  \clicky_site_ids.push(100919464); \n\
  \(function() { \n\
  \  var s = document.createElement('script'); \n\
  \  s.type = 'text/javascript'; \n\
  \  s.async = true; \n\
  \  s.src = '//static.getclicky.com/js'; \n\
  \  ( document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0] ).appendChild( s ); \n\
  \})(); \n\
  \</script> \n\
  \<noscript><p><img alt=\"Clicky\" width=\"1\" height=\"1\" src=\"//in.getclicky.com/100919464ns.gif\" /></p></noscript> \n\
  \"
