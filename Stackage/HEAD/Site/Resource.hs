{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stackage.HEAD.Site.Resource
  ( -- * Resources
    Resource
  , bootstrap4Stylesheet
  , fontawesomeStylesheet
  , jqueryScript
  , popperScript
  , bootstrap4Script
  , clipboardJsScript
    -- * Rendering
  , stylesheetResource
  , scriptResource )
where

import Data.Text (Text)
import Lucid

-- | External resource such as stylesheet or JavaScript.

data Resource (k :: ResourceKind)= Resource
  { resourceHref :: Text
    -- ^ Location of the resource
  , resourceIntegrity :: Text
    -- ^ Hash of the resource
  }

-- | Resource kind.

data ResourceKind
  = Stylesheet
  | Script

bootstrap4Stylesheet :: Resource 'Stylesheet
bootstrap4Stylesheet = Resource
  { resourceHref = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"
  , resourceIntegrity = "sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4"
  }

fontawesomeStylesheet :: Resource 'Stylesheet
fontawesomeStylesheet = Resource
  { resourceHref = "https://opensource.keycdn.com/fontawesome/4.7.0/font-awesome.min.css"
  , resourceIntegrity = "sha384-dNpIIXE8U05kAbPhy3G1cz+yZmTzA6CY8Vg/u2L9xRnHjJiAK76m2BIEaSEV+/aU"
  }

jqueryScript :: Resource 'Script
jqueryScript = Resource
  { resourceHref = "https://code.jquery.com/jquery-3.3.1.slim.min.js"
  , resourceIntegrity = "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
  }

popperScript :: Resource 'Script
popperScript = Resource
  { resourceHref = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js"
  , resourceIntegrity = "sha384-cs/chFZiN24E4KMATLdqdvsezGxaGsi4hLGOzlXwp5UZB1LY//20VyM2taTB4QvJ"
  }

bootstrap4Script :: Resource 'Script
bootstrap4Script = Resource
  { resourceHref = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js"
  , resourceIntegrity = "sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm"
  }

clipboardJsScript :: Resource 'Script
clipboardJsScript = Resource
  { resourceHref = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.js"
  , resourceIntegrity = "sha256-zpT7ps/VwyyMR5LVQdRBLaQjo1R2G9HvAJBgK1uV0dM="
  }

stylesheetResource :: Monad m => Resource 'Stylesheet -> HtmlT m ()
stylesheetResource Resource {..} = link_
  [ rel_ "stylesheet"
  , type_ "text/css"
  , href_ resourceHref
  , integrity_ resourceIntegrity
  , crossorigin_ "anonymous"
  ]

scriptResource :: Monad m => Resource 'Script -> HtmlT m ()
scriptResource Resource {..} = script_
  [ src_ resourceHref
  , integrity_ resourceIntegrity
  , crossorigin_ "anonymous"
  ] ("" :: Html ())
