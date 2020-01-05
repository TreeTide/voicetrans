module Sphinx where

import Data.Text (Text)
import Foreign (alloca, peek)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)

-- TODO move utils
peekIntegral = fmap fromIntegral . peek

#include <pocketsphinx/pocketsphinx.h>

{#pointer *arg_t as AcceptedArgs newtype#}

{#fun unsafe ps_args as defaultAcceptedArgs
  { } -> `AcceptedArgs'#}

{#pointer *cmd_ln_t as Config newtype#}

{#fun unsafe variadic cmd_ln_init
  [ const char*, const char*
  , const char*, const char*
  , const char*, const char*
  , const char*
  ]
  as initConfig3
  { `Config'
  , `AcceptedArgs'
  , `Bool'
  , `String', `String'
  , `String', `String'
  , `String', `String'
  , withNull- `String'
  } -> `Config'#}

{#fun unsafe cmd_ln_free_r as freeConfig
  { `Config' } -> `()'#}

withNull :: (CString -> IO a) -> IO a
withNull f = f nullPtr

{#pointer *ps_decoder_t as Decoder newtype#}
{#pointer *int16 as Samples newtype#}

{#fun unsafe ps_init as initDecoder
  { `Config' } -> `Decoder'#}

{#fun unsafe ps_free as freeDecoder
  { `Decoder' } -> `Int'#}

{#fun unsafe ps_start_utt as startUtt
  { `Decoder' } -> `Int'#}

-- TODO bytestring marshaller?
{#fun unsafe ps_process_raw as processRaw
  { `Decoder'
  , `Samples'
     -- ^ Buffer holding 16-bit samples.
  , `Int'
     -- ^ The sample count to process.
  , `Bool'
    -- ^ Perform full recognition (True) or only feature detection (False).
  , `Bool'
    -- ^ Do the samples constitute a full utterance?
  } -> `Int'
    -- ^ Number of samples searched, or <0 for error.
#}

{#fun unsafe ps_end_utt as endUtt
  { `Decoder' } -> `Int'#}

{#fun unsafe ps_get_hyp as getHyp
  {         `Decoder'
  , alloca- `Int'     peekIntegral*
  } -> `String'#}
