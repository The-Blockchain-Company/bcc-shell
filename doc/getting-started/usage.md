
Example of usage of 'bcc-shell' in 'bcc-ledger'
=======================================================


The 'validate-mainnet' application in 'bcc-ledger' runs a set of recorded epochs through validation of ledger rules. These epochs are conveniently stored in local files.
The app starts in [Main.hs](https://github.com/The-Blockchain-Company/bcc-ledger/blob/master/validate-mainnet/app/Main.hs) with initialisation of configuration and environment:

```
main :: IO ()
main = do
  let bccConfiguration = mainnetConfiguration
  bccEnvironment <- initializebccEnvironment

```

Then, creates the logging feature:
```
  (loggingLayer, loggingFeature) <- createLoggingFeature
    bccEnvironment
    bccConfiguration

```

And, creates its own blockchain validation feature:
```
  (blockchainLayer, blockchainFeature) <- createBlockchainFeature
    bccEnvironment
    bccConfiguration
    Production
    loggingLayer

```

The application will have access to these features, and the functions defined in 'loggingLayer' and 'blockchainLayer':
```
  runbccApplicationWithFeatures
      Production
      [blockchainFeature, loggingFeature]
    . bccApplication
    $ blockchainApp loggingLayer blockchainLayer

```

Imports and dependencies
------------------------

The main module only imports modules exported by 'bcc.Shell'. No need to directly import any module from the logging framework, for example.
The features defined in 'bcc.Shell' provide a structure, or layer, with the exported functions. 

The application's import of 'bcc.Shell' modules:

```
import bcc.Shell.Features.Logging
  ( LoggingLayer(..), Trace, createLoggingFeature)
import bcc.Shell.Lib (runbccApplicationWithFeatures)
import bcc.Shell.Presets (mainnetConfiguration)
import bcc.Shell.Types
  ( ApplicationEnvironment(..)
  , bccApplication(..)
  , initializebccEnvironment
  )

```


Logging support
---------------

The 'LoggingLayer' is a structure that provides most logging functions. And, the types used are re-exported by the 'LoggingFeature'.
A declaration `import bcc.Shell.Features.Logging` will add them to the local namespace.

The logging is setup and provides a basic trace to which observables can be traced, or from which more named traces can be derived.

The application enters a logging context named "validate-mainnet" by creating a subtrace:
```
blockchainApp :: LoggingLayer -> BlockchainLayer -> IO ()
blockchainApp ll bcl = do
  mainTrace <- llAppendName ll "validate-mainnet" (llBasicTrace ll)

  -- Bulk chain validation
  bulkChainValidation mainTrace bcl ll
```
All subsequent traced observables will be labelled with this name.

The app continues to validate epochs, one epoch file after the other, and outputs logging messages:

```
bulkChainValidation :: Trace IO Text -> BlockchainLayer -> LoggingLayer -> IO ()
bulkChainValidation logTrace bcl ll = do
  logNotice logTrace "Begin validating epoch files..."
  bulkChainValidationLoop logTrace
 where
  logNotice :: Trace IO Text -> Text -> IO ()
  logNotice = llLogNotice ll
```

The `llLogNotice` functions, available from the 'LoggingLayer', outputs a message with severity 'Notice' on the trace.


