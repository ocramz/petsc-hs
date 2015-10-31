import Distribution.Simple

import Control.Monad (when)
import System.Cmd
import System.Exit ( ExitCode(..) )

testing _ _ _ _ = do
     err <- system "make"
     -- system "make -s -C tests clean"
     when (err /= ExitSuccess) $ ioError $ userError "failed"


main = defaultMainWithHooks simpleUserHooks
        {runTests = testing}
