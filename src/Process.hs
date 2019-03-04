module Process
    ( runProcess
    , runProcess'
    , runProcessNonStrict
    , runProcessWithDir
    )
where

import           System.IO
import           System.Exit
import           Config
import           Logger
import           System.Process                 ( readCreateProcessWithExitCode
                                                , shell
                                                )

runProcessWithDir dir shellCommand =
    runProcess ("cd " <> dir <> " && " <> shellCommand)

runProcess' shellCommand = do
    runProcess shellCommand
    return ()

runProcess shellCommand = do
    (_, output, _) <- exitIfFailed shellCommand =<< _runProcess shellCommand []
    return output


runProcessNonStrict :: String -> Program (ExitCode, String, String)
runProcessNonStrict shellCommand = _runProcess shellCommand []


_runProcess shellCommand std_in = do
    (exitCode, output, error) <- liftIO
        $ readCreateProcessWithExitCode (shell shellCommand) std_in
    logDebug $ shellCommand <> "\n" <> output
    return (exitCode, output, error)


exitIfFailed shellCommand commandResult = case commandResult of
    (ExitFailure _, _, _) -> exitWithError shellCommand commandResult
    _                     -> return commandResult


exitWithError shellCommand (exitCode, output, error) = do
    logError
        ("\n-----------------------------\n\
      \An error has occurred! Debug info is provided below\n\
      \COMMAND: \""
        <> shellCommand
        <> "\"\nSTD_OUT:\n"
        <> output
        <> "\nERROR:\n"
        <> error
        )
    liftIO exitFailure
