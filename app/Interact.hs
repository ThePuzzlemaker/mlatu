module Interact
  ( run,
  )
where

import Mlatu (Prelude (..), compilePrelude)
import Mlatu qualified
import Mlatu.Codegen qualified as Codegen
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Infer (typeFromSignature, typecheck)
import Mlatu.Informer (errorCheckpoint, warnCheckpoint)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute),
    Unqualified (Unqualified),
  )
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printQualified, printType)
import Mlatu.Signature qualified as Signature
import Mlatu.Term qualified as Term
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Vocabulary
import Optics
import Prettyprinter (vcat)
import Relude
import Report (reportAll)
import System.Console.Repline
import System.Directory (removeFile)
import System.IO (hPrint)
import System.Process.Typed (proc, runProcess_)

type MRepl = HaskelineT (ReaderT Dictionary (StateT (Text, Int) IO))

cmd :: String -> MRepl ()
cmd input = do
  (text, lineNumber) <- get
  commonDictionary <- ask
  let entryNameUnqualified = toText $ "entry" ++ show lineNumber
      entryName =
        Qualified
          (Qualifier Absolute ["interactive"])
          $ Unqualified entryNameUnqualified
  mResults <- liftIO $
    runMlatuExceptT $ do
      fragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Global "io"]
          (Just entryName)
          lineNumber
          "<interactive>"
          (text <> " " <> toText input)
      errorCheckpoint
      dictionary <- Enter.fragment fragment commonDictionary
      errorCheckpoint
      pure dictionary
  case mResults of
    Left reports -> do
      liftIO $ reportAll reports
    Right dictionary -> do
      put (text <> " " <> toText input, lineNumber + 1)
      liftIO $ do
        bs <- Codegen.generate dictionary (Just entryName)
        writeFileBS "repl.rs" bs
        runProcess_ (proc "rustfmt" ["repl.rs"])
        runProcess_
          ( proc
              "rustc"
              [ "--emit=link",
                "--crate-type=bin",
                "--edition=2018",
                "-C",
                "opt-level=3",
                "-C",
                "lto=y",
                "-C",
                "panic=abort",
                "-C",
                "codegen-units=1",
                "repl.rs"
              ]
          )
        runProcess_ (proc "./repl" [])

-- TODO
completer :: String -> ReaderT Dictionary (StateT (Text, Int) IO) [String]
completer n = pure []

helpCmd :: String -> MRepl ()
helpCmd s = liftIO $ case words (toText s) of
  ["help"] -> putStrLn helpHelp
  _ -> traverse_ putStrLn [helpHelp]
  where
    helpHelp = ":help - Show this help."

opts :: [(String, String -> MRepl ())]
opts = [("help", helpCmd)]

ini :: MRepl ()
ini = liftIO $ putStrLn "Welcome!"

final :: MRepl ExitDecision
final = do
  liftIO $ putStrLn "Bye!"
  liftIO $ removeFile "repl.rs"
  liftIO $ removeFile "./repl"
  pure Exit

run :: Prelude -> IO Int
run prelude = do
  result <- runMlatuExceptT $ Mlatu.compilePrelude prelude [QualifiedName $ Global "io"] Nothing
  case result of
    Left reports -> do
      reportAll reports
      pure 1
    Right commonDictionary -> do
      execStateT (runReaderT (evalReplOpts replOpts) commonDictionary) ("", 1)
      pure 0
  where
    replOpts =
      ReplOpts
        { banner = \case
            SingleLine -> pure "> "
            MultiLine -> pure "| ",
          command = cmd,
          options = opts,
          prefix = Just ':',
          multilineCommand = Just "paste",
          tabComplete = Word completer,
          initialiser = ini,
          finaliser = final
        }
