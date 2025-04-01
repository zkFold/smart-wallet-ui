import Options.Applicative
import ZkFold.Cardano.SmartWallet.Server.Options

main :: IO ()
main = runCommand =<< execParser opts
 where
  opts =
    info
      (parseCommand <**> helper)
      ( fullDesc
          <> progDesc "zkFold smart wallet helpful operations"
          <> header "zkFold smart wallet"
      )