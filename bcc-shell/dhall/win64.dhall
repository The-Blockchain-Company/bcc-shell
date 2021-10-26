\(cluster : ./types/cluster.type)      ->
   let installDir = "Klarity${cluster.ccfgInstallDirectorySuffix}"
in let dataDir = "\${APPDATA}\\${installDir}"
    --
    --
in
{ osName      = "win64"
, osConfigurationYaml  = "\${KLARITY_INSTALL_DIRECTORY}\\configuration.yaml"
, osInstallDirectory   = installDir
, osX509ToolPath       = "\${KLARITY_DIR}\\bcc-x509-certificates.exe"
, osNodeArgs           =
  { naKeyfile          = "Secrets-1.0\\secret.key"
  , naLogsPrefix       = "Logs"
  , naTopology         = "\${KLARITY_DIR}\\wallet-topology.yaml"
  , naUpdateLatestPath = "Installer.exe"
  , naWalletDBPath     = "Wallet-1.0"
  , naTlsPath          = "tls"
  }
, osPass      =
  { pStatePath           = dataDir
  , pWorkingDir          = dataDir
  , pNodePath            = "\${KLARITY_DIR}\\bcc-node.exe"
  , pNodeDbPath          = "DB-1.0"
  , pNodeLogConfig       = "\${KLARITY_INSTALL_DIRECTORY}\\log-config-prod.yaml"
  , pNodeLogPath         = [] : Optional Text
  , pWalletPath          = "\${KLARITY_DIR}\\Klarity.exe"
  , pWalletLogging       = True
  , pFrontendOnlyMode    = True

  , pUpdaterPath         = "Installer.exe"
  , pUpdaterArgs         = [] : List Text
  , pUpdateArchive       = [] : Optional Text
  , pUpdateWindowsRunner = ["Installer.bat"] : Optional Text

  , pLauncherLogsPrefix  = "Logs\\pub"
  }
}
