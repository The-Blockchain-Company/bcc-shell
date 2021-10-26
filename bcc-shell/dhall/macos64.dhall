\(cluster : ./types/cluster.type)      ->
let dataDir = "\${HOME}/Library/Application Support/Klarity${cluster.ccfgInstallDirectorySuffix}"
    --
    --
in
{ osName      = "macos64"
, osConfigurationYaml  = "\${KLARITY_INSTALL_DIRECTORY}/configuration.yaml"
, osInstallDirectory   = "Klarity${cluster.ccfgInstallDirectorySuffix}"
, osX509ToolPath       = "\${KLARITY_INSTALL_DIRECTORY}/bcc-x509-certificates"
, osNodeArgs           =
  { naKeyfile          = "${dataDir}/Secrets-1.0/secret.key"
  , naLogsPrefix       = "${dataDir}/Logs"
  , naTopology         = "\${KLARITY_INSTALL_DIRECTORY}/wallet-topology.yaml"
  , naUpdateLatestPath = "${dataDir}/installer.pkg"
  , naWalletDBPath     = "${dataDir}/Wallet-1.0"
  , naTlsPath          = "${dataDir}/tls"
  }
, osPass      =
  { pStatePath           = dataDir
  , pWorkingDir          = dataDir
  , pNodePath            = "\${KLARITY_INSTALL_DIRECTORY}/bcc-node"
  , pNodeDbPath          = "${dataDir}/DB-1.0"
  , pNodeLogConfig       = "\${KLARITY_INSTALL_DIRECTORY}/log-config-prod.yaml"
  , pNodeLogPath         = [] : Optional Text

  , pWalletPath          = "\${KLARITY_INSTALL_DIRECTORY}/Frontend"
  , pWalletLogging       = True
  , pFrontendOnlyMode    = True

  , pUpdaterPath         = "/usr/bin/open"
  , pUpdaterArgs         = ["-FW"]
  , pUpdateArchive       = ["${dataDir}/installer.pkg"] : Optional Text
  , pUpdateWindowsRunner = [] : Optional Text

  , pLauncherLogsPrefix  = "${dataDir}/Logs/pub/"
  }
}
