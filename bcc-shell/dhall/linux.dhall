\(cluster : ./types/cluster.type)      ->
let dataDir = "\${XDG_DATA_HOME}/Klarity/${cluster.ccfgName}"
in
{ osName      = "linux64"
, osConfigurationYaml  = "\${KLARITY_CONFIG}/configuration.yaml"
, osInstallDirectory   = ""
, osX509ToolPath       = "bcc-x509-certificates"
, osNodeArgs           =
  { naKeyfile          = "${dataDir}/Secrets/secret.key"
  , naLogsPrefix       = "${dataDir}/Logs"
  , naTopology         = "\${KLARITY_CONFIG}/wallet-topology.yaml"
  , naUpdateLatestPath = "${dataDir}/installer.sh"
  , naWalletDBPath     = "${dataDir}/Wallet"
  , naTlsPath          = "${dataDir}/tls"
  }
, osPass      =
  { pStatePath           = dataDir
  , pWorkingDir          = dataDir
  , pNodePath            = "bcc-node"
  , pNodeDbPath          = "${dataDir}/DB/"
  , pNodeLogConfig       = "\${KLARITY_CONFIG}/log-config-prod.yaml"
  , pNodeLogPath         = [] : Optional Text
  , pWalletPath          = "klarity-frontend"
  , pWalletLogging       = False
  , pFrontendOnlyMode    = True

  -- todo, find some way to disable updates when unsandboxed?
  , pUpdaterPath         = "/bin/update-runner"
  , pUpdaterArgs         = [] : List Text
  , pUpdateArchive       = [ "${dataDir}/installer.sh" ] : Optional Text
  , pUpdateWindowsRunner = [] : Optional Text

  , pLauncherLogsPrefix  = "${dataDir}/Logs/"
  }
}
