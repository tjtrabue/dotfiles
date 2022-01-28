-- See `:help vim.lsp.start_client` for an overview of the supported `config` options.

local fs = require("tjdot.fs")
local jdtls = require("jdtls")
local jdtls_setup = require("jdtls.setup")

local jdtls_install_root = os.getenv("HOME") .. "/applications/jdtls"
local jdtls_plugins_dir = jdtls_install_root .. "/plugins"
local config_dir_name

-- Where JDTLS will store per-project data.
local jdtls_workspace_root = os.getenv("HOME") .. "/.jdtls/workspace"
-- If you started neovim within `~/dev/xy/project-1` this would resolve to `project-1`
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")

-- Figure out JDTLS config dir based on OS.
if vim.fn.has("mac") or vim.fn.has("macunix") then
  config_dir_name = "config_mac"
elseif vim.fn.has("win64") or vim.fn.has("win32") then
  config_dir_name = "config_win"
else
  config_dir_name = "config_linux"
end

-- The JDTLS executable jar file.
local launcher_jar =
  fs.os_cmd_to_string(
  "find " .. jdtls_plugins_dir .. " -maxdepth 1 -mindepth 1 -type f -iname '*org.eclipse.equinox.launcher_*.jar'"
)

-- May not be necessary
-- Create the workspace root dir
-- os.execute("mkdir -p " .. jdtls_workspace_root)

local config = {
  -- The command that starts the language server
  -- See: https://github.com/eclipse/eclipse.jdt.ls#running-from-the-command-line
  cmd = {
    "java",
    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xms1g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens",
    "java.base/java.util=ALL-UNNAMED",
    "--add-opens",
    "java.base/java.lang=ALL-UNNAMED",
    "-jar",
    launcher_jar,
    "-configuration",
    jdtls_install_root .. "/" .. config_dir_name,
    "-data",
    jdtls_workspace_root .. "/" .. project_name
  },
  -- 💀
  -- This is the default if not provided, you can remove it. Or adjust as needed.
  -- One dedicated LSP server & client will be started per unique root_dir
  root_dir = jdtls_setup.find_root({".git", "mvnw", "gradlew"}),
  -- Here you can configure eclipse.jdt.ls specific settings
  -- See https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
  -- for a list of options
  settings = {
    java = {}
  },
  -- Language server `initializationOptions`
  -- You need to extend the `bundles` with paths to jar files
  -- if you want to use additional eclipse.jdt.ls plugins.
  --
  -- See https://github.com/mfussenegger/nvim-jdtls#java-debug-installation
  --
  -- If you don't plan on using the debugger or other eclipse.jdt.ls plugins you can remove this
  init_options = {
    bundles = {}
  }
}

-- This starts a new client & server,
-- or attaches to an existing client & server depending on the `root_dir`.
jdtls.start_or_attach(config)
