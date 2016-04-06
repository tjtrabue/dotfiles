Dojo Modules
============

Adds commands to insert full Dojo module and class names, for example, `com.company.spam.Eggs`. Dojo modules are discovered by parsing JavaScript files for `dojo.require` calls.

Setup
-----

This plugin must be configured with a list of directories to scan for JavaScript files. The JavaScript files in the current project are _not_ scanned automatically.

Either edit the included `DojoModules.sublime-settings` file or (preferred) copy that file to your Sublime Text `Packages/User` directory. Set the add `search_paths` value to an array of paths to scan for `.js` files. Windows paths must escape the `\` character. For example:

	"search_paths": [
		"C:\\dojo\source",
		"C:\\workspace\\my_dojo_widgets"
	]

Usage
-----

You can insert the found module names as just he module name, or wrapped in a `dojo.require`. Both commands show a quick panel of all discovered module names to pick from.


	Insert `dojo.require`			Ctrl+Shift+O
	Insert only module name			Ctrl+Shift+I

Compatibility
-------------

Works with Sublime Text 3. May work with Sublime Text 2 still, but is not be actively used or tested there anymore.

Performance
-----------

This package opens and reads all the JavaScript files that it finds in its search paths. This used to cause Sublime Text to pause for several seconds while starting up when there were a large number of files to read. Files are now scanned in background threads to avoid any startup delays, but that means all modules are not necessarily immediately available after startup. Once files have been scanned, a "done" message is shown in the Sublime Text status bar.