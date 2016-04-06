import sublime_plugin, sublime
import os, os.path, re, pprint
try:
	import modulecache
except:
	import DojoModules.modulecache as modulecache

module_cache = modulecache.ModuleCache()
settings_file = 'DojoModules.sublime-settings'
settings = None

def load_dojo_module_cache():
	search_paths = settings.get('search_paths')
	if not search_paths:
		modulecache.log('WARNING: No search_paths have been set. See the README file for `Dojo Module Completions` for details.')
		return

	module_cache.scan_all_paths(search_paths)

def init_globals():
	global settings
	if not settings:
		settings = sublime.load_settings(settings_file)
		settings.add_on_change('search_paths', load_dojo_module_cache)
		load_dojo_module_cache()

def init_regexs():
	global prefixes_re
	global ignore_paths_re
	prefixes = settings.get("plugin_prefixes")
	prefixes_re = re.compile('|'.join(prefixes))
	ignore_paths = settings.get("ignore_paths")
	if not ignore_paths:
		ignore_paths_re = re.compile("a^")
	else:
		ignore_paths_re = re.compile('|'.join(ignore_paths) + '\\b')

def process_file_name(fileName):
	paths = fileName.split('\\')
	processed = "";
	for path in paths:
		print("Path: " + path)
		print("Processed: " + processed)
		if not processed:
			if prefixes_re.match(path):
				processed += path
		else:
			if ignore_paths_re.match(path):
				continue
			processed += "." + path
		print("End loop: " + processed)
	return processed

class DojoProvideCommand(sublime_plugin.TextCommand):

	def __init__(self, view):
		super(DojoProvideCommand, self).__init__(view)
		init_globals()
		init_regexs()

	def run(self, edit):
		fileName = self.view.file_name()
		jsIndex = fileName.find('.js');
		if (jsIndex != len(fileName)-3):
			return;
		else:
			fileName = fileName.rstrip('.js')
		fileName = process_file_name(fileName)
		self.view.insert(edit, 0, "dojo.provide(\"" + fileName + "\");" + settings.get("provide_comment"))		

class InsertDojoModuleCommand(sublime_plugin.TextCommand):
	"""Shows quick panel to insert a Dojo module name.

	These are the same modules as returned by the suggested completions.
	Provided as a way to bind a keyboard shortcut to this action instead of
	relying on completions.

	"""
	def __init__(self, view):
		super(InsertDojoModuleCommand, self).__init__(view)
		init_globals()

	def is_enabled(self):
		for region in self.view.sel():
			if self.view.score_selector(region.a, 'source.js | text.html.basic'):
				return True
		return False

	def run(self, edit):
		modules = sorted(set(module_cache.modules))
		def on_done(i):
			if i == -1: return
			for region in self.view.sel():
				self.view.run_command('insert_text', {
						'point': region.b,
						'text': modules[i]
					})
		self.view.window().show_quick_panel(modules, on_done)


class InsertTextCommand(sublime_plugin.TextCommand):
	"""Command just to insert text into a view.

	Work around for SublimeText 3 changes the invalidate the Edit object after
	`run` which means it can't easily be used in callbacks.
	"""
	
	def __init__(self, view):
		super(InsertTextCommand, self).__init__(view)
		init_globals()

	def run(self, edit, point, text):
		self.view.insert(edit, point, text)


class RequireDojoModuleCommand(sublime_plugin.TextCommand):
	"""Insert a Dojo module as part of a `dojo.require` statement."""

	def is_enabled(self):
		for region in self.view.sel():
			if self.view.score_selector(region.a, 'source.js'):
				return True
		return False

	def run(self, edit):
		modules = sorted(set(module_cache.modules))
		def on_done(i):
			if i == -1: return
			comment = settings.get('require_comment') or ""
			message = ("dojo.require('%s');" + comment) % (modules[i])
			for region in self.view.sel():
				self.view.run_command('insert_text', {
						'point': region.b,
						'text': message
					})
		self.view.window().show_quick_panel(modules, on_done)


class DojoModuleCompletions(sublime_plugin.EventListener):
	"""Provides completions for all the scanned Dojo modules."""

	def on_post_save(self, view):
		saved_file = view.file_name()	
		if any(saved_file.startswith(path) for path in settings.get('search_paths')):
			module_cache.scan_file_for_requires(saved_file)

	# def on_query_completions(self, view, prefix, locations):
	# 	completions = []
	
	# 	if not all(view.score_selector(loc, 'source.js | text.html.basic') for loc in locations):
	# 		return None

	# 	for class_name, module_name in dict(module_cache.modules_by_name).items():
	# 		if class_name.lower().startswith(prefix.lower()):
	# 			completions.append(('%s - %s' % (class_name, module_name[:-len(class_name) - 1]), module_name))
	# 			# completions.append((module_name))

	# 	print 'here', prefix
	# 	print sublime.INHIBIT_WORD_COMPLETIONS
	# 	return (completions, 0)