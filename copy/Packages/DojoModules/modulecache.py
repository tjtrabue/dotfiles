import re, os, itertools, logging, threading, functools, sublime

def log(message):
	print('Dojo Modules:', message)

__dojo_provide_re__ = re.compile("""
	dojo.provide\( \s* 
		(?:\'|")				# single or double open quote
		([-a-zA-Z0-9_\.\$]+)	# module name
		(?:\'|")				# close quote
	\s* \)""", re.VERBOSE)	

def open_utf8(filename):
	try:
		return open(filename, encoding="utf-8")
	except:
		return open(filename)

class ModuleCache (object):
	"""A cache of Dojo module names that are found by parsing JavaScript files."""

	def __init__(self):
		# TODO: Should acquire lock around this since it's inited from another thread now.
		self._paths_to_caches = dict()

	@property
	def modules_by_name(self):
		"""Iterator of 2-tuples of all cached names-to-modules (e.g, (Eggs: com.spam.Eggs))."""
		for caches in self._paths_to_caches.values():
			for item in caches.items():
				yield item

	@property
	def modules(self):
		"""Iterator for all cached module names (e.g, com.spam.Eggs)."""
		for caches in self._paths_to_caches.values():
			for short_name, module in caches.items():
				yield module

	def scan_file_for_requires(self, file_name):
		max_lines = 100
		found_any = []
		# Clear cache of this file each time we come here. There may be
		# removed "provide"s that we don't want anymore.
		self._paths_to_caches[file_name] = short_names_to_full_names = {}
		with open_utf8(file_name) as file:
			for i, line in enumerate(file):
				if i > max_lines: break
				for m in __dojo_provide_re__.finditer(line):
					fully_qualified_name = m.group(1)
					package, short_name = os.path.splitext(fully_qualified_name)
					# strip leading .
					short_name = short_name[1:]
					short_names_to_full_names[short_name] = fully_qualified_name
					found_any.append(fully_qualified_name)
					# Look a few more lines down for supporting classes before bailing.
					max_lines = i + 4
		return found_any

	def scan_path(self, path):
		"""Recursively scans a single path for JavaScript files and dojo.provide statements.

		This can be called many times. Each time adds to the cache.

		"""
		if not os.path.isdir(path):
			log('WARNING: Search path not found: ' + path)
			return

		visited_paths = []
		for dirpath, dirnames, filenames in os.walk(path):
			# print('visit', dirpath)
			visited_paths.append(dirpath)
			for name in filenames:
				if '.js' == os.path.splitext(name)[1]:
					self.scan_file_for_requires(os.path.join(dirpath, name))
		joined_paths = '", "'.join(visited_paths)
		log('For search path "%s", scanned these directories: "%s"' % (path, joined_paths))

	def scan_all_paths(self, search_paths):
		"""Scans all given paths for `dojo.provide` statments and caches them all.
		Runs in a background thread.

		Any existing cached modules are cleared first to ensure that modules
		in files that no longer exist won't still be returned.

		search_paths - List of directory paths to recursively search for .js files.

		"""
		self._paths_to_caches.clear()
		search_paths = set(search_paths)

		# Using a background thread means processing we can process these in
		# the background instead of slowing startup.
		# This may not be needed in ST 3 if the "init" is restructured.
		def run_scan_paths():
			sublime.status_message('Dojo Modules: Scanning all paths...')
			for path in search_paths:
				self.scan_path(path)
			log('Done. All paths were scanned.')
			sublime.status_message('Dojo Modules: Done scanning paths. Ready to go.')

		threading.Thread(target=run_scan_paths).start()
