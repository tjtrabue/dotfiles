import sublime, sublime_plugin, sys, json, inspect, os

# http://sublimetexttips.com/execute-a-command-every-time-sublime-launches/
def plugin_loaded():
	# settings = sublime.load_settings("AutoSpell.sublime-settings")
	# settings.add_on_change('default_replacements', build)	
	# settings.add_on_change('default_triggers', build)	
	# settings.add_on_change('custom_replacements', build)	
	# settings.add_on_change('custom_triggers', build)	
	build()


# def plugin_unloaded():
	# settings = sublime.load_settings("AutoSpell.sublime-settings")
	# settings.clear_on_change('default_replacements')
	# settings.clear_on_change('default_triggers')
	# settings.clear_on_change('custom_replacements')
	# settings.clear_on_change('custom_triggers')

def build():	
	sys.stdout.write('Building AutoSpell Index: Started\n')

	settings = sublime.load_settings("AutoSpell.sublime-settings")

	key_map = []

	default_replacements = settings.get('default_replacements', {})
	if(default_replacements == False):
		default_replacements = {}
	elif(type(default_replacements) is not dict):
		print('invalid default_replacements in settings')
		return

	default_triggers = settings.get('default_triggers', [])
	if(default_triggers == False):
		default_triggers = []
	elif(type(default_triggers) is not list):
		print('invalid default_triggers in settings')
		return

	custom_replacements = settings.get('custom_replacements', {})
	if(type(custom_replacements) is not dict):
		print('invalid custom_replacements in settings')
		return

	custom_triggers = settings.get('custom_triggers', [])
	if(type(custom_triggers) is not list):
		print('invalid custom_triggers in settings')
		return

	# Combine them
	default_replacements.update(custom_replacements)
	replacements = default_replacements
	triggers = list(set(default_triggers + custom_triggers))

	for mispelled in replacements:
		replacement = replacements[mispelled]

		for key in triggers:
			char = key
			if char == 'enter':
				char = '\n'

			# lowercase version
			entry = {'command': 'insert', 'args': {}}
			entry['args']['characters'] = replacement + char
			entry['keys'] = list(mispelled)
			entry['keys'].append(key)

			key_map.append(entry)

			# TODO make it DRY
			# capitalize first letter version
			entry = {'command': 'insert', 'args': {}}
			entry['args']['characters'] = (replacement + char).capitalize()
			entry['keys'] = list(mispelled.capitalize())
			entry['keys'].append(key)

			key_map.append(entry)


	if(not key_map):
		print('no key maps')
		return

	current_dir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))

	key_map_data = open('%s/Default.sublime-keymap' % current_dir, 'w')
	key_map_data.write(json.dumps(key_map))
	key_map_data.close()

	sys.stdout.write('Building AutoSpell Index: Finished\n')

class AutoSpell(sublime_plugin.ApplicationCommand):
	def run(self):
		print('run AutoSpell')
