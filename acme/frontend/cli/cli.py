import acme.configuration as conf
import os
from acme.util import *
from colorama import Fore, Back, Style, init
import sys
import pycountry
import pytz
import readline
import tempfile
from subprocess import call

init(autoreset=True)

EDITOR = os.environ.get('EDITOR', 'emacs')

def query_yes_no(question, default="yes"):
    """Ask a yes/no question via raw_input() and return their answer.

    "question" is a string that is presented to the user.
    "default" is the presumed answer if the user just hits <Enter>.
        It must be "yes" (the default), "no" or None (meaning
        an answer is required of the user).

    The "answer" return value is True for "yes" or False for "no".
    """
    valid = {"yes": True, "y": True, "ye": True,
             "no": False, "n": False}
    if default is None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = raw_input().lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' "
                             "(or 'y' or 'n').\n")

def option_display_string(option, config=None, display_value=False, display_default=False):
    if display_value:
        assert(config is not None)
        option_value, option_origin = config.option_value(option)

        if option_value is not None:
            option_value_string = option.display_value(option_value)
        else:
            if option.default_value is not None:
                option_value_string = option.display_value(option.default_value)
                option_origin = 'Default'
            else:
                option_value_string = 'Not set'

        origin_string = ''
        if option_origin is not None:
            origin_string = ' (' + str(option_origin) + ')'
        
        if option.is_required:
            return option.name + ' ' + origin_string + '(*): ' + Style.NORMAL + str(option_value_string)
        else:
            return option.name + ' ' + origin_string + ': ' + str(option_value_string)
    else:
        if option.is_required:
            display = option.name + '(*)'
            if display_default and option.default_value is not None:
                display += '. Default: ' + str(option.default_value)
            return display
        else:
            display = option.name
            if display_default and option.default_value is not None:
                display += '. Default: ' + str(option.default_value)
            return display


def delete_configuration(config, **kwargs):
    confirmed = True
    if not kwargs.get('force'):
        confirmed = query_yes_no('Delete configuration ' + config.name + '?')
    if confirmed:
        conf.Configuration.unregister_config(config)
        filename = kwargs.get('filename') or os.getcwd() + '/acme.config'
        save_configs(filename)

def create_configuration(config, **kwargs):
    def print_help():
        print 'Create configuration'

    def edit_option(option):
        print option_display_string(option, display_default=True)
        print
        print '[s] Set'
        if not option.is_required:
            print '[ENTER] Skip'
        print '[l] Edit previous option'
        print '[h] Help | [q] Quit'
        print
        command = raw_input('Command: ')

        if command == 's':
            editor_class_name = 'Cli' + option.option_type.name + 'Editor'
            editor_class = globals()[editor_class_name]
            editor = editor_class(option, config)
            value = editor.edit()
            config.set_option_value(option, value)
        elif command == ''  and not option.is_required:
            pass
        elif command == 'l':
            print 'Not implemented'
        elif command == 'h' or command == 'help':
            print_help()
        elif command == 'q' or command == 'quit':
            if query_yes_no('Quit without saving?'):
                print 'Bye.'
                exit(0)
        else:
            print 'Invalid command'
            edit_option(option)
        print

    print 'Creating ' + config.name + ' configuration'
    print
    for section in config.schema.sections():
        print Fore.WHITE + Back.BLUE +  section.name + ':'
        for option in section.options():
            edit_option(option)
       
    # We are done. Save?
    print_config(config)
    if query_yes_no('Save the configuration?'):
        filename = kwargs.get('filename') or os.getcwd() + '/acme.config'
        confirmed_filename = raw_input('Save to file[' + filename + ']:') or filename
        save_configs(confirmed_filename)
        print Fore.GREEN + 'CONFIGURATIONS SAVED.'           

def print_section(config, section):
    print Fore.WHITE + Back.BLUE +  Style.BRIGHT + section.name + ':'
    for option in section.options():
        print option_display_string(option, config=config, display_value=True)

def print_config(config):
    print Style.BRIGHT + config.name + ' configuration'
    print
    if config.documentation is not None:
        print Fore.WHITE + Back.CYAN + config.documentation
        print
    for section in config.schema.sections():
        print_section(config, section)
        print

def edit_configurations(configs, **kwargs):
    print 'Select configuration:'
    
    index = 0
    for config in configs:
        print '[' + str(index) + '] ' + config.name
        index = index + 1
    print '[s] Save | [h] Help | [q] Quit'
    
    command = raw_input('Command: ')
    try:
        selected_config = int(command)
    except ValueError:
        selected_config = None

    if selected_config is not None:
        if selected_config >= len(configs):
            print 'Config ' + str(selected_config) + ' not found'
            print
            return edit_configurations(configs, **kwargs)
        else:
            config = configs[selected_config]
            edit_configuration(config, **kwargs)
            return edit_configurations(config, **kwargs)
    else:
        def print_help():
            print 'Select a configuration with a number'
            print
        if command == 's' or command == 'save':
            if query_yes_no('Save changes to configurations?'):
                filename = kwargs.get('filename') or os.getcwd() + '/acme.config'
                confirmed_filename = raw_input('Save to file[' + filename + ']:') or filename
                save_configs(confirmed_filename)
                print Fore.GREEN + 'CONFIGURATIONS SAVED.'
            else:
                return edit_configurations(configs, **kwargs)
        if command == 'h' or command == 'help':
            print_help()
            return edit_configurations(configs, **kwargs)
        if command == 'q' or command == 'quit':
            if query_yes_no('Quit without saving?'):
                print 'Bye.'
                exit(0)
            else:
                return edit_configurations(configs, **kwargs)
        else:
            print 'Invalid command: ' + command
            print
            return edit_configurations(configs, **kwargs)

def manage_configuration(config, **kwargs):
    print 'Manage ' + config.name + ' configuration'
    print '[s] Save'
    print '[d] Update description'
    print '[v] Validate'
    print '[p] Print'
    print '[u] Set parent'
    print '[h] Help'
    print '[l] Go back'
    command = raw_input('Command: ')

    def print_help():
        print 'Configuration management help'

    if command == 'p' or command == 'print':
        print_config(config)
        return manage_configuration(config, **kwargs)
    if command == 's' or command == 'save':
        print_config(config)
        if query_yes_no('Save configuration?'):
            filename = kwargs.get('filename') or os.getcwd() + '/acme.config'
            confirmed_filename = raw_input('Save to file[' + filename + ']:') or filename
            save_configs(confirmed_filename)
            print Fore.GREEN + 'CONFIGURATION SAVED.'
            return manage_configuration(config, **kwargs)
        else:
            return manage_configuration(config, **kwargs)
    if command == 'd':
        with tempfile.NamedTemporaryFile(suffix=".tmp") as tmpfile:
            tmpfile.write(config.documentation)
            tmpfile.flush()
            call([EDITOR, tmpfile.name])
            config.documentation = tmpfile.read()
        return manage_configuration(config, **kwargs)
    if command == 'v':
        print 'Not implemented'
        return manage_configuration(config, **kwargs)
    if command == 'u':
        print 'Not implemented'
        return manage_configuration(config, **kwargs)
    if command == 'h' or command == 'help':
        print_help()
        return manage_configuration(config, **kwargs)
    if command == 'l' or command == 'back':
        return
    else:
        print 'Invalid command: ' + command
        print
        return manage_configuration(config, **kwargs)

def edit_configuration(config, **kwargs):
    print 'Select section:'
    sections = config.schema.sections()
    index = 0
    for section in sections:
        print '[' + str(index) + '] ' + section.name
        index = index + 1
    print '[p] Print | [s] Save | [m] Manage | [h] Help | [q] Quit'
    print
    command = raw_input('Command: ')
    try:
        selected_section = int(command)
    except ValueError:
        selected_section = None

    if selected_section is not None:
        if selected_section >= len(sections):
            print 'Section ' + str(selected_section) + ' not found'
            print
            return edit_configuration(config, **kwargs)
        else:
            select_option(config, sections[selected_section])
            return edit_configuration(config, **kwargs)
    else:
        def print_help():
            print 'Select a section with a number'
            print
        if command == 'p' or command == 'print':
            print_config(config)
            return edit_configuration(config, **kwargs)
        if command == 's' or command == 'save':
            print_config(config)
            if query_yes_no('Save configuration?'):
                filename = kwargs.get('filename') or os.getcwd() + '/acme.config'
                confirmed_filename = raw_input('Save to file[' + filename + ']:') or filename
                save_configs(confirmed_filename)
                print Fore.GREEN + 'CONFIGURATION SAVED.'
            else:
                return edit_configuration(config, **kwargs)
        if command == 'm':
            manage_configuration(config, **kwargs)
            return edit_configuration(config, **kwargs)
        if command == 'h' or command == 'help':
            print_help()
            return edit_configuration(config, **kwargs)
        if command == 'q' or command == 'quit':
            if query_yes_no('Quit without saving?'):
                print 'Bye.'
                exit(0)
            else:
                return edit_configuration(config, **kwargs)
        else:
            print 'Invalid command: ' + command
            print
            return edit_configuration(config, **kwargs)

def select_option(config, section):
    print 'Select option:'
    options = section.options()
    index = 0
    for option in options:
        print '[' + str(index) + '] ' + option_display_string(option, config, display_value=True)
        index = index + 1
    print '[l] Go back | [h] Help | [q] Quit'
    print
    command = raw_input('Command: ')
    try:
        selected_option = int(command)
    except ValueError:
        selected_option = None

    if selected_option is not None:
        if selected_option >= len(options):
            print 'Option ' + str(selected_option) + ' not found'
            print
            return select_option(config, section)
        else:
            edit_option(config, options[selected_option])
            select_option(config, section)
    else:
        def print_help():
            print 'Select an option with a number'
            print
        if command == 'h' or command == 'help':
            print_help()
            return select_option(config, section)
        if command == 'q' or command == 'quit':
            if query_yes_no('Quit without saving?'):
                print 'Bye.'
                exit(0)
            else:
                return select_option(config, section)
        elif command == 'l':
            return
        else:
            print 'Invalid command: ' + command
            print
            return select_option(config, section)

def save_configs(filename):
    serializer = conf.ConfigurationsXMLSerializer()
    for config in conf.Configuration.configurations():
        serializer.serialize(config)
    serializer.write(filename)
        
def edit_option(config, option):
    print 'Edit option: ' + option_display_string(option)
    
    option_value, option_origin = config.option_value(option)

    if option_value is not None:
        option_value_string = option.display_value(option_value)
    else:
        if option.default_value is not None:
            option_value_string = option.display_value(option.default_value)
            option_origin = 'Default'
        else:
            option_value_string = 'Not set'
   
    origin_string = ''
    if option_origin is not None:
        origin_string = ' (' + str(option_origin) + ')'

    print 'Type: ' + str(option.option_type)
    print 'Value: ' + Style.BRIGHT + str(option_value_string) + origin_string
    if option.default_value is not None:
        print 'Default: ' + str(option.default_value)
    print
    print '[s] Set'
    print '[u] Unset'
    print '[l] Go back | [h] Help | [q] Quit'
    print

    def print_help():
        print 'Set/unset the option value'
    command = raw_input('Command: ')
    if command == 's':
        editor_class_name = 'Cli' + option.option_type.name + 'Editor'
        editor_class = globals()[editor_class_name]
        editor = editor_class(option, config)
        value = editor.edit()
        config.set_option_value(option, value)
        return edit_option(config, option)
    elif command == 'u':
        config.unset_option(option)
        return edit_option(config, option)
    elif command == 'l':
        return 'go_back'
    elif command == 'h':
        print_help()
    elif command == 'q':
        if query_yes_no('Quit without saving?'):
            exit(0)
        else:
            return edit_option(config, option)

class OptionTypeEditor(object):
    option = None
    config = None

    def __init__(self, option, config):
        self.option = option
        self.config = config

    
class CliStringEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('New value: ')
        return self.option.parse_value(value)

class CliBooleanEditor(OptionTypeEditor):
    def edit(self):
        return query_yes_no(self.option.name + '?')

class CliNumberEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('New value: ')
        return self.option.parse_value(value)

class CliFilenameEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('Enter a filepath: ')
        if not os.path.isfile(value):
            cont = query_yes_no(value + ' file does not exist. Continue?')
            if not cont:
                return self.edit()

        return self.option.parse_value(value)

class CliEmailEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('New value: ')
        return self.option.parse_value(value)

class CliURIEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('New value: ')
        return self.option.parse_value(value)

class CliDirectoryEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('Enter new directory: ')
        if not os.path.isdir(value):
            cont = query_yes_no(value + ' file does not exist. Continue?')
            if not cont:
                return self.edit()
        return self.option.parse_value(value)

class CliColorEditor(OptionTypeEditor):
    def edit(self):
        value = raw_input('New value: ')
        return self.option.parse_value(value)

class CliDateEditor(OptionTypeEditor):
    def edit(self):
        day = raw_input('Day: ')
        month = raw_input('Month: ')
        year = raw_input('Year: ')
        date = day + '/' + month + '/' + year
        return self.option.parse_value(date)

class CliTimeEditor(OptionTypeEditor):
    def edit(self):
        hour = raw_input('Hour: ')
        minutes = raw_input('Minute[00]:') or '00'
        seconds = raw_input('Seconds[00]') or '00'
        time = hour + ':' + minutes + ':' + seconds
        return self.option.parse_value(time)

class CliDatetimeEditor(OptionTypeEditor):
    def edit(self):
        day = raw_input('Day: ')
        month = raw_input('Month: ')
        year = raw_input('Year: ')
        date = day + '/' + month + '/' + year
        
        hour = raw_input('Hour: ')
        minutes = raw_input('Minute[00]:') or '00'
        seconds = raw_input('Seconds[00]') or '00'
        time = hour + ':' + minutes + ':' + seconds

        return self.option.parse_value("('" + date + "','" + time + "')")

class CliChoiceEditor(OptionTypeEditor):
    def edit(self):
        options = self.option.option_type.options()
        index = 0
        for option in options:
            print '[' + str(index) + '] ' + str(option)
            index = index + 1
        selection = raw_input('Select option: ')
        try:
            selected_option = int(selection)
        except ValueError:
            selected_option = None

        if selected_option is not None:
            if selected_option >= len(options):
                print 'Option ' + str(selected_option) + ' not found'
                print
                return self.edit()
            else:
                return self.option.parse_value(options[selected_option])
        else:
            print 'Enter an option number'
            print
            return self.edit()

class CliCountryEditor(OptionTypeEditor):
    options = [country.name for country in pycountry.countries]

    def edit(self):
        readline.set_completer(self.completer)
        readline.parse_and_bind('tab: complete')
        value = raw_input('Enter country (tab for completion): ')
        return self.option.parse_value(value)
        
    def completer(self, text, state):
        matches = [i for i in self.options if i.startswith(text)]
        if state < len(matches):
            return matches[state]
        else:
            return None

class CliTimezoneEditor(OptionTypeEditor):
    options = [str(tz) for tz in pytz.all_timezones]

    def edit(self):
        readline.set_completer(self.completer)
        readline.parse_and_bind('tab: complete')
        value = raw_input('Enter timezone (tab for completion): ')
        return self.option.parse_value(value)
        
    def completer(self, text, state):
        matches = [i for i in self.options if i.startswith(text)]
        if state < len(matches):
            return matches[state]
        else:
            return None

class CliLanguageEditor(OptionTypeEditor):
    options = [lang.name for lang in pycountry.languages]

    def edit(self):
        readline.set_completer(self.completer)
        readline.parse_and_bind('tab: complete')
        value = raw_input('Enter language (tab for completion): ')
        return self.option.parse_value(value)
        
    def completer(self, text, state):
        matches = [i for i in self.options if i.startswith(text)]
        if state < len(matches):
            return matches[state]
        else:
            return None

class CliCurrencyEditor(OptionTypeEditor):
    options = [currency.name for currency in pycountry.currencies]

    def edit(self):
        readline.set_completer(self.completer)
        readline.parse_and_bind('tab: complete')
        value = raw_input('Enter currency (tab for completion): ')
        return self.option.parse_value(value)
        
    def completer(self, text, state):
        matches = [i for i in self.options if i.startswith(text)]
        if state < len(matches):
            return matches[state]
        else:
            return None

class CliListEditor(OptionTypeEditor):
    def __init__(self, option, config):
        super(CliListEditor, self).__init__(option, config)
        self.options = self.option.option_type.options()
        
    def edit(self):
        value, origin = self.config.option_value(self.option)
        return self.edit_list(value)

    def edit_list(self, items):
        if items is None:
            items = []
        index = 0
        for item in items:
            print '[' + str(index) + '] ' + str(item)
            index = index + 1
        print '[c] Clear list | [+] Add item | [-] Remove item | [d] Done'
        print
        command = raw_input('Command: ')
        if command == 'c' or command == 'clear':
            return self.edit_list([])
        elif command == '+':
            choose_from = [item for item in self.options if item not in items]
            index = 0
            for option in choose_from:
                print '[' + str(index) + '] ' + str(option)
                index = index + 1
            selection = raw_input('Select item to add: ')
            try:
                selected_option = int(selection)
            except ValueError:
                selected_option = None
        
            if selected_option is not None:
                if selected_option >= len(choose_from):
                    print 'Option ' + str(selected_option) + ' not found'
                    print
                    return self.edit_list(items)
                else:
                    items.append(choose_from[selected_option])
                    return self.edit_list(items)
        elif command == '-':
            selection = raw_input('Item to remove: ')
            try:
                selected_item = int(selection)
            except ValueError:
                selected_item = None
            if selected_item is not None:
                if selected_item >= len(items):
                    print 'Item ' + str(selected_item) + ' not found'
                    print
                    return self.edit_list(items)
                else:
                    del(items[selected_item])
                    return self.edit_list(items)
        elif command == 'd' or command == 'done':
            return items
        else:
            print 'Invalid command'
            return self.edit_list(items)

class CliManyEditor(OptionTypeEditor):
    def edit(self):
        value, origin = self.config.option_value(self.option)
        return self.edit_many(value)

    def edit_many(self, items):
        if items is None:
            items = []
        index = 0
        for item in items:
            print '[' + str(index) + '] ' + str(item)
            index = index + 1
        print '[c] Clear list | [+] Add item | [-] Remove item | [d] Done'
        print
        command = raw_input('Command: ')
        if command == 'c' or command == 'clear':
            return self.edit_many([])
        elif command == '+':
            new_item = self.create_item()
            items.append(new_item)
            return self.edit_many(items)
        elif command == '-':
            selection = raw_input('Item to remove: ')
            try:
                selected_item = int(selection)
            except ValueError:
                selected_item = None
            if selected_item is not None:
                if selected_item >= len(items):
                    print 'Item ' + str(selected_item) + ' not found'
                    print
                    return self.edit_many(items)
                else:
                    del(items[selected_item])
                    return self.edit_many(items)
        elif command == 'd' or command == 'done':
            return items
        else:
            print 'Invalid command'
            return self.edit_many(items)

    def create_item(self):
        editor_class_name = 'Cli' + self.option.option_type.option_type.name + 'Editor'
        editor_class = globals()[editor_class_name]
        editor = editor_class(self.option.option_type.option_type, self.config)
        return editor.edit()
