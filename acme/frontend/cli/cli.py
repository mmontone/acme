import configuration as conf
import os
from util import *
from colorama import Fore, Back, Style
import sys

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


def create_configuration(config):
    for section in config.schema.sections():
        print Fore.WHITE + Back.BLUE +  section.name + ' options:'
        #print Fore.BLUE + 'INFO: ' +  section.documentation
        for option in section.options():
            option_value, option_origin = config.option_value(option)

            if option_value is None:
                option_value = option.default_value
            if option_value is None:
                print 'Empty'
            else:
                if option_origin is None:
                    option_origin = 'Default'
            
            print option.name + ' (' + str(option_origin) + '): ' + str(option_value)
            raw_input('Value:')
            print

def print_section(config, section):
    print Fore.WHITE + Back.BLUE +  section.name + ' options:'
    for option in section.options():
        option_value, option_origin = config.option_value(option)

        if option_value is None:
            option_value = option.default_value
        if option_value is None:
            print 'Empty'
        else:
            if option_origin is None:
                option_origin = 'Default'
            
        print option.name + ' (' + str(option_origin) + '): ' + str(option_value)

def print_config(config):
    print config.name + ' configuration'
    print
    if config.documentation is not None:
        print config.documentation
        print
    for section in config.schema.sections():
        print_section(config, section)
        print

def select_section(config):
    sections = config.schema.sections()
    index = 0
    for section in sections:
        print '[' + str(index) + '] ' + section.name
        index = index + 1
    print '[p] Print | [h] Help | [q] Quit'
    print
    command = raw_input('Command: ')
    try:
        selected_section = int(command)
    except ValueError:
        selected_section = None

    if selected_section is not None:
        if selected_section > len(sections):
            print 'Section ' + str(selected_section) + ' not found'
            print
            return select_section(config)
        else:
            select_option(config, sections[selected_section])
            return select_section(config)
    else:
        def print_help():
            print 'Select a section with a number'
            print
        if command == 'p' or command == 'print':
            print_config(config)
            return select_section(config)
        if command == 'h' or command == 'help':
            print_help()
            return select_section(config)
        if command == 'q' or command == 'quit':
            if query_yes_no('Quit without saving?'):
                exit(0)
            else:
                return select_section(config)
        else:
            print 'Invalid command: ' + command
            print
            return select_section(config)

def select_option(config, section):
    print 'Select option:'
    options = section.options()
    index = 0
    for option in options:
        option_value, option_origin = config.option_value(option)

        if option_value is None:
            option_value = option.default_value
        if option_value is None:
            print 'Not set'
        else:
            if option_origin is None:
                option_origin = 'Default'
            
        print '[' + str(index) + '] ' + option.name + ' (' + str(option_origin) + '): ' + str(option_value)
        index = index + 1
    print '[l] Go back | [h] Help | [q] Quit'
    print
    command = raw_input('Command: ')
    try:
        selected_option = int(command)
    except ValueError:
        selected_option = None

    if selected_option is not None:
        if selected_option > len(options):
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
                exit(0)
            else:
                return select_option(config, section)
        elif command == 'l':
            return
        else:
            print 'Invalid command: ' + command
            print
            return select_option(config, section)
        
def edit_configuration(config):
    select_section(config)            

def edit_option(config, option):
    print 'Edit option: ' + str(option.path_string())
    
    option_value, option_origin = config.option_value(option)

    if option_value is None:
        option_value = option.default_value
    if option_value is None:
        print 'Not set'
    else:
        if option_origin is None:
            option_origin = 'Default'
            
    print 'Value: ' + str(option_value) +  ' (' + str(option_origin) + ')'
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
        value_entry = raw_input('New value: ')
        value = option.parse_value(value_entry)
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
