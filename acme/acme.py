#!/usr/bin/env python

from util import *
import configuration as conf
import os
import sys
import test
import argparse
import logging
import datetime
import json
import frontend.tk.tk as tk
import frontend.cli.cli as cli

class AcmeJSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%d/%m/%Y %H:%M:%S")
            
        return json.JSONEncoder.default(self, obj)

def acme_version():
    here = os.path.abspath(os.path.dirname(__file__))
    with open(os.path.join(here, '..', 'VERSION')) as version_file:
        version = version_file.read().strip()
    return version    

def read_schemas_file(args):
    schemas_file = None
    if args.schemas is not None:
        if os.path.exists(args.schemas):
            schemas_file = args.schemas
        elif os.path.exists(os.getcwd() + '/' + args.schemas):
            schemas_file = os.getcwd() + '/' + args.schemas
        else:
            sys.exit('Schema file ' + args.schemas + ' does not exist')
        
    if schemas_file is None:
        schemas_file = os.getcwd() + '/acme.schema'
        
    return schemas_file

def load_schemas(args):
    schemas_file = read_schemas_file(args)
    schemas = []
    if  os.path.exists(schemas_file):
        unserializer = conf.ConfigurationSchemasXMLUnserializer()
        schemas = unserializer.read(schemas_file)

    return schemas

def read_configs_file(args):
    configs_file = None
    if args.configs is not None:
        if os.path.exists(args.configs):
            configs_file = args.configs
        elif os.path.exists(os.getcwd() + '/' + args.configs):
            configs_file = os.path.exists(os.getcwd() + '/' + args.configs)
        else:
            sys.exit('Configuration file ' + args.configs + ' does not exist')     
    
    if configs_file is None:
        configs_file = os.getcwd() + '/acme.config'

    return configs_file

def load_configs(args):
    load_schemas(args)
    
    configs_file = read_configs_file(args)
    
    # Try loading the configurations
    configs = []
    
    if os.path.exists(configs_file):
        unserializer = conf.ConfigurationsXMLUnserializer()
        configs = unserializer.read(configs_file)

    return configs

def cmd_install(args):
    "Create a new configuration from a schema"
    pass

def cmd_setup(args):
    configs = load_configs(args)
    tk.start_full_manager(configs=configs)
    
def cmd_schema_edit(args):
    load_schemas(args)
    tk.start_schemas_manager()

def cmd_schema_list(args):
    print 'Not implemented'

def cmd_schema_inspect(args):
    print 'Not implemented'

def cmd_schema_delete(args):
    print 'Not implemented'

def cmd_import(args):
    print 'Not implemented'

def cmd_export(args):
    print 'Not implemented'

def cmd_delete(args):
    print 'Not implemented'
    
def cmd_get(args):
    load_configs(args)
    logging.info('Get option at ' + args.get)
    full_option_path = args.get
    split_path = full_option_path.split('.')
    config_name = split_path[0]
    option_path = split_path[1:]

    logging.info('Trying to read option ' + str(option_path) + ' from ' + config_name + ' configuration')
    config = conf.Configuration.get_named(config_name)
    option = config.schema.option_in_path(option_path)
    value, origin = config.option_value(option)
    logging.info(str(option_path) + ' option value: ' + str(value))

    option_value = value
    if option_value is None:
        option_value = option.default_value

    if option_value is None:
        print 'Not set'
    else:
        option_origin = origin
        if option_origin is None:
                option_origin = 'Default'
        if args.json:
            attributes = {'value' : option_value,
                          'type': str(option.option_type),
                          'origin' : str(option_origin)}
            print json.dumps(attributes, cls=AcmeJSONEncoder)
        else:
            print option.unparse_value(option_value)
            print str(option.option_type)
            print str(option_origin)


def cmd_set(args):

    logging.info('Set option: ' + args.set)
    
    configs_file = read_configs_file(args)    
    configs = load_configs(args)

    if args.json:
        list = json.loads(args.set)
        full_option_path = list[0]
        value = list[1]
    else:
        full_option_path, value = args.set.split('=')

    split_path = full_option_path.split('.')
    config_name = split_path[0]
    option_path = split_path[1:]

    logging.info('Trying to set option ' + str(option_path) + ' from ' + config_name + ' configuration')
    config = conf.Configuration.get_named(config_name)
    option = config.schema.option_in_path(option_path)
    parsed_value = option.parse_value(value)

    config.set_option_value(option, parsed_value)

    def serialize_config():
        serializer = conf.ConfigurationsXMLSerializer()
        for config in configs:
            serializer.serialize(config)
        serializer.write(configs_file)
        sys.exit()
    if not args.no_validate:
        errors = config.validate()
        if errors is not None:
            error_msgs = ', '.join(map(lambda e: e.get('message'), errors))
            sys.exit(config.name + " configuration is not valid: " + error_msgs)
        else:
            serialize_config()
    else:
        serialize_config()                                       
    

def cmd_inspect(args):
    configs = load_configs(args)
    config = conf.Configuration.get_named(args.config)
    schema = config.schema
        
    if args.json:
        def inspect_section(section, options):
            for option in section.options():
                value, origin = config.option_value(option)
                
                option_value = value
                if option_value is None:
                    option_value = option.default_value
                    
                option_origin = origin
                if option_origin is None:
                    option_origin = 'Default'
                    
                if option_value is not None:
                    attributes = {'option': option.path_string(),
                                  'value': option_value,
                                  'type': str(option.option_type),
                                  'origin': str(option_origin)}
                    options.append(attributes) 
                for section in section.subsections():
                    inspect_section(section, options)
        options = []
        for section in config.sections():
            inspect_section(section, options)
        print json.dumps(options, cls=AcmeJSONEncoder)
    else:
        def inspect_section(section):
            for option in section.options():
                value, origin = config.option_value(option)
                    
                option_value = value
                if option_value is None:
                    option_value = option.default_value
                        
                option_origin = origin
                if option_origin is None:
                    option_origin = 'Default'
                        
                if option_value is not None:
                    print option.path_string() + ", " + option.unparse_value(option_value) + ", " + str(option.option_type) + ", " + str(option_origin) 
                for section in section.subsections():
                    inspect_section(section)
                        
        for section in schema.sections():
            inspect_section(section)

def cmd_validate(args):
    load_configs(args)
    if args.config is not None:
        # Validate configuration
        config = conf.Configuration.get_named(args.config)
        errors = config.validate()
        
        if errors is not None:
            error_msgs = "\n".join(map(lambda e: e.get('message'), errors))
            sys.exit(config.name + " configuration is not valid: \n\n" + error_msgs)
        else:
            print "The configuration is valid"
    else:
        # Validate all
        for config in conf.Configuration.configurations():
            errors = config.validate()
        
            if errors is not None:
                print config.name + " is invalid: \n"
                for error in errors:
                    print error.get('message')  
                print ""             
            else:
                print config.name + " is valid\n"

def cmd_list(args):
    configs = load_configs(args)
    if args.json:
        print json.dumps(map(lambda c: c.name, configs))
    else:
        for config in configs:
            print config.name

def cmd_edit(args):
    configs = load_configs(args)
    if args.config is not None:
        config = conf.Configuration.get_named(args.config)
        if args.frontend == 'tk' or args.tk:
            tk.start_configuration_manager(config)
        elif args.frontend == 'cli' or args.cli:
            cli.edit_configuration(config)
        else:  # no frontend selected
            tk.start_configuration_manager(config)
    else:
        if args.frontend == 'tk' or args.tk:
            tk.start_configurations_manager(configs)      
        elif args.frontend == 'cli' or args.cli:
            cli.edit_configurations(configs)
        else:
            tk.start_configurations_manager(configs)

def cmd_create(args):
    load_configs(args)
    config = conf.Configuration(args.config, schema=conf.ConfigurationSchema.get_named(args.schema))
    if args.frontend == 'tk' or args.tk:
        tk.start_configuration_manager(config)
    elif args.frontend == 'cli' or args.cli:
        if args.install:
            cli.create_configuration(config)
        else:
            cli.edit_configuration(config)
    else:  # no frontend selected
        tk.start_configuration_manager(config)

def main():
    parser = argparse.ArgumentParser(prog='acme', description='Acme. Application Configuration ManagEr.')
    parser.add_argument('-c', '--configs', help='The configurations file. Default is acme.config')
    parser.add_argument('-s', '--schemas', help='The configuration schemas files. Default is acme.schema')

    parser.add_argument('--version', help='Acme version', action="version", version=acme_version())
    parser.add_argument('--debug', help='Run in debug mode. Provide the debugging level, one of DEBUG or INFO')

    subparsers = parser.add_subparsers(help='command help')

    parser_setup = subparsers.add_parser('setup', help="Edit both schemas and configurations")
    parser_setup.set_defaults(func=cmd_setup)
        
    parser_list = subparsers.add_parser('list', help='List configurations')
    parser_list.add_argument('--json', help="Use JSON for communication", action="store_true")
    parser_list.set_defaults(func=cmd_list)
        
    parser_inspect = subparsers.add_parser('inspect', help='Inspect a configuration. A CSV(Comma separated values) list with <option path>, <value>, <option type>, <origin>')
    parser_inspect.add_argument('config', help='Configuration to validate')
    parser_inspect.add_argument('--json', help="Use JSON for communication", action="store_true")
    parser_inspect.set_defaults(func=cmd_inspect)

    parser_get = subparsers.add_parser('get', help='Get an option value')
    parser_get.add_argument('get', help='Option to get')
    parser_get.add_argument('--json', help="Use JSON for communication", action="store_true")
    parser_get.set_defaults(func=cmd_get)

    parser_set = subparsers.add_parser('set', help='Set an option value')
    parser_set.add_argument('set', help='Set expression')
    parser_set.add_argument('--json', help="Use JSON for communication", action="store_true")
    parser_set.add_argument('--no-validate', help='Avoid configuration validation before setting', action='store_true')
    parser_set.set_defaults(func=cmd_set)
    
    parser_edit = subparsers.add_parser('edit', help="Edit configurations or a specific configuration")
    parser_edit.add_argument('config', nargs='?', help='Configuration to validate')
    parser_edit.add_argument('--frontend', help='Select prefered frontend. One of tk, cli, or dialog')
    parser_edit.add_argument('--tk', help='Use tk frontend', action='store_true')
    parser_edit.add_argument('--cli', help='Use terminal frontend', action='store_true')
    parser_edit.add_argument('--dialog', help='Use dialog frontend', action='store_true')
    parser_edit.set_defaults(func=cmd_edit)

    parser_create = subparsers.add_parser('create', help="Create a configuration")
    parser_create.add_argument('config', help='Name of the configuration to create')
    parser_create.add_argument('schema', help='Schema for the new configuration')
    parser_create.add_argument('--install', help='Create configuration in install mode', action='store_true')
    parser_create.add_argument('--frontend', help='Select prefered frontend. One of tk, cli, or dialog')
    parser_create.add_argument('--tk', help='Use tk frontend', action='store_true')
    parser_create.add_argument('--cli', help='Use terminal frontend', action='store_true')
    parser_create.add_argument('--dialog', help='Use dialog frontend', action='store_true')
    parser_create.set_defaults(func=cmd_create)

    parser_delete = subparsers.add_parser('delete', help='Delete configuration')
    parser_delete.add_argument('config', help='Name of the configuration to delete')
    parser_delete.set_defaults(func=cmd_delete)

    parser_import = subparsers.add_parser('import', help='Import configuration')
    parser_import.add_argument('file', help='The file to import configurations from')
    parser_import.set_defaults(func=cmd_import)

    parser_export = subparsers.add_parser('export', help='Export configuration')
    parser_export.add_argument('config', help='Name of the configuration to export')
    parser_export.add_argument('file', help='The file to export configuration to')
    parser_export.set_defaults(func=cmd_export)

    parser_validate = subparsers.add_parser('validate', help='Validate configurations or a configuration')
    parser_validate.add_argument('config', nargs='?', help='Configuration to validate')
    parser_validate.set_defaults(func=cmd_validate)

    parser_install = subparsers.add_parser('install', help='Trigger install')
    parser_install.set_defaults(func=cmd_install)

    schema_parser = subparsers.add_parser('schema', help='Schemas management')
    schema_subparsers = schema_parser.add_subparsers(help='Schemas management')
    
    schema_parser_edit = schema_subparsers.add_parser('edit', help='Edit configuration schemas')
    schema_parser_edit.set_defaults(func=cmd_schema_edit)

    schema_parser_list = schema_subparsers.add_parser('list', help='List schemas')
    schema_parser_list.set_defaults(func=cmd_schema_list)

    schema_parser_delete = schema_subparsers.add_parser('delete', help='Delete schema')
    schema_parser_delete.set_defaults(func=cmd_schema_delete)

    schema_parser_inspect = schema_subparsers.add_parser('inspect', help='Inspect schema')
    schema_parser_inspect.add_argument('schema', help='Schema to inspect')
    schema_parser_inspect.set_defaults(func=cmd_schema_inspect)    
    
    args = parser.parse_args()

    if args.debug is not None:
        if args.debug == 'INFO':
            logging.basicConfig(level=logging.INFO)
        else:
            logging.basicConfig(level=logging.DEBUG)   
            
    logging.info("Command line args: " + str(args)) 

    args.func(args)    

if __name__ == '__main__':
    main()
