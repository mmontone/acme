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

class AcmeJSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%d/%m/%Y %H:%M:%S")
            
        return json.JSONEncoder.default(self, obj)

def main():
    parser = argparse.ArgumentParser(description='Acme. Application Configuration ManagEr.')
    parser.add_argument('-f', '--full', help='Run the full acme (both configurations and schemas navigation)', action='store_true')
    parser.add_argument('-s', '--schemas', help='The configuration schemas files. Default is acme.schema')
    parser.add_argument('-c', '--configs', help='The configurations file. Default is acme.config')
    parser.add_argument('-l', '--list-configs', help='List configurations', action='store_true')
    parser.add_argument('-i', '--inspect-config', help='Inspect a configuration. A CSV(Comma separated values) list with <option path>, <value>, <option type>, <origin>')
    parser.add_argument('-g', '--get', help='Get an option value')
    parser.add_argument('--set', help='Set an option value')
    parser.add_argument('--config', help="Edit a specific configuration")
    parser.add_argument('--validation', help='Enable or disable configurations validation')
    parser.add_argument('--validate', help='Validate a configuration. Pass the configuration name')
    parser.add_argument('--validate-all', help='Validate all configurations', action='store_true')
    parser.add_argument('--json', help="Use JSON for communication", action="store_true")
    parser.add_argument('--setup', help='Edit configuration schemas', action='store_true')
    parser.add_argument('--debug', help='Run in debug mode. Provide the debugging level, one of DEBUG or INFO')
    args = parser.parse_args()
        
    if args.debug is not None:
        if args.debug == 'INFO':
            logging.basicConfig(level=logging.INFO)
        else:
            logging.basicConfig(level=logging.DEBUG)
            
    logging.info("Command line args: " + str(args))
    
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
    
    # Try to load the schemas
    schemas = []
    if  os.path.exists(schemas_file):
        unserializer = conf.ConfigurationSchemasXMLUnserializer()
        schemas = unserializer.read(schemas_file)
    
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
            
    # Try loading the configurations
    configs = []
    
    if os.path.exists(configs_file):
        unserializer = conf.ConfigurationsXMLUnserializer()
        configs = unserializer.read(configs_file)
        
    # List configurations?
    if args.list_configs:
        if args.json:
            print json.dumps(map(lambda c: c.name, configs))
        else:
            for config in configs:
                print config.name
        sys.exit()
        
    # Validate configuration?
    if args.validate is not None:
        config = conf.Configuration.get_named(args.validate)
        errors = config.validate()
        
        if errors is not None:
            error_msgs = "\n".join(map(lambda e: e.get('message'), errors))
            sys.exit(config.name + " configuration is not valid: \n\n" + error_msgs)
        else:
            print "The configuration is valid"
            sys.exit()
            
    # Validate all?
    if args.validate_all:
        for config in conf.Configuration.configurations():
            errors = config.validate()
        
            if errors is not None:
                print config.name + " is invalid: \n"
                for error in errors:
                    print error.get('message')  
                print ""             
            else:
                print config.name + " is valid\n"            
        sys.exit()
        
    # Inspect config?
    if args.inspect_config is not None:
                
        config = conf.Configuration.get_named(args.inspect_config)
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
        sys.exit()
        
    # Process get and set parameters
    if args.get is not None:
        if len(configs) == 0:
            sys.exit('No configurations loaded')
        else:
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
            
            sys.exit()
            
    if args.set is not None:
        if len(configs) == 0:
            sys.exit('No configurations loaded')
        else:
            logging.info('Set option: ' + args.set)
            
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
            if not args.validation in ['False', 'No', 'Off', 'false', 'no', 'off']:
                errors = config.validate()
                if errors is not None:
                    error_msgs = ', '.join(map(lambda e: e.get('message'), errors))
                    sys.exit(config.name + " configuration is not valid: " + error_msgs)
                else:
                    serialize_config()
            else:
                serialize_config()                                       
        
        
    if args.full:
        tk.start_full_manager(configs=configs)
    elif args.setup:
        tk.start_schemas_manager()
    else:
        if len(schemas) == 0:
            print 'Can\'t load configuration schemas'
            print ''
            parser.print_help()
            sys.exit(1)
        else:
            if args.config is not None:
                tk.start_configuration_manager(conf.Configuration.get_named(args.config))
            else:
                tk.start_configurations_manager(configs)

if __name__ == '__main__':
    main()
