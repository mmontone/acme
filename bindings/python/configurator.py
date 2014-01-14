import os
import subprocess
import json
import sys

configurator_command = "/usr/local/bin/configurator"

schemas_file = None
configs_file = None
configurations = {}

def configurator_setup(schemas, configs):
    global schemas_file
    global configs_file
    schemas_file = schemas
    configs_file = configs
    
    load_configurations()
    
def configurator_setup_example():
    configurator_setup(os.getcwd() + "/../../doc/example/configurator.schema",
                       os.getcwd() + "/../../doc/example/configurator.config")
    
class Configuration():
    def __init__(self, name):
        self._name = name
        self._options = {}
        
    def get(self, name):
        return self._options.get(name)
    
    def set(self, name, value):
        self._options[name] = value   
    
def load_configurations():
    global configurations
    
    configurations = {}
    
    for config in configurator_list_configs():
        data = configurator_inspect(config)
        configuration = Configuration(config)
        for option in data:
            configuration.set(option.get('option'),
                              parse_option_value(option))
        configurations[config] = configuration

def parse_option_value(option):
    return option.get('value')

def get_configuration(name):
    return configurations.get(name)

def configurator_list_configs():
    output = subprocess.check_output([configurator_command, '--schemas', schemas_file, '--configs', configs_file, '-list', '--json'])
    
    return json.loads(output)

def configurator_inspect(config):
    output = subprocess.check_output([configurator_command, '--schemas', schemas_file, '--configs', configs_file, '-i', config, '--json'])
    
    return json.loads(output)


if __name__ == '__main__':
    
    configurator_setup_example()
    
    configuration = get_configuration("Dev")
    
    print "Dev.Web server.Host=" + configuration.get("Web server.Host")