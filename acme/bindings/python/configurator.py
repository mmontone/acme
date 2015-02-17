import os
import subprocess
import json
import sys

acme_command = "/usr/local/bin/acme"

schemas_file = None
configs_file = None
configurations = {}

def acme_setup(schemas, configs):
    global schemas_file
    global configs_file
    schemas_file = schemas
    configs_file = configs
    
    load_configurations()
    
def acme_setup_example():
    acme_setup(os.getcwd() + "/../../doc/example/acme.schema",
                       os.getcwd() + "/../../doc/example/acme.config")
    
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
    
    for config in acme_list_configs():
        data = acme_inspect(config)
        configuration = Configuration(config)
        for option in data:
            configuration.set(option.get('option'),
                              parse_option_value(option))
        configurations[config] = configuration

def parse_option_value(option):
    return option.get('value')

def get_configuration(name):
    return configurations.get(name)

def acme_list_configs():
    output = subprocess.check_output([acme_command, '--schemas', schemas_file, '--configs', configs_file, '-list', '--json'])
    
    return json.loads(output)

def acme_inspect(config):
    output = subprocess.check_output([acme_command, '--schemas', schemas_file, '--configs', configs_file, '-i', config, '--json'])
    
    return json.loads(output)


if __name__ == '__main__':
    
    acme_setup_example()
    
    configuration = get_configuration("Dev")
    
    print "Dev.Web server.Host=" + configuration.get("Web server.Host")
