<?php

define('configurator_command', "/usr/local/bin/configurator");

$schemas_file = null;
$configs_file = null;
$configurations = array();

class Configuration {
    var $name = '';
    var $options = array();
    
    function __constructor__($name) {
        $this->name = $name;
        $this->options = array();
    }

    function get($name) {
        return $this->options[$name];
    }

    function set($name, $value) {
        $this->options[$name] = $value;
    }
}

function configurator_setup($schemas, $configs) {
    global $schemas_file;
    global $configs_file;
    
    $schemas_file = $schemas;
    $configs_file = $configs;

    load_configurations();
}

function configurator_setup_example() {
    configurator_setup(__FILE__ . "../../doc/example/configurator.schema",
                       __FILE__ . "../../doc/example/configurator.config");
}

function configurator_list_configs() {
    global $schemas_file;
    global $configs_file;
    
    $output = exec(constant('configurator_command') . ' --schemas ' . $schemas_file .
                    ' --configs ' . $configs_file . ' --list --json');
    return json_decode($output, true);
}

function configurator_inspect($config) {
    global $schemas_file;
    global $configs_file;
    
    $output = exec(constant('configurator_command') . ' --schemas ' . $schemas_file .
                    ' --configs ' . $configs_file . ' -i ' . $config . ' --json');
    return json_decode($output, true);
}

function load_configurations() {
    global $configurations;

    $configurations = array();
    
    foreach (configurator_list_configs() as $config) {
        $configuration = new Configuration($config);

        foreach (configurator_inspect($config) as $option) {
            $configuration->set($option['option'], $option['value']);
        }

        $configurations[$config] = $configuration;        
    }
}

function get_configuration($name) {
    global $configurations;

    return $configurations[$name];

}

?>