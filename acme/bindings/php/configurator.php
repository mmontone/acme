<?php

define('acme_command', "/usr/local/bin/acme");

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

function acme_setup($schemas, $configs) {
    global $schemas_file;
    global $configs_file;
    
    $schemas_file = $schemas;
    $configs_file = $configs;

    load_configurations();
}

function acme_setup_example() {
    acme_setup(__FILE__ . "../../doc/example/acme.schema",
                       __FILE__ . "../../doc/example/acme.config");
}

function acme_list_configs() {
    global $schemas_file;
    global $configs_file;
    
    $output = exec(constant('acme_command') . ' --schemas ' . $schemas_file .
                    ' --configs ' . $configs_file . ' --list --json');
    return json_decode($output, true);
}

function acme_inspect($config) {
    global $schemas_file;
    global $configs_file;
    
    $output = exec(constant('acme_command') . ' --schemas ' . $schemas_file .
                    ' --configs ' . $configs_file . ' -i ' . $config . ' --json');
    return json_decode($output, true);
}

function load_configurations() {
    global $configurations;

    $configurations = array();
    
    foreach (acme_list_configs() as $config) {
        $configuration = new Configuration($config);

        foreach (acme_inspect($config) as $option) {
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