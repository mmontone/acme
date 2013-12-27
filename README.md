#Configurator#

**Configurator** is a configuration management utility. It is written in Python and provides a Tk GUI for configurations manipulation.

##Overview##

The idea is to define configuration schemas and get a proper way of:

- Sharing and versioning your project’s configuration schemas, but not your configurations. That way, you avoid overwriting configurations from different developers. Each developer has his own configurations that need to match the configuration schemas in the project. Whenever a project’s configuration schema changes, each developer is reponsible for updating his configurations to match the new schemas.
- Being able to define configuration schemas from the GUI, with no need for programming in most cases.
- Provide configurations documentation and validation.
- Edit configurations from a GUI.
- Define your own option configurations types and provide validation for them. 

##Running##

Configurator is run invoking configurator command from the command line. By default, it runs in normal mode; that means, it opens a GUI for adding, removing and editing configurations.

A configuration schemas files is required. The schemas file contains a serialization of the configuration schemas in XML format.

By default, Configurator looks for configurator.schema in the current directory. It shows an error if it can not find it. A different file or location can be specified through the –schemas SCHEMAS option. If the schemas file is found, then it is parsed and loaded. 

Apart from that, Configurator maintains configurations in another file, which by default is configurator.config. That file contains the configurations serialized in XML format. It can be specified to be something else WITH the -–configs CONFIGS option. 

Configurator can be run in three different modes fundamentally.

- **Normal mode**: this mode is invoked running configurator with no special arguments from the command line (apart from the schema and configs arguments). In this mode, the standard configuration navigation UI is opened. This UI is meant for end users. The user can create, remove and edit his configurations from here. He doesn’t need to know about how to build a configuration schema (although that is not difficult at all, as we will see.) Apart from that, when editing the configuration, the user gets a (hopefully) decent UI with custom option editors depending on the type of options and validation.

    This is an example of Configurator running in normal mode: 

![configs](https://raw.github.com/mmontone/configurator/master/doc/images/configurator1.png)

- **Setup mode**: this mode is invoked running configurator with the --setup option from the command line. In this mode, the configuration schemas navigator UI is opened. The developer can create, remove and edit configuration schemas from here. Configuration schemas are descriptions of how configurations should be, with nested sections and different type of options. He can build the application specific configuration schemas from here.

    This is an example of Configurator running in setup mode: 

![schemas](https://raw.github.com/mmontone/configurator/master/doc/images/schemas1.png)

- **Full mode:** this mode is invoked running configurator with the –full option from the command line. In this mode, both the configurations navigator and the configurations schemas navigator are available in two different tabs.

   This is an example of Configurator running in full mode: 

![full](https://raw.github.com/mmontone/configurator/master/doc/images/full.png)

###Command line summary:###

    $> configurator -h

    usage: configurator.py [-h] [-f] [-s SCHEMAS] [-c CONFIGS] [--setup] [--debug]

    Configurator. Configuration management utility.

    optional arguments:
       -h, --help            show this help message and exit
       -f, --full            Run the full configurator (both configurations and
                             schemas navigation)
       -s SCHEMAS, --schemas SCHEMAS
                             The configuration schemas files. Default is
                             configurator.schema
       -c CONFIGS, --configs CONFIGS
                             The configurations file. Default is
                             configurator.config
       --setup               Edit configuration schemas
       --debug               Run in debug mode
