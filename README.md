#Configurator#

**Configurator** is a configuration management utility. It is written in Python and provides a Tk GUI for configurations manipulation.

##Overview##

The idea is to define configuration schemas and get a proper way of:

- Sharing and versioning your project’s configuration schemas, but not your configurations. That way, you avoid overwriting configurations from different developers. Each developer has his own configurations that need to match the configuration schemas in the project. Whenever a project’s configuration schema changes, each developer is reponsible for updating his configurations to match the new schemas.
- Being able to define configuration schemas from the GUI, with no need for programming in most cases.
- Provide configurations documentation and validation.
- Edit configurations from a GUI.
- Define your own option configurations types and provide validation for them.


##Install##

Download Configurator, or clone the git repository. Then run ``sudo sh ./install.sh``. That installs configurator on ``/usr/local/lib/configurator`` directory, and a ``configurator`` binary on ``/usr/local/bin``. After that, you can run configurator typing ``configurator`` at the shell.

##Quickstart##

There are some example configurations and schemas to see what the application is about.

Run ``configurator``, ``configurator --full``, and ``configurator --setup`` from the ``doc/example`` directory.

##Documentation##

[Manual in HTML format](http://mmontone.github.io/configurator/doc/configurator.html)

[Manual in PDF format](http://mmontone.github.io/configurator/doc/configurator.pdf)

##Running##

Configurator is run invoking configurator command from the command line. By default, it runs in normal mode; that means, it opens a GUI for adding, removing and editing configurations.

A configuration schemas files is required. The schemas file contains a serialization of the configuration schemas in XML format.

By default, Configurator looks for configurator.schema in the current directory. It shows an error if it can not find it. A different file or location can be specified through the –schemas SCHEMAS option. If the schemas file is found, then it is parsed and loaded. 

Apart from that, Configurator maintains configurations in another file, which by default is configurator.config. That file contains the configurations serialized in XML format. It can be specified to be something else WITH the -–configs CONFIGS option. 

Configurator can be run in three different modes fundamentally.

- **Normal mode**: this mode is invoked running configurator with no special arguments from the command line (apart from the schema and configs arguments). In this mode, the standard configuration navigation UI is opened. This UI is meant for end users. The user can create, remove and edit his configurations from here. He doesn’t need to know about how to build a configuration schema (although that is not difficult at all, as we will see.) Apart from that, when editing the configuration, the user gets a (hopefully) decent UI with custom option editors depending on the type of options and validation.

    This is an example of Configurator running in normal mode: 

![configs](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/configurator1.png)

- **Setup mode**: this mode is invoked running configurator with the --setup option from the command line. In this mode, the configuration schemas navigator UI is opened. The developer can create, remove and edit configuration schemas from here. Configuration schemas are descriptions of how configurations should be, with nested sections and different type of options. He can build the application specific configuration schemas from here.

    This is an example of Configurator running in setup mode: 

![schemas](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/schemas1.png)

- **Full mode:** this mode is invoked running configurator with the –full option from the command line. In this mode, both the configurations navigator and the configurations schemas navigator are available in two different tabs.

   This is an example of Configurator running in full mode: 

![full](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/full.png)

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

## Configuration schemas ##

### Schemas ###

Configuration schemas define the configurations structure. They have a name, a list of parents, and a list of sections with options definitions.

![newschema](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/newschema.png)

Configuration schemas can be composed by inheriting from multiple parents. Configuration sections from the parents appear in the child configuration schema. For instance, a full stack web framework configuration schema could inherit from a generic Web schema for web server configuration, and another Database schema for database connection configuration.

Configuration schemas have sections, each containing other sections and schema options. The schemas sections and options can be manipulated in the tree appearing on the left of the configuration schemas navigator.

![schemas_navigation](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/schemas_navigation.png)

### Sections ###

Each configuration schema section has a name, a documentation, subsections and a list of option schemas. Sections, subsections, and sections' options can all be added and removed from the tree widget on the left of the schemas navigator.

### Options ###

Options in schemas have a name and a type. 

![schema_option](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/schema_option.png)

The type of option determines the possible values that can be assigned to it in configurations. The option type can be String, Number, Boolean, Email, URI, Filename, Directory, Color, Timezone, Language, Country, Currency, Date, Time, Datetime, etc. Some of them will be described later. When editing configurations, each option is edited with its corresponding editor, depending on the type of option. For instance, options of type Date are edited using a calendar.

![option_editing](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/option_editing.png)

Apart from name and type, schema options specify if it required for the option to be assigned a value in the configuration. Also, they can have a default value in case the user doesn't assign one in the configuration. 

They also have a documentation string. This is very important for the end user to know the option meaning.

#### Options dependencies ####

It often happens that some options only make sense when some others are set to certain values. For instance, if we are configuring a database, then specific database engine options should only be available when the the specific engine is selected. Or a list of options make sense only when a boolean option is enabled.

Configurator has support for that by attaching a ``dependencies expression`` to an option schema. The dependency expression is written in a very simple domain specific language. They are basically boolean expressions, with options being referenced. 

Example: MySQL supports two storage engines, MyISAM and InnoDB, but this option only makes sense for MySQL. To specify that, we can use the following dependencies expression: ``Database.Engine = 'MySQL'``. This way the ``Storage engine`` option will only be editable when the Database.Engine is set to 'MySQL'.

Dependency expressions can be added in the schema option editing screen:

![dependencies](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/dependencies.png)

## Configurations ##

Configurations are instances of Configuration schemas, much like objects are instances of classes in object oriented programming languages. That is, configurations structure is determined by the configuration schema they belong to. In particular, their sections are that of their configuration schemas; and their options values depend on the option schemas defined in the configruration schemas.

Configurations belong to one and only one configuration schema, and that's compulsory, as it can be expected. Besides, configurations can inherit from each other: a configuration can inherit from another configuration and overwrite its parent options values. A configuration can have one and only one parent, o no parent; this is different from configuration schemas, that can have several parents. An understandable restriction is that the configuration parent schema has to be the same that the configuration schema.

Configurations can be added and removed from the list appering on the left of the configurations navigator.

![configs](https://raw.github.com/mmontone/configurator/master/doc/user-manual/images/configurator1.png)

Configurations can be loaded and saved. They are serialized in XML format. The default filename is ``configurator.config``, but it can be changed if desired.

Configuration options editing happens on the right panel of the configurations navigator. A specific option editor is offered for each type of option, and each option documentation is displayed too. When trying to save a configuration section, it is ensured that required options (options declared with ``required`` enabled in the configuration schema) are filled. Options that are not currently set have their default value, if any. Also, options can be ``set`` and ``unset``. Setting a configuration option means setting the configuration option in the current configuration to the value being shown in the option editor. Unsetting a configuration option means removing the option value setting from the current configuration.
